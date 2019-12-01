{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module CustomAxios where

-- import           Prelude ()
-- import           Prelude.Compat

import           Control.Lens
import           Data.Maybe          (isJust)
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Data.Text.Encoding  (decodeUtf8)
import           Servant.Foreign
import           Servant.JS.Axios    (AxiosOptions, withCredentials,
                                      xsrfCookieName, xsrfHeaderName)
import           Servant.JS.Internal
import Text.RawString.QQ
import           AppContext             (Environment(..))

-- | Generate regular javacript functions that use
--   the axios library, using default values for 'CommonGeneratorOptions'.
customAxios :: Environment -> AxiosOptions -> JavaScriptGenerator
customAxios e aopts = customAxiosWith e aopts defCommonGeneratorOptions

responseInterceptor :: Text
responseInterceptor = [r|axios.interceptors.response.use(
    (response) => {
        return response;
    },
    (error) => {
        console.log('Interception');
        return Promise.reject(error.response);
    },
);

|]

-- | Generate regular javascript functions that use the axios library.
customAxiosWith :: Environment -> AxiosOptions -> CommonGeneratorOptions -> JavaScriptGenerator
customAxiosWith e aopts opts = \reqs ->
  "import axios from 'axios';\n\n"
    <> responseInterceptor
    <> "class Api {\n"
    <> (mconcat . map (generateCustomAxiosJSWith e aopts opts) $ reqs)
    <> "}\n"
    <> "export { Api as default }"

-- | js codegen using axios library
generateCustomAxiosJSWith
  :: Environment -> AxiosOptions -> CommonGeneratorOptions -> AjaxReq -> Text
generateCustomAxiosJSWith e aopts opts req =
  "\n"
    <> fname
    <> "("
    <> argsStr
    <> ") {\n"
    <> "  return axios({ url: "
    <> url
    <> "\n"
    <> baseUrl
    <> "    , method: '"
    <> method
    <> "'\n"
    <> dataBody
    <> reqheaders
    <> withCreds
    <> xsrfCookie
    <> xsrfHeader
    <> "  });\n"
    <> "}\n"
 where
  baseUrl = if e == Travis then "    , baseURL: '/api'\n" else "    , baseURL: 'http://localhost:8888'\n"
  argsStr = T.intercalate ", " args
  args =
    captures
      ++ map (view $ queryArgName . argPath) queryparams
      ++ body
      ++ map
           (toValidFunctionName . (<>) "header" . view (headerArg . argPath))
           hs

  captures =
    map (view argPath . captureArg) . filter isCapture $ req ^. reqUrl . path

  hs = filter (\h -> (view (headerArg . argPath) h) /= "Authorization")
              (req ^. reqHeaders)

  queryparams = req ^.. reqUrl . queryStr . traverse

  body        = if isJust (req ^. reqBody) then [requestBody opts] else []

  dataBody    = if isJust (req ^. reqBody)
    then "    , data: body\n" <> "    , responseType: 'json'\n"
    else ""

  withCreds =
    if withCredentials aopts then "    , withCredentials: true\n" else ""

  xsrfCookie = case xsrfCookieName aopts of
    Just name -> "    , xsrfCookieName: '" <> name <> "'\n"
    Nothing   -> ""

  xsrfHeader = case xsrfHeaderName aopts of
    Just name -> "    , xsrfHeaderName: '" <> name <> "'\n"
    Nothing   -> ""

  reqheaders = if null hs
    then
      if isJust (req ^. reqBody) && (req ^. reqBodyContentType == ReqBodyJSON)
        then "    , headers: {\"Content-Type\":\"application/json\"}\n"
        else ""
    else
      "    , headers: { "
      <> headersStr
      <> ", \"Content-Type\":\"application/json\" }\n"
   where
    headersStr = T.intercalate ", " $ map headerStr hs
    headerStr header =
      let headerName  = header ^. headerArg . argPath
          headerValue = toJSHeader header
      in  "\"" <> headerName <> "\": " <> headerValue

  namespace = if hasNoModule then "" else (moduleName opts) <> "."
    where hasNoModule = moduleName opts == ""

  fname =
    namespace
      <> (toValidFunctionName (functionNameBuilder opts $ req ^. reqFuncName))

  method    = T.toLower . decodeUtf8 $ req ^. reqMethod
  url       = if url' == "'" then "'/'" else url'
  url'      = "'" <> urlPrefix opts <> urlArgs <> queryArgs

  urlArgs   = jsSegments $ req ^.. reqUrl . path . traverse

  queryArgs = if null queryparams then "" else " + '?" <> jsParams queryparams
