{-# LANGUAGE OverloadedStrings #-}
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

-- | Generate regular javacript functions that use
--   the axios library, using default values for 'CommonGeneratorOptions'.
customAxios :: AxiosOptions -> JavaScriptGenerator
customAxios aopts = customAxiosWith aopts defCommonGeneratorOptions

-- | Generate regular javascript functions that use the axios library.
customAxiosWith :: AxiosOptions -> CommonGeneratorOptions -> JavaScriptGenerator
customAxiosWith aopts opts = \reqs ->
  "import axios from 'axios';\n\n"
    <> "class Api {\n"
    <> "  constructor(jwt) { this.jwt = jwt }\n\n"
    <> (mconcat . map (generateCustomAxiosJSWith aopts opts) $ reqs)
    <> "}\n"
    <> "export { Api as default }"
    -- T.intercalate "\n\n" .  map (generateCustomAxiosJSWith aopts opts)

-- | js codegen using axios library
generateCustomAxiosJSWith
  :: AxiosOptions -> CommonGeneratorOptions -> AjaxReq -> Text
generateCustomAxiosJSWith aopts opts req =
  "\n"
    <> fname
    <> "("
    <> argsStr
    <> ") {\n"
    <> "  return axios({ url: "
    <> url
    <> "\n"
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
  argsStr = T.intercalate ", " args
  args =
    captures ++ map (view $ queryArgName . argPath) queryparams ++ body ++ map
      (toValidFunctionName . (<>) "header" . view (headerArg . argPath))
      (filter (\h -> (view (headerArg . argPath) h) /= "Authorization") hs)

  captures =
    map (view argPath . captureArg) . filter isCapture $ req ^. reqUrl . path

  hs          = req ^. reqHeaders

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
    then ""
    else "    , headers: { " <> headersStr <> " }\n"
   where
    headersStr = T.intercalate ", " $ map headerStr hs
    headerStr header =
      let headerName = header ^. headerArg . argPath
      in  let headerValue = case headerName of
                "Authorization" -> "\"Bearer \" + this.jwt"
                _               -> toJSHeader header
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
