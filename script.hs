:set -XOverloadedStrings
import           Network.AWS
import Network.AWS.SecretsManager
import           Data.Text.Lazy              (fromStrict)
import           Data.Text.Lazy.Encoding     (encodeUtf8)
import           Data.Aeson                  (FromJSON, ToJSON, decode)
c <- getAWSConfig
secretsSession <- connect c secretsManagerService
k <- getPgConnectInfo "pgconnectinfo" secretsSession
decode (encodeUtf8 $ fromStrict $ fromJust k) :: Maybe PGConnectInfo
