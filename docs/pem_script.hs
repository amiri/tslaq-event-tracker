:set -XOverloadedStrings

import Network.AWS.Easy
import Network.AWS.SecretsManager
import           Data.X509.File

c <- getAWSConfig
secretsSession <- connect c secretsManagerService
getJwtKey "tslaq-jwt-key" secretsSession

