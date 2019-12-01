module Main where

import           App                   (generate, runApp)
import           System.Console.GetOpt
import           System.Environment

-- | The 'main' function gathers the required environment information and
-- initializes the application.

data Options = Options
  { optJs      :: Bool
  }

defaultOptions :: Options
defaultOptions = Options { optJs = False }

options :: [OptDescr (Options -> IO Options)]
options =
  [ Option "j"
           ["js"]
           (NoArg $ \o -> return o { optJs = True })
           "Write API JS to disk"
  ]
main :: IO ()
main = do
  args <- getArgs
  let (actions, _, _) = getOpt RequireOrder options args
  opts <- foldl (>>=) (return defaultOptions) actions
  let Options { optJs = g } = opts
  case g of
    True -> do
      generate
    _ -> do
      runApp
