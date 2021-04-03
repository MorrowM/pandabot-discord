module Main where

import           Control.Monad.Except (runExceptT)
import qualified Data.Text.IO         as TIO
import           Discord              (RunDiscordOpts (discordOnEvent, discordOnStart, discordToken),
                                       def, runDiscord)
import           Bot                  (eventHandler, onStart)
import           Config               (App (..), Config (..), parseConfigFile)
import           Types                (runHandler)

main :: IO ()
main = do
  mcfg <- runExceptT (parseConfigFile "bot.json")
  case mcfg of
    Left err -> TIO.putStrLn $ "error parsing configuration: " <> err
    Right cfg -> do
      userFacingError <- runDiscord $ def
        { discordOnStart = \dis -> runHandler (App { appDis = dis, appConfig = cfg }) onStart,
          discordToken = botToken cfg,
          discordOnEvent = \dis e -> runHandler (App { appDis = dis, appConfig = cfg }) (eventHandler e)
        }
      TIO.putStrLn userFacingError
