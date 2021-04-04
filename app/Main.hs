module Main where

import           Bot                  (eventHandler, onStart)
import           Control.Concurrent
import           Control.Monad.Except (runExceptT)
import qualified Data.Text.IO         as TIO
import           Discord              (RunDiscordOpts (discordOnEvent, discordOnStart, discordToken),
                                       def, runDiscord)
import           Types

main :: IO ()
main = do
  mcfg <- runExceptT (parseConfigFile "bot.json")
  cache' <- newMVar =<< fetchCache
  case mcfg of
    Left err -> TIO.putStrLn $ "error parsing configuration: " <> err
    Right cfg -> do
      userFacingError <- runDiscord $ def
        { discordOnStart = \dis -> runHandler (App { disHandle = dis, config = cfg, cache = cache' }) onStart,
          discordToken = botToken cfg,
          discordOnEvent = \dis e -> runHandler (App { disHandle = dis, config = cfg, cache = cache' }) (eventHandler e)
        }
      TIO.putStrLn userFacingError
