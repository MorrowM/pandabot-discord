{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Text (Text, pack)
import qualified Data.Text.IO as TIO
import Discord
import qualified Discord.Requests as R
import Discord.Types
import System.Environment

main :: IO ()
main = do
  tok <- pack <$> getEnv "PANDABOT_TOK"
  userFacingError <-
    runDiscord $
      def
        { discordOnStart = const $ TIO.putStrLn "Connected!",
          discordToken = tok,
          discordOnEvent = eventHandler
        }
  TIO.putStrLn userFacingError

eventHandler :: DiscordHandle -> Event -> IO ()
eventHandler dis event = case event of
  GuildMemberAdd gid mem -> do
    let uid = userId $ memberUser mem
    putStrLn $ "User " <> show uid <> " joined guild " <> show gid
    res <- addPandaRole dis uid gid
    case res of
      Left err -> print err
      Right () -> putStrLn $ "Added panda role for user " <> show mem
  TypingStart _ -> pure ()
  other -> putStrLn . head . words . show $ other

addPandaRole :: DiscordHandle -> UserId -> GuildId -> IO (Either RestCallErrorCode ())
addPandaRole dis usr gid = restCall dis $ R.AddGuildMemberRole gid usr 762055744555188234 -- Hardcoded! TODO Change this