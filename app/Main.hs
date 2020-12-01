{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (pack)
import Discord
import qualified Data.Text.IO as TIO
import System.Environment

import Bot
import Types

main :: IO ()
main = do
  tok <- pack <$> getEnv "PANDABOT_TOK"
  userFacingError <-
    runDiscord $
      def
        { discordOnStart = flip runHandler onStart,
          discordToken = tok,
          discordOnEvent = \dis e -> runHandler dis (eventHandler e)
        }
  TIO.putStrLn userFacingError