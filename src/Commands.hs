{-# LANGUAGE OverloadedStrings #-}

module Commands where

import Discord.Types
import Data.Text (Text ())
import Options.Applicative

newtype Comm = ButtonComm ButtonComm

rootComm :: ParserInfo Comm
rootComm =
  info
    (rootSubComm <**> helper)
    ( fullDesc
        <> progDesc "Pandabot"
        <> header "A bot for pandas! (duh)"
    )

rootSubComm :: Parser Comm
rootSubComm =
  hsubparser
    ( command "button" (info (ButtonComm <$> buttonSubComm) (progDesc "Manage role buttons"))
    )

data ButtonComm = AddButton ChannelId Text RoleId Text

addOptions :: Parser ButtonComm
addOptions = AddButton
  <$> argument auto (metavar "CHANNEL_ID")
  <*> argument str (metavar "EMOJI_NAME")
  <*> argument auto (metavar "ROLE_ID")
  <*> argument str (metavar "MESSAGE_CONTENT")

buttonSubComm :: Parser ButtonComm
buttonSubComm =
  hsubparser
    ( command "add" (info addOptions (progDesc "Add a button"))
    )
