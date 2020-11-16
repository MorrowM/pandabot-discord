{-# LANGUAGE OverloadedStrings #-}

module Commands 
  ( Comm (..)
  , ButtonComm (..)
  , rootComm
  ) where

import Data.Text (Text)
import Discord.Types
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

data ButtonComm 
  = AddButton ChannelId Text RoleId Text
  | InsertButton ChannelId Text RoleId MessageId

addOptions :: Parser ButtonComm
addOptions =
  AddButton
    <$> argument auto (metavar "CHANNEL_ID")
    <*> argument str (metavar "EMOJI_NAME")
    <*> argument auto (metavar "ROLE_ID")
    <*> argument str (metavar "MESSAGE_CONTENT")

insertOptions :: Parser ButtonComm
insertOptions =
  InsertButton
    <$> argument auto (metavar "CHANNEL_ID")
    <*> argument str (metavar "EMOJI_NAME")
    <*> argument auto (metavar "ROLE_ID")
    <*> argument auto (metavar "MESSAGE_ID")

buttonSubComm :: Parser ButtonComm
buttonSubComm =
  hsubparser
    ( command "add" (info addOptions (progDesc "Add a button"))
   <> command "insert" (info insertOptions (progDesc "Insert a button into an existing message"))
    )
