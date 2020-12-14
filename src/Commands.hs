{-# LANGUAGE OverloadedStrings #-}

module Commands 
  ( Comm (..)
  , ButtonComm (..)
  , NotifPointsComm (..)
  , rootComm
  ) where

import Data.Text (Text)
import Discord.Types ( MessageId )
import Options.Applicative
    ( (<**>),
      ParserInfo,
      argument,
      auto,
      command,
      fullDesc,
      header,
      info,
      metavar,
      progDesc,
      str,
      helper,
      hsubparser,
      Parser )

data Comm 
  = ButtonComm ButtonComm
  | NotifPointsComm NotifPointsComm

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
   <> command "notifpoints" (info (NotifPointsComm <$> notifPointsSubComm) (progDesc "See how much you've contributed to the #NotifGang"))
    )

data ButtonComm 
  = AddButton Text Text Text Text
  | InsertButton Text Text Text MessageId
  | RemoveButton Text Text Text MessageId

addOptions :: Parser ButtonComm
addOptions =
  AddButton
    <$> argument str (metavar "CHANNEL")
    <*> argument str (metavar "EMOJI")
    <*> argument str (metavar "ROLE")
    <*> argument str (metavar "MESSAGE_CONTENT")

insertOptions :: Parser ButtonComm
insertOptions =
  InsertButton
    <$> argument str (metavar "CHANNEL")
    <*> argument str (metavar "EMOJI")
    <*> argument str (metavar "ROLE")
    <*> argument auto (metavar "MESSAGE_ID")

buttonSubComm :: Parser ButtonComm
buttonSubComm =
  hsubparser
    ( command "add" (info addOptions (progDesc "Add a button"))
   <> command "insert" (info insertOptions (progDesc "Insert a button into an existing message"))
   <> command "remove" (info removeOptions (progDesc "Remove an existing button from a message"))
    )

removeOptions :: Parser ButtonComm
removeOptions =
  RemoveButton
    <$> argument str (metavar "CHANNEL")
    <*> argument str (metavar "EMOJI")
    <*> argument str (metavar "ROLE")
    <*> argument auto (metavar "MESSAGE_ID")

data NotifPointsComm = ViewSelf

notifPointsSubComm :: Parser NotifPointsComm
notifPointsSubComm = pure ViewSelf