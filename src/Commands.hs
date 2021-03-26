module Commands
  ( Comm (..)
  , ButtonComm (..)
  , NotifPointsComm (..)
  , LeaderboardComm (..)
  , rootComm
  ) where

import           Data.Text           (Text)
import           Discord.Types       (MessageId)
import           Options.Applicative (Parser, ParserInfo, argument, auto,
                                      command, fullDesc, header, helper,
                                      hsubparser, info, metavar, progDesc, str,
                                      (<**>))

-- | The overall command tree.
data Comm
  = ButtonComm ButtonComm
  | NotifPointsComm NotifPointsComm
  | LeaderboardComm LeaderboardComm

-- | The root command parser. Pass @True@ if the invoking user has admin perms.
rootComm :: Bool -> ParserInfo Comm
rootComm admin =
  info
    (rootSubComm admin <**> helper)
    ( fullDesc
        <> progDesc "Pandabot"
        <> header "A bot for pandas! (duh)"
    )

rootSubComm :: Bool -> Parser Comm
rootSubComm admin =
  hsubparser
    ( (if admin then command "button" (info (ButtonComm <$> buttonSubComm) (progDesc "Manage role buttons")) else mempty)
   <> command "points" (info (NotifPointsComm <$> notifPointsSubComm) (progDesc "How many do you have?"))
   <> command "leaderboard" (info (LeaderboardComm <$> leaderboardSubComm) (progDesc "Who has the most points?"))
    )

-- | The button command tree.
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

-- | The notifpoints command tree,
data NotifPointsComm = ViewSelf

notifPointsSubComm :: Parser NotifPointsComm
notifPointsSubComm = pure ViewSelf

-- | The leaderboard command tree.
data LeaderboardComm = ViewLeaderboard

leaderboardSubComm :: Parser LeaderboardComm
leaderboardSubComm = pure ViewLeaderboard
