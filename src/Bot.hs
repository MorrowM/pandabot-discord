{-# LANGUAGE OverloadedStrings #-}

module Bot 
( onStart
, eventHandler
) where

import Control.Monad.IO.Class ( MonadIO(liftIO) )
import Data.List ( isPrefixOf )
import Database.Persist.Sql ( runMigration )
import Discord ( readCache, Cache(_currentUser) )
import Discord.Requests
    ( ChannelRequest(CreateReaction, CreateMessage),
      GuildRequest(AddGuildMemberRole, GetGuildMember) )
import Discord.Types
    ( GuildId,
      UserId,
      Message(..),
      Event(..),
      GuildMember(memberUser),
      Role(roleId),
      User(userName, userId) )
import Options.Applicative
    ( defaultPrefs,
      execParserPure,
      ParserHelp(helpUsage),
      ParserFailure(execFailure),
      ParserResult(Failure, Success) )
import Options.Applicative.Help.Chunk ( Chunk(unChunk) )
import qualified Data.Text as T
import System.Exit ( ExitCode(ExitSuccess) )

import Buttons
    ( runButtonComm,
      buttonHandler,
      ButtonCommError(ChannelIdNameError, RoleIdNameError) )
import Commands ( rootComm, Comm(..) )
import Config ( welcomeRole )
import NotifPoints ( runNotifPointsComm, runLeaderboardComm, handlePointAssign, handlePointRemove )
import Schema ( migrateAll )
import Types
    ( assertTrue,
      catchErr,
      exec,
      run,
      runDB,
      Handler,
      NameError(NameAmbiguous, NameNotFound),
      getConfig,
      getDis )
import Util ( wordsWithQuotes, inGuild, isAdmin, logS )

onStart :: Handler ()
onStart = catchErr $ do
  runDB $ runMigration migrateAll
  dis <- getDis
  cache <- liftIO $ readCache dis
  liftIO $ putStrLn $ "Connected as " <> show (userName $ _currentUser cache)

eventHandler :: Event -> Handler ()
eventHandler event = case event of
  GuildMemberAdd gid mem -> do
    let uid = userId $ memberUser mem
    logS $ "User " <> show uid <> " joined guild " <> show gid
    addPandaRole uid gid
  TypingStart _ -> pure ()
  PresenceUpdate _ -> pure ()
  Ready {} -> pure ()
  GuildCreate {} -> pure ()
  MessageReactionAdd rinfo -> buttonHandler rinfo *> handlePointAssign rinfo
  MessageReactionRemove rinfo -> handlePointRemove rinfo
  MessageCreate msg -> handleMessageCreate msg
  other -> logS . head . words . show $ other

handleMessageCreate :: Message -> Handler ()
handleMessageCreate msg = catchErr $ do
  handleComm msg

handleComm :: Message -> Handler ()
handleComm msg =
  catchErr $ do
    if "!help" `T.isPrefixOf` messageText msg
      then runComm ["-h"] msg
      else
        if "!" `T.isPrefixOf` messageText msg
          then
            let args = map T.unpack $ wordsWithQuotes (T.drop 1 (messageText msg))
             in runComm args msg
          else pure ()

runComm :: [String] -> Message -> Handler ()
runComm args msg = catchErr $ do
  inGuild (messageGuild msg) $ \gid -> do
    mem <- run $ GetGuildMember gid (userId $ messageAuthor msg)
    usrIsAdmin <- isAdmin gid mem
    case execParserPure defaultPrefs (rootComm usrIsAdmin) args of
      Success whichComm -> case whichComm of
        ButtonComm comm -> 
          if usrIsAdmin then do
            res <- runButtonComm comm gid
            case res of
              Right () -> reactPositive
              Left err -> do
                reactNegative
                case err of
                  RoleIdNameError NameNotFound -> reply "Sorry, I couldn't find a role with this name."
                  RoleIdNameError (NameAmbiguous roles) -> reply $ "There are multiple roles with this name: " <> T.pack (show $ roleId <$> roles)
                  ChannelIdNameError NameNotFound -> reply "Sorry, I couldn't find a channel with this name."
                  ChannelIdNameError (NameAmbiguous chans) -> reply $ "There are multiple channels with this name: " <> T.pack (show $ snd <$> chans)
            else do
              reactNegative
              reply "You must be an administrator to run this command."
        NotifPointsComm comm -> do
          res <- runNotifPointsComm comm gid (messageAuthor msg) reply
          case res of
            Right () -> reactPositive
            Left _ -> do
              reactNegative
              reply "Sorry, an unknown error has occured while handling your request"

        LeaderboardComm comm -> do
          res <- runLeaderboardComm comm gid reply
          case res of
            Right () -> reactPositive
            Left _ -> do
              reactNegative
              reply "Sorry, an unknown error has occured while handling your request"
            
      Failure f -> do
        let (hlp, status, _) = execFailure f ""
            helpStr = "```" ++ show hlp ++ "```"
        assertTrue $ 
          not ("Usage:  COMMAND\n" `isPrefixOf` maybe "" show (unChunk $ helpUsage hlp))
          || status == ExitSuccess

        if status == ExitSuccess
          then reactPositive
          else reactNegative

        reply $ T.pack helpStr
      _ -> pure ()
  where
    reply = exec . CreateMessage (messageChannel msg)
    reactPositive = run $ CreateReaction (messageChannel msg, messageId msg) ":white_check_mark:"
    reactNegative = run $ CreateReaction (messageChannel msg, messageId msg) ":x:"


addPandaRole :: UserId -> GuildId -> Handler ()
addPandaRole usr gid = do
  welcome <- welcomeRole <$> getConfig
  run $ AddGuildMemberRole gid usr welcome

