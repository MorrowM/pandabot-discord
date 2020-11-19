{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.Char
import Data.Foldable
import Data.List
import Data.Text (Text, pack)
import Data.Time
import Discord
import Discord.Requests
import Discord.Types
import Options.Applicative
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Database.Persist as P
import qualified Database.Persist.Sql as SQL
import System.Environment
import System.Exit

import Buttons
import Commands
import Schema
import Types

import Options.Applicative.Help.Chunk

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

onStart :: Handler ()
onStart = catchErr $ do
  runDB $ SQL.runMigration migrateAll
  dis <- lift ask
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
  MessageReactionAdd rinfo -> do
    myUid <- myUserId
    assertTrue $ myUid /= reactionUserId rinfo
    case reactionGuildId rinfo of
      Nothing -> pure ()
      Just gid -> do
        logS $ "User " <> show (reactionUserId rinfo) <> " reacted with " <> show (emojiName $ reactionEmoji rinfo) <> " on message " <> show (reactionMessageId rinfo)
        mem <- run $ GetGuildMember gid (reactionUserId rinfo)
        btns <- buttons
        for_ btns $ \button -> do
          let bmsg = toEnum $ buttonMessage button
              bemoji = buttonEmoji button
              bchannel = toEnum $ buttonChannel button
              brole = toEnum $ buttonRole button
          when (bmsg == reactionMessageId rinfo && emojiName (reactionEmoji rinfo) == stripEmoji bemoji) $ do
            if brole `elem` memberRoles mem
              then removeRole brole (reactionUserId rinfo) gid
              else giveRole brole (reactionUserId rinfo) gid
            run $ DeleteUserReaction (bchannel, bmsg) (reactionUserId rinfo) bemoji
            logS $ "User " <> show (reactionUserId rinfo) <> " pressed the button " <> show bemoji <> " on message " <> show bmsg
  MessageCreate msg -> handleMessageCreate msg
  other -> logS . head . words . show $ other

handleMessageCreate :: Message -> Handler ()
handleMessageCreate msg = catchErr $ do
  handleComm msg

handleComm :: Message -> Handler ()
handleComm msg =
  catchErr $
    if "!help" `T.isPrefixOf` messageText msg
      then runComm ["-h"] msg
      else
        if "!" `T.isPrefixOf` messageText msg
          then
            let args = map T.unpack $ wordsWithQuotes (T.drop 1 (messageText msg))
             in runComm args msg
          else pure ()

runComm :: [String] -> Message -> Handler ()
runComm args msg = catchErr $ case execParserPure defaultPrefs rootComm args of
  Success whichComm -> case whichComm of
    ButtonComm comm -> case messageGuild msg of
      Nothing -> pure ()
      Just gid -> do
        mem <- run $ GetGuildMember gid (userId $ messageAuthor msg)
        assertTrue $ 753003004407447644 `elem` memberRoles mem -- hardcoding!
        reactPositive
        runButtonComm comm
  Failure f -> do
    let (hlp, status, _) = execFailure f ""
        helpStr = "```" ++ show hlp ++ "```"
    assertTrue $ 
      not ("Usage:  COMMAND\n" `isPrefixOf` maybe "" show (unChunk $ helpUsage hlp))
      || status == ExitSuccess

    if status == ExitSuccess
      then reactPositive
      else reactNegative

    void $ run $ CreateMessage (messageChannel msg) (T.pack helpStr)
  _ -> pure ()
  where
    reactPositive = run $ CreateReaction (messageChannel msg, messageId msg) ":white_check_mark:"
    reactNegative = run $ CreateReaction (messageChannel msg, messageId msg) ":x:"

runButtonComm :: ButtonComm -> Handler ()
runButtonComm btn = case btn of
  AddButton chan emoji role txt -> do
    msg <- run $ CreateMessage chan txt
    void $ runDB $ P.insert $ Button (fromEnum chan) (fromEnum $ messageId msg) emoji (fromEnum role)
    logS $ "Created button in channel " <> show chan <> " on message " <> show (messageId msg) <> " with emoji " <> show emoji <> " for role " <> show role
    run $ CreateReaction (chan, messageId msg) emoji
  InsertButton chan emoji role mid -> do
    void $ runDB $ P.insert $ Button (fromEnum chan) (fromEnum mid) emoji (fromEnum role)
    logS $ "Inserted button in channel " <> show chan <> " on message " <> show mid <> " with emoji " <> show emoji <> " for role " <> show role
    run $ CreateReaction (chan, mid) emoji

logS :: String -> Handler ()
logS s = liftIO $ do
  t <- getCurrentTime
  let fmt = formatTime defaultTimeLocale "[%F %T] " t
      output = fmt <> s <> "\n"
  putStr output
  appendFile "log.txt" output

addPandaRole :: UserId -> GuildId -> Handler ()
addPandaRole usr gid = run $ AddGuildMemberRole gid usr 762055744555188234 -- Hardcoded! TODO Change this

wordsWithQuotes :: Text -> [Text]
wordsWithQuotes = concat . wordsEveryOther . T.splitOn "\""
  where
    wordsEveryOther :: [Text] -> [[Text]]
    wordsEveryOther [] = []
    wordsEveryOther [z] = [T.words z]
    wordsEveryOther (x : y : xs) = T.words x : [y] : wordsEveryOther xs

myUserId :: Handler UserId
myUserId = do
  dis <- lift ask
  cache <- liftIO $ readCache dis
  pure $ userId $ _currentUser cache

stripEmoji :: Text -> Text
stripEmoji emoji = if T.all isAscii emoji
  then T.takeWhile (/= ':') . T.drop 2 $ emoji
  else emoji