module Pandabot.VoiceLog
  ( registerVoiceLogHandler
  ) where

import           Calamity
import           Control.Concurrent
import           Control.Monad
import           Data.Default
import           Data.Foldable
import           Optics
import qualified Polysemy             as P
import qualified Polysemy.AtomicState as P
import qualified Polysemy.Fail        as P
import qualified Polysemy.Reader      as P

import           Pandabot.Bot.Config
import           Pandabot.Bot.Util
import           Pandabot.Points
import qualified Polysemy.Async       as P

registerVoiceLogHandler ::
  ( BotC r
  , P.Members
   '[ P.Fail
    , P.AtomicState MessagePointMessages
    , P.Reader Config
    ] r
  ) => P.Sem r ()
registerVoiceLogHandler = void $ react @'VoiceStateUpdateEvt $ \(mbefore, after') -> do
    roleList <- P.asks @Config . view $ #voiceConfig % #roles
    Just gid <- pure $ after' ^. #guildID
    for_ roleList $ \r -> if
      | (mbefore >>= view #channelID) `notElem` fmap Just (r ^. #voiceChannels)
      , (after' ^. #channelID) `elem` fmap Just (r ^. #voiceChannels) -> do
        void . invoke $ AddGuildMemberRole gid (after' ^. #userID) (r ^. #role)
        whenJust (r ^. #textChannel) $ \chanid -> do
          Just chan <- upgrade chanid
          Just usr <- upgrade (after' ^. #userID)
          Right msg <- invoke $ CreateMessage chan
            (def & #content ?~ "**" <> (usr ^. #username) <> "** joined the channel")
          delayDeleteMessage msg
      | (mbefore >>= view #channelID) `elem` fmap Just (r ^. #voiceChannels)
      , (after' ^. #channelID) `notElem` fmap Just (r ^. #voiceChannels) -> do
        void . invoke $ RemoveGuildMemberRole gid (after' ^. #userID) (r ^. #role)
        whenJust (r ^. #textChannel) $ \chanid -> do
          Just chan <- upgrade chanid
          Just usr <- upgrade (after' ^. #userID)
          Right msg <- invoke $ CreateMessage chan
            (def & #content ?~ "**" <> (usr ^. #username) <> "** left the channel")
          delayDeleteMessage msg
      | otherwise -> pure ()

delayDeleteMessage :: (BotC r, P.Member (P.Reader Config) r) => Message -> P.Sem r ()
delayDeleteMessage msg = void . P.async $ do
  msecs <- P.asks @Config . view $ #voiceConfig % #messageDeleteDelay
  whenJust msecs $ \secs -> do
    P.embed @IO $ threadDelay (secs * 1_000_000)
    void . invoke $ DeleteMessage msg msg

