{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
module Pandabot.Bot.Schema where

import           Pandabot.Bot.Orphans           ()

import           Calamity
import           Calamity.Commands
import           CalamityCommands.ParameterInfo
import           Control.Applicative
import           Data.Aeson
import           Data.Maybe
import           Data.Text                      (Text)
import qualified Data.Text                      as S
import           Data.Time
import           Database.Persist
import           Database.Persist.Sql
import           Database.Persist.TH
import           GHC.Generics                   (Generic)
import           Text.Megaparsec.Char
import           TextShow
import           TextShow.Generic

newtype PersistFieldEnum a = PersistFieldEnum a
  deriving newtype (Eq, Ord, Bounded, Enum)

instance (Enum (PersistFieldEnum a), Bounded (PersistFieldEnum a), Ord (PersistFieldEnum a)) => PersistField (PersistFieldEnum a) where
  toPersistValue = PersistInt64 . fromIntegral . fromEnum
  fromPersistValue (PersistInt64 n) = Right $ toEnum (fromIntegral n)
  fromPersistValue _                = Left "Error parsing enum field" -- TODO better message

instance (Enum (PersistFieldEnum a), Bounded (PersistFieldEnum a), Ord (PersistFieldEnum a)) => PersistFieldSql (PersistFieldEnum a) where
  sqlType _ = SqlInt64

data NameType = TwitchName | DiscordName | MinecraftJavaName | MinecraftBedrockName
  deriving stock (Eq, Ord, Bounded, Enum, Show)
  deriving (PersistField, PersistFieldSql) via (PersistFieldEnum NameType)

parserName :: forall a c r. ParameterParser a c r => Text
parserName =
  let ParameterInfo (fromMaybe "" -> name) type_ _ = parameterInfo @a @c @r
   in name <> ":" <> S.pack (show type_)

instance ParameterParser NameType c r where
  parameterDescription = "type of username"
  parse = parseMP (parserName @NameType)
    (   (TwitchName <$ string "twitch")
    <|> (DiscordName <$ string "discord")
    <|> (MinecraftJavaName <$ string "java")
    <|> (MinecraftBedrockName <$ string "bedrock")
    )

data CommunityMemberStatus = StandardMember | Whitelisted | Banned
  deriving stock (Eq, Ord, Bounded, Enum, Show)
  deriving (PersistField, PersistFieldSql) via (PersistFieldEnum CommunityMemberStatus)

instance ParameterParser CommunityMemberStatus c r where
  parameterDescription = "player status"
  parse = parseMP (parserName @NameType)
    (   (StandardMember <$ string "standard")
    <|> (Whitelisted <$ string "whitelisted")
    <|> (Banned <$ string "banned")
    )

newtype UUID = UUID
  { getUUID :: Text
  }
  deriving (Eq, Ord, Show, Generic)
  deriving newtype (ToJSON, FromJSON, PersistFieldSql, PersistField)
  deriving TextShow via (FromGeneric UUID)

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
  Button
    channel (Snowflake Channel)
    message (Snowflake Message)
    emoji Text
    role (Snowflake Role)
    deriving Show

  MessagePoint
    message (Snowflake Message)
    guild (Snowflake Guild)
    assignedBy (Snowflake User)
    assignedTo (Snowflake User)
    assignedAt UTCTime
    deriving Show

  FreePoint
    guild (Snowflake Guild)
    assignedBy (Snowflake User)
    assignedTo (Snowflake User)
    assignedAt UTCTime
    amount Int
    deriving Show

  CommunityMember
    status CommunityMemberStatus
    createdAt UTCTime

    deriving Eq
    deriving Ord
    deriving Show
    deriving Generic

  MemberName
    memberID CommunityMemberId OnDeleteCascade OnUpdateCascade
    nameType NameType
    name Text
    uuid UUID Maybe

    UniqueNameTypeName nameType name

    deriving Eq
    deriving Ord
    deriving Show
|]
