module Buttons 
  ( giveRole
  , removeRole
  , buttons
  ) where

import Discord.Requests
import Discord.Types
import qualified Database.Persist as P

import Schema
import Types

giveRole :: RoleId -> UserId -> GuildId -> Handler ()
giveRole role user gid = run $ AddGuildMemberRole gid user role

removeRole :: RoleId -> UserId -> GuildId -> Handler ()
removeRole role user gid = run $ RemoveGuildMemberRole gid user role

buttons :: Handler [Button]
buttons = fmap (map P.entityVal) $ runDB $ P.selectList [] []