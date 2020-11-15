module Buttons where

import Types
import Discord.Requests
import Discord.Types
import Schema
import qualified Database.Persist as P

giveRole :: RoleId -> UserId -> GuildId -> Handler ()
giveRole role user gid = run $ AddGuildMemberRole gid user role

removeRole :: RoleId -> UserId -> GuildId -> Handler ()
removeRole role user gid = run $ RemoveGuildMemberRole gid user role

buttons :: Handler [Button]
buttons = fmap (map P.entityVal) $ runDB $ P.selectList [] []