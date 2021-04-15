{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module Pandabot.Hotfix
( HotfixCustomReactions (..)
) where

import           Calamity
import           Calamity.HTTP.Internal.Request
import           Calamity.HTTP.Internal.Route
import           Control.Lens
import           TextShow

newtype HotfixCustomReactions a = HF
  { hf :: ChannelRequest a
  }

instance Request (HotfixCustomReactions a) where
  type Result (HotfixCustomReactions a) = a
  route (HF a) = case a of
    CreateReaction (getID -> cid) (getID @Message -> mid) (CustomEmoji pemoj) ->
      baseRoute cid // S "messages" // ID @Message // S "reactions" // S (pemoj ^. #name . strict <> ":" <> pemoj ^. #id . to showt) // S "@me"
        & giveID mid
        & buildRoute
    x -> route x
  action (HF a) = action a

baseRoute :: Snowflake Channel -> RouteBuilder _
baseRoute id' =
  mkRouteBuilder // S "channels" // ID @Channel
    & giveID id'
