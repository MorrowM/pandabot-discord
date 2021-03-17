{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Snappers where

import Control.Applicative
import Control.Concurrent
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Text ( Text )
import qualified Data.Text as T
import Discord
import Discord.Requests
import Network.Wreq

import Util

data Latest = Latest
  { release :: Text
  , snapshot :: Text
  } deriving (Show, Eq)

instance FromJSON Latest where
  parseJSON (Object v) = 
    Latest
    <$> (v .: "latest" >>= (.: "release"))
    <*> (v .: "latest" >>= (.: "snapshot"))
  parseJSON _ = empty

manifestUrl :: String
manifestUrl = "https://launchermeta.mojang.com/mc/game/version_manifest.json"

getLatest :: MonadIO m => m (Maybe Latest)
getLatest = do
  r <- liftIO $ get manifestUrl
  pure $ r ^. responseBody & decode

checkForSnapshots :: DiscordHandle -> IO ()
checkForSnapshots dis = do
  mlatest <- getLatest
  case mlatest of
    Nothing -> logS "There was an error decoding the version manifest!"
    Just latest -> do
      logS $ "The latest versions of Minecraft are: " <> show latest
      loop latest
  where
    loop oldver = do
      mver <- getLatest
      case mver of
        Nothing -> logS "There was an error decoding the version manifest"
        Just ver -> when (ver /= oldver) $
          let (newVer, blogPostStr) = if release ver /= release oldver 
              then (release ver, "java-edition-" <> formatRelease (release ver))
              else (snapshot ver, "snapshot-" <> snapshot ver)
          in void $ restCall dis $ CreateMessage 693537434784366605 $
            "@everyone\nMinecraft version " <> newVer <> " is out!\nA blog post should be available at https://www.minecraft.net/en-us/article/minecraft-" <> blogPostStr
      liftIO $ threadDelay (60 * 10^(6 :: Int))
    formatRelease = T.map $ \case
      '.' -> '-'
      x -> x