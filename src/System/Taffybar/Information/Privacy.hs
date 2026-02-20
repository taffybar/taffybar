{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      : System.Taffybar.Information.Privacy
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan A. Malison
-- Stability   : unstable
-- Portability : unportable
--
-- PipeWire-based privacy monitoring for microphone, camera, and screen sharing.
--
-- This module uses @pw-dump@ to detect active audio/video streams.
module System.Taffybar.Information.Privacy
  ( -- * Data types
    PrivacyInfo (..),
    PrivacyNode (..),
    NodeType (..),

    -- * Query functions
    getPrivacyInfo,

    -- * Channel-based monitoring
    getPrivacyInfoChan,
    getPrivacyInfoState,

    -- * Configuration
    PrivacyConfig (..),
    defaultPrivacyConfig,
  )
where

import Control.Concurrent.MVar
import Control.Concurrent.STM.TChan
import Control.Exception (SomeException, catch)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.STM (atomically)
import Data.Aeson
  ( FromJSON (..),
    withObject,
    (.:),
    (.:?),
  )
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Default (Default (..))
import Data.List (nubBy)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)
import System.Log.Logger (Priority (..))
import System.Taffybar.Context (TaffyIO, getStateDefault)
import System.Taffybar.Information.Wakeup (taffyForeverWithDelay)
import System.Taffybar.Util (logPrintF, runCommand)

-- | Type of privacy-relevant node.
data NodeType
  = -- | Microphone / audio capture
    AudioInput
  | -- | Audio playback (less privacy-sensitive, but useful)
    AudioOutput
  | -- | Camera / screen capture
    VideoInput
  deriving (Eq, Show, Generic, Ord)

-- | Information about an active privacy-relevant node.
data PrivacyNode = PrivacyNode
  { nodeType :: NodeType,
    appName :: Text,
    appIcon :: Maybe Text,
    nodeName :: Text,
    isMonitor :: Bool
  }
  deriving (Eq, Show, Generic)

-- | Aggregated privacy information.
newtype PrivacyInfo = PrivacyInfo
  { activeNodes :: [PrivacyNode]
  }
  deriving (Eq, Show, Generic)

-- | Configuration for the privacy monitor.
data PrivacyConfig = PrivacyConfig
  { -- | Polling interval in seconds
    privacyPollingInterval :: Double,
    -- | Path to pw-dump command
    privacyPwDumpPath :: FilePath,
    -- | Whether to ignore monitor streams
    privacyIgnoreMonitors :: Bool,
    -- | Whether to ignore audio output streams
    privacyIgnoreAudioOutput :: Bool
  }
  deriving (Eq, Show, Generic)

-- | Default privacy configuration.
defaultPrivacyConfig :: PrivacyConfig
defaultPrivacyConfig =
  PrivacyConfig
    { privacyPollingInterval = 2.0,
      privacyPwDumpPath = "pw-dump",
      privacyIgnoreMonitors = True,
      privacyIgnoreAudioOutput = True
    }

instance Default PrivacyConfig where
  def = defaultPrivacyConfig

privacyLogPath :: String
privacyLogPath = "System.Taffybar.Information.Privacy"

privacyLogF :: (Show t) => Priority -> String -> t -> IO ()
privacyLogF = logPrintF privacyLogPath

-- | Internal representation of a PipeWire object from pw-dump.
data PwObject = PwObject
  { pwId :: Int,
    pwType :: Text,
    pwInfo :: Maybe PwInfo
  }
  deriving (Eq, Show, Generic)

data PwInfo = PwInfo
  { pwState :: Maybe Text,
    pwProps :: Maybe PwProps
  }
  deriving (Eq, Show, Generic)

data PwProps = PwProps
  { propMediaClass :: Maybe Text,
    propMediaName :: Maybe Text,
    propNodeName :: Maybe Text,
    propAppName :: Maybe Text,
    propAppIconName :: Maybe Text,
    propPortalAppId :: Maybe Text,
    propStreamMonitor :: Maybe Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON PwObject where
  parseJSON = withObject "PwObject" $ \v ->
    PwObject
      <$> v .: "id"
      <*> v .: "type"
      <*> v .:? "info"

instance FromJSON PwInfo where
  parseJSON = withObject "PwInfo" $ \v ->
    PwInfo
      <$> v .:? "state"
      <*> v .:? "props"

instance FromJSON PwProps where
  parseJSON = withObject "PwProps" $ \v ->
    PwProps
      <$> v .:? "media.class"
      <*> v .:? "media.name"
      <*> v .:? "node.name"
      <*> v .:? "application.name"
      <*> v .:? "application.icon-name"
      <*> v .:? "pipewire.access.portal.app_id"
      <*> v .:? "stream.monitor"

-- | Get current privacy information by running pw-dump.
getPrivacyInfo :: PrivacyConfig -> IO PrivacyInfo
getPrivacyInfo config = do
  result <- runCommand (privacyPwDumpPath config) []
  case result of
    Left err -> do
      privacyLogF WARNING "pw-dump failed: %s" err
      return $ PrivacyInfo []
    Right output -> do
      let parsed = Aeson.decode (BL.fromStrict $ TE.encodeUtf8 $ T.pack output) :: Maybe [PwObject]
      case parsed of
        Nothing -> do
          privacyLogF WARNING "Failed to parse pw-dump output" ("" :: String)
          return $ PrivacyInfo []
        Just objects -> do
          let nodes = mapMaybe (toPrivacyNode config) objects
              filtered = filterNodes config nodes
              -- Remove duplicates based on app name and node type
              unique = nubBy (\a b -> appName a == appName b && nodeType a == nodeType b) filtered
          return $ PrivacyInfo unique

-- | Convert a PipeWire object to a PrivacyNode if relevant.
toPrivacyNode :: PrivacyConfig -> PwObject -> Maybe PrivacyNode
toPrivacyNode _config obj = do
  -- Only process Node type objects
  if pwType obj /= "PipeWire:Interface:Node"
    then Nothing
    else do
      info <- pwInfo obj
      props <- pwProps info
      mediaClass <- propMediaClass props

      -- Check if the node is running
      let state = pwState info
          isRunning = state == Just "running"

      if not isRunning
        then Nothing
        else do
          -- Determine node type from media.class
          nType <- classToNodeType mediaClass

          -- Get application name (try multiple sources)
          let name =
                fromMaybe "Unknown" $
                  propAppName props
                    <|> propPortalAppId props
                    <|> propNodeName props
                    <|> propMediaName props

              -- Get icon name
              icon =
                propAppIconName props
                  <|> propPortalAppId props
                  <|> propAppName props

              -- Check if it's a monitor stream
              monitor = propStreamMonitor props == Just "true"

              nName = fromMaybe "" $ propNodeName props

          Just
            PrivacyNode
              { nodeType = nType,
                appName = name,
                appIcon = icon,
                nodeName = nName,
                isMonitor = monitor
              }
  where
    (<|>) :: Maybe a -> Maybe a -> Maybe a
    (<|>) ma mb = case ma of
      Nothing -> mb
      just -> just

-- | Map media.class to NodeType.
classToNodeType :: Text -> Maybe NodeType
classToNodeType cls
  | "Stream/Input/Audio" `T.isInfixOf` cls = Just AudioInput
  | "Audio/Source" `T.isInfixOf` cls = Just AudioInput
  | "Stream/Output/Audio" `T.isInfixOf` cls = Just AudioOutput
  | "Audio/Sink" `T.isInfixOf` cls = Just AudioOutput
  | "Video/Source" `T.isInfixOf` cls = Just VideoInput
  | "Stream/Input/Video" `T.isInfixOf` cls = Just VideoInput
  | otherwise = Nothing

-- | Filter nodes based on configuration.
filterNodes :: PrivacyConfig -> [PrivacyNode] -> [PrivacyNode]
filterNodes config = filter keep
  where
    keep node
      | privacyIgnoreMonitors config && isMonitor node = False
      | privacyIgnoreAudioOutput config && nodeType node == AudioOutput = False
      | otherwise = True

-- | State for the privacy info channel.
newtype PrivacyInfoChanVar
  = PrivacyInfoChanVar
      (TChan PrivacyInfo, MVar PrivacyInfo)

-- | Get a broadcast channel for privacy info updates.
--
-- The first call starts a monitoring thread that polls PipeWire at the
-- configured interval. Subsequent calls return the already created channel.
getPrivacyInfoChan :: PrivacyConfig -> TaffyIO (TChan PrivacyInfo)
getPrivacyInfoChan config = do
  PrivacyInfoChanVar (chan, _) <- getPrivacyInfoChanVar config
  pure chan

-- | Read the current privacy info state.
getPrivacyInfoState :: PrivacyConfig -> TaffyIO PrivacyInfo
getPrivacyInfoState config = do
  PrivacyInfoChanVar (_, var) <- getPrivacyInfoChanVar config
  liftIO $ readMVar var

getPrivacyInfoChanVar :: PrivacyConfig -> TaffyIO PrivacyInfoChanVar
getPrivacyInfoChanVar config =
  getStateDefault $ do
    chan <- liftIO newBroadcastTChanIO
    var <- liftIO $ newMVar (PrivacyInfo [])
    let intervalSeconds :: Double
        intervalSeconds = max 0.1 (privacyPollingInterval config)
    liftIO $ refreshPrivacyInfo config chan var
    void $ taffyForeverWithDelay intervalSeconds (liftIO $ refreshPrivacyInfo config chan var)
    pure $ PrivacyInfoChanVar (chan, var)

refreshPrivacyInfo ::
  PrivacyConfig ->
  TChan PrivacyInfo ->
  MVar PrivacyInfo ->
  IO ()
refreshPrivacyInfo config chan var = do
  info <- catch (getPrivacyInfo config) $ \(e :: SomeException) -> do
    privacyLogF WARNING "Privacy info refresh failed: %s" e
    return $ PrivacyInfo []
  _ <- swapMVar var info
  atomically $ writeTChan chan info
