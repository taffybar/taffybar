{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      : System.Taffybar.Information.WirePlumber
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan A. Malison
-- Stability   : unstable
-- Portability : unportable
--
-- WirePlumber/PipeWire audio information using the @wpctl@ command-line tool.
--
-- This module provides access to audio volume and mute state for the default
-- audio sink or source via WirePlumber's command-line interface.
module System.Taffybar.Information.WirePlumber
  ( WirePlumberInfo (..),
    NodeType (..),
    getWirePlumberInfo,
    getWirePlumberInfoChan,
    getWirePlumberInfoState,
    getWirePlumberInfoChanFor,
    getWirePlumberInfoStateFor,
    toggleWirePlumberMute,
    adjustWirePlumberVolume,
    setWirePlumberVolume,
  )
where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Control.Concurrent.STM.TChan
import Control.Exception (SomeException, try)
import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.STM (atomically)
import Data.List (isInfixOf)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.TypeLits (KnownSymbol, SomeSymbol (..), Symbol, someSymbolVal, symbolVal)
import System.Log.Logger (Priority (..))
import System.Taffybar.Context (TaffyIO, getStateDefault)
import System.Taffybar.Util (logPrintF, runCommand)
import Text.Read (readMaybe)

-- | Type of audio node.
data NodeType
  = -- | Output device (speakers, headphones)
    Sink
  | -- | Input device (microphone)
    Source
  deriving (Eq, Show)

-- | WirePlumber audio information for a node.
data WirePlumberInfo = WirePlumberInfo
  { -- | Volume level from 0.0 to 1.0 (can exceed 1.0 for amplification)
    wirePlumberVolume :: Double,
    -- | Whether the node is muted
    wirePlumberMuted :: Bool,
    -- | Name of the node (e.g., "@DEFAULT_AUDIO_SINK@")
    wirePlumberNodeName :: Text
  }
  deriving (Eq, Show)

wpLogPath :: String
wpLogPath = "System.Taffybar.Information.WirePlumber"

wpLogF :: (MonadIO m, Show t) => Priority -> String -> t -> m ()
wpLogF = logPrintF wpLogPath

newtype WirePlumberInfoChanVar (a :: Symbol)
  = WirePlumberInfoChanVar (TChan (Maybe WirePlumberInfo), MVar (Maybe WirePlumberInfo))

-- | Get a broadcast channel for WirePlumber info for the provided node spec.
--
-- The first call for a given node spec will start a monitoring thread that
-- polls @wpctl@ periodically and broadcasts updates.
-- Subsequent calls return the already created channel.
getWirePlumberInfoChan :: String -> TaffyIO (TChan (Maybe WirePlumberInfo))
getWirePlumberInfoChan nodeSpec =
  case someSymbolVal nodeSpec of
    SomeSymbol (Proxy :: Proxy sym) -> getWirePlumberInfoChanFor @sym

-- | Read the current WirePlumber info state for the provided node spec.
--
-- See 'getWirePlumberInfoChan' for monitoring behavior.
getWirePlumberInfoState :: String -> TaffyIO (Maybe WirePlumberInfo)
getWirePlumberInfoState nodeSpec =
  case someSymbolVal nodeSpec of
    SomeSymbol (Proxy :: Proxy sym) -> getWirePlumberInfoStateFor @sym

-- | Get a broadcast channel for WirePlumber info for a node spec given as a
-- type-level string.
getWirePlumberInfoChanFor :: forall a. (KnownSymbol a) => TaffyIO (TChan (Maybe WirePlumberInfo))
getWirePlumberInfoChanFor = do
  WirePlumberInfoChanVar (chan, _) <- getWirePlumberInfoChanVarFor @a
  pure chan

-- | Read the current WirePlumber info state for a node spec given as a
-- type-level string.
getWirePlumberInfoStateFor :: forall a. (KnownSymbol a) => TaffyIO (Maybe WirePlumberInfo)
getWirePlumberInfoStateFor = do
  WirePlumberInfoChanVar (_, var) <- getWirePlumberInfoChanVarFor @a
  liftIO $ readMVar var

getWirePlumberInfoChanVarFor :: forall a. (KnownSymbol a) => TaffyIO (WirePlumberInfoChanVar a)
getWirePlumberInfoChanVarFor =
  getStateDefault $ do
    let nodeSpec = symbolVal (Proxy @a)
    liftIO $ do
      chan <- newBroadcastTChanIO
      var <- newMVar Nothing
      _ <- forkIO $ monitorWirePlumberInfo nodeSpec chan var
      pure $ WirePlumberInfoChanVar (chan, var)

-- | Polling interval in microseconds (1 second).
pollIntervalMicros :: Int
pollIntervalMicros = 1_000_000

monitorWirePlumberInfo ::
  String ->
  TChan (Maybe WirePlumberInfo) ->
  MVar (Maybe WirePlumberInfo) ->
  IO ()
monitorWirePlumberInfo nodeSpec chan var = forever $ do
  result <- try $ getWirePlumberInfo nodeSpec
  let info = case result of
        Left (_ :: SomeException) -> Nothing
        Right i -> i
  _ <- swapMVar var info
  atomically $ writeTChan chan info
  threadDelay pollIntervalMicros

-- | Query volume and mute state for the provided node spec.
--
-- The node spec can be:
-- * "@DEFAULT_AUDIO_SINK@" or "" for the default output
-- * "@DEFAULT_AUDIO_SOURCE@" for the default input
-- * A numeric node ID
--
-- Returns 'Nothing' if wpctl is not available or fails.
getWirePlumberInfo :: String -> IO (Maybe WirePlumberInfo)
getWirePlumberInfo nodeSpec = do
  let node = if null nodeSpec then "@DEFAULT_AUDIO_SINK@" else nodeSpec
  result <- runCommand "wpctl" ["get-volume", node]
  case result of
    Left err -> do
      wpLogF WARNING "wpctl get-volume failed: %s" err
      return Nothing
    Right output -> return $ parseWpctlOutput node output

-- | Parse the output of @wpctl get-volume@.
--
-- Expected formats:
-- * "Volume: 0.50"
-- * "Volume: 0.50 [MUTED]"
parseWpctlOutput :: String -> String -> Maybe WirePlumberInfo
parseWpctlOutput nodeSpec output = do
  let trimmed = dropWhile (== ' ') output
  -- Check if it starts with "Volume:"
  volumeStr <-
    if "Volume:" `isInfixOf` trimmed
      then Just $ drop 7 $ dropWhile (/= ':') trimmed
      else Nothing
  -- Parse the volume value and mute state
  let isMuted = "[MUTED]" `isInfixOf` volumeStr
      cleanVolStr = takeWhile (\c -> c /= '[' && c /= '\n') $ dropWhile (== ' ') volumeStr
  volume <- readMaybe cleanVolStr
  return
    WirePlumberInfo
      { wirePlumberVolume = volume,
        wirePlumberMuted = isMuted,
        wirePlumberNodeName = T.pack nodeSpec
      }

-- | Toggle mute for the provided node spec. Returns True on success.
toggleWirePlumberMute :: String -> IO Bool
toggleWirePlumberMute nodeSpec = do
  let node = if null nodeSpec then "@DEFAULT_AUDIO_SINK@" else nodeSpec
  result <- runCommand "wpctl" ["set-mute", node, "toggle"]
  case result of
    Left err -> do
      wpLogF WARNING "wpctl set-mute toggle failed: %s" err
      return False
    Right _ -> return True

-- | Adjust volume by the provided percentage delta. Returns True on success.
--
-- Positive values increase volume, negative values decrease it.
-- The delta is clamped to prevent going below 0%.
adjustWirePlumberVolume :: String -> Int -> IO Bool
adjustWirePlumberVolume nodeSpec deltaPercent = do
  let node = if null nodeSpec then "@DEFAULT_AUDIO_SINK@" else nodeSpec
      sign = if deltaPercent >= 0 then "+" else "-"
      absVal = abs deltaPercent
      arg = show absVal ++ "%" ++ sign
  result <- runCommand "wpctl" ["set-volume", node, arg, "--limit", "1.5"]
  case result of
    Left err -> do
      wpLogF WARNING "wpctl set-volume failed: %s" err
      return False
    Right _ -> return True

-- | Set volume to an absolute percentage value. Returns True on success.
setWirePlumberVolume :: String -> Int -> IO Bool
setWirePlumberVolume nodeSpec percent = do
  let node = if null nodeSpec then "@DEFAULT_AUDIO_SINK@" else nodeSpec
      arg = show (max 0 percent) ++ "%"
  result <- runCommand "wpctl" ["set-volume", node, arg, "--limit", "1.5"]
  case result of
    Left err -> do
      wpLogF WARNING "wpctl set-volume failed: %s" err
      return False
    Right _ -> return True
