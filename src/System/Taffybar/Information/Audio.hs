{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      : System.Taffybar.Information.Audio
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan A. Malison
-- Stability   : unstable
-- Portability : unportable
--
-- Unified audio information for PulseAudio and PipeWire/WirePlumber systems.
--
-- PulseAudio is preferred when its DBus protocol is available. On modern
-- PipeWire systems where @pipewire-pulse@ does not expose that DBus protocol,
-- this module falls back to the WirePlumber backend.
module System.Taffybar.Information.Audio
  ( AudioBackend (..),
    AudioInfo (..),
    defaultAudioPulseSink,
    defaultAudioWirePlumberNode,
    audioBackendAvailable,
    getAudioInfo,
    getAudioInfoChan,
    getAudioInfoState,
    getAudioInfoChanAndVar,
    getAudioInfoChanFor,
    getAudioInfoStateFor,
    toggleAudioMute,
    adjustAudioVolume,
    pulseSinkToWirePlumberNode,
  )
where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Concurrent.STM.TChan
import Control.Monad (forever, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.STM (atomically)
import Data.Proxy (Proxy (..))
#ifdef WITH_WIREPLUMBER
import qualified Data.Text as T
#endif
import GHC.TypeLits (KnownSymbol, SomeSymbol (..), Symbol, someSymbolVal, symbolVal)
import System.Taffybar.Context (TaffyIO)
import qualified System.Taffybar.Information.PulseAudio as PulseAudio
#ifdef WITH_WIREPLUMBER
import qualified System.Taffybar.Information.WirePlumber as WirePlumber
#endif

data AudioBackend
  = PulseAudioBackend
  | WirePlumberBackend
  deriving (Eq, Show)

data AudioInfo = AudioInfo
  { audioVolumePercent :: Maybe Int,
    audioMuted :: Maybe Bool,
    audioNodeName :: String,
    audioBackend :: AudioBackend
  }
  deriving (Eq, Show)

defaultAudioPulseSink :: String
defaultAudioPulseSink = "@DEFAULT_SINK@"

defaultAudioWirePlumberNode :: String
defaultAudioWirePlumberNode = "@DEFAULT_AUDIO_SINK@"

newtype AudioInfoChanVar (a :: Symbol)
  = AudioInfoChanVar (TChan (Maybe AudioInfo), MVar (Maybe AudioInfo))

audioBackendAvailable :: IO AudioBackend
audioBackendAvailable = do
#ifdef WITH_WIREPLUMBER
  pulseAvailable <- PulseAudio.pulseAudioAvailable
  pure $
    if pulseAvailable
      then PulseAudioBackend
      else WirePlumberBackend
#else
  pure PulseAudioBackend
#endif

getAudioInfo :: String -> String -> IO (Maybe AudioInfo)
getAudioInfo pulseSink wirePlumberNode = do
  backend <- audioBackendAvailable
  case backend of
    PulseAudioBackend -> fmap fromPulseAudioInfo <$> PulseAudio.getPulseAudioInfo pulseSink
#ifdef WITH_WIREPLUMBER
    WirePlumberBackend -> fmap fromWirePlumberInfo <$> WirePlumber.getWirePlumberInfo wirePlumberNode
#else
    WirePlumberBackend -> wirePlumberNode `seq` pure Nothing
#endif

getAudioInfoChan :: String -> String -> TaffyIO (TChan (Maybe AudioInfo))
getAudioInfoChan pulseSink wirePlumberNode =
  case someSymbolVal $ encodeAudioSpecs pulseSink wirePlumberNode of
    SomeSymbol (Proxy :: Proxy sym) -> getAudioInfoChanFor @sym

getAudioInfoState :: String -> String -> TaffyIO (Maybe AudioInfo)
getAudioInfoState pulseSink wirePlumberNode =
  case someSymbolVal $ encodeAudioSpecs pulseSink wirePlumberNode of
    SomeSymbol (Proxy :: Proxy sym) -> getAudioInfoStateFor @sym

getAudioInfoChanAndVar :: String -> String -> TaffyIO (TChan (Maybe AudioInfo), MVar (Maybe AudioInfo))
getAudioInfoChanAndVar pulseSink wirePlumberNode =
  case someSymbolVal $ encodeAudioSpecs pulseSink wirePlumberNode of
    SomeSymbol (Proxy :: Proxy sym) -> do
      AudioInfoChanVar pair <- getAudioInfoChanVarFor @sym
      pure pair

getAudioInfoChanFor :: forall a. (KnownSymbol a) => TaffyIO (TChan (Maybe AudioInfo))
getAudioInfoChanFor = do
  AudioInfoChanVar (chan, _) <- getAudioInfoChanVarFor @a
  pure chan

getAudioInfoStateFor :: forall a. (KnownSymbol a) => TaffyIO (Maybe AudioInfo)
getAudioInfoStateFor = do
  AudioInfoChanVar (_, var) <- getAudioInfoChanVarFor @a
  liftIO $ readMVar var

getAudioInfoChanVarFor :: forall a. (KnownSymbol a) => TaffyIO (AudioInfoChanVar a)
getAudioInfoChanVarFor = do
  let (pulseSink, wirePlumberNode) = decodeAudioSpecs $ symbolVal (Proxy @a)
  backend <- liftIO audioBackendAvailable
  case backend of
    PulseAudioBackend -> do
      (pulseChan, pulseVar) <- PulseAudio.getPulseAudioInfoChanAndVar pulseSink
      buildMappedChanVar fromPulseAudioInfo pulseChan pulseVar
#ifdef WITH_WIREPLUMBER
    WirePlumberBackend -> do
      (wireChan, wireVar) <- WirePlumber.getWirePlumberInfoChanAndVar wirePlumberNode
      buildMappedChanVar fromWirePlumberInfo wireChan wireVar
#else
    WirePlumberBackend -> liftIO $ wirePlumberNode `seq` buildUnavailableChanVar
#endif

toggleAudioMute :: String -> String -> IO Bool
toggleAudioMute pulseSink wirePlumberNode = do
  backend <- audioBackendAvailable
  case backend of
    PulseAudioBackend -> PulseAudio.togglePulseAudioMute pulseSink
#ifdef WITH_WIREPLUMBER
    WirePlumberBackend -> WirePlumber.toggleWirePlumberMute wirePlumberNode
#else
    WirePlumberBackend -> wirePlumberNode `seq` pure False
#endif

adjustAudioVolume :: String -> String -> Int -> IO Bool
adjustAudioVolume pulseSink wirePlumberNode deltaPercent = do
  backend <- audioBackendAvailable
  case backend of
    PulseAudioBackend -> PulseAudio.adjustPulseAudioVolume pulseSink deltaPercent
#ifdef WITH_WIREPLUMBER
    WirePlumberBackend -> WirePlumber.adjustWirePlumberVolume wirePlumberNode deltaPercent
#else
    WirePlumberBackend -> wirePlumberNode `seq` pure False
#endif

pulseSinkToWirePlumberNode :: String -> String
pulseSinkToWirePlumberNode "" = defaultAudioWirePlumberNode
pulseSinkToWirePlumberNode sink
  | sink == defaultAudioPulseSink = defaultAudioWirePlumberNode
  | otherwise = sink

buildMappedChanVar ::
  (backendInfo -> AudioInfo) ->
  TChan (Maybe backendInfo) ->
  MVar (Maybe backendInfo) ->
  TaffyIO (AudioInfoChanVar a)
buildMappedChanVar convert backendChan backendVar = do
  initialInfo <- liftIO $ fmap convert <$> readMVar backendVar
  liftIO $ do
    chan <- newBroadcastTChanIO
    var <- newMVar initialInfo
    atomically $ writeTChan chan initialInfo
    _ <- forkIO $ do
      ourChan <- atomically $ dupTChan backendChan
      forever $ do
        info <- fmap convert <$> atomically (readTChan ourChan)
        oldInfo <- swapMVar var info
        when (oldInfo /= info) $
          atomically $
            writeTChan chan info
    pure $ AudioInfoChanVar (chan, var)

#ifndef WITH_WIREPLUMBER
buildUnavailableChanVar :: IO (AudioInfoChanVar a)
buildUnavailableChanVar = do
  chan <- newBroadcastTChanIO
  var <- newMVar Nothing
  atomically $ writeTChan chan Nothing
  pure $ AudioInfoChanVar (chan, var)
#endif

fromPulseAudioInfo :: PulseAudio.PulseAudioInfo -> AudioInfo
fromPulseAudioInfo info =
  AudioInfo
    { audioVolumePercent = PulseAudio.pulseAudioVolumePercent info,
      audioMuted = PulseAudio.pulseAudioMuted info,
      audioNodeName = PulseAudio.pulseAudioSinkName info,
      audioBackend = PulseAudioBackend
    }

#ifdef WITH_WIREPLUMBER
fromWirePlumberInfo :: WirePlumber.WirePlumberInfo -> AudioInfo
fromWirePlumberInfo info =
  AudioInfo
    { audioVolumePercent = Just $ round $ WirePlumber.wirePlumberVolume info * 100,
      audioMuted = Just $ WirePlumber.wirePlumberMuted info,
      audioNodeName = T.unpack $ WirePlumber.wirePlumberNodeName info,
      audioBackend = WirePlumberBackend
    }
#endif

encodeAudioSpecs :: String -> String -> String
encodeAudioSpecs pulseSink wirePlumberNode = pulseSink ++ "\n" ++ wirePlumberNode

decodeAudioSpecs :: String -> (String, String)
decodeAudioSpecs encoded =
  case break (== '\n') encoded of
    (pulseSink, '\n' : wirePlumberNode) -> (pulseSink, wirePlumberNode)
    (pulseSink, _) -> (pulseSink, pulseSinkToWirePlumberNode pulseSink)
