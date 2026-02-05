-----------------------------------------------------------------------------
-- |
-- Module      : System.Taffybar.Hyprland
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan A. Malison
-- Stability   : unstable
-- Portability : unportable
--
-- Context-integrated helpers for the Hyprland client.
--
-- This module follows the existing taffybar pattern of storing shared runtime
-- resources in 'Context' state via 'getStateDefault'. It lets multiple widgets
-- share a single Hyprland client instance and (optionally) a single event socket
-- reader thread that broadcasts events over a 'TChan'.
-----------------------------------------------------------------------------

module System.Taffybar.Hyprland
  ( -- * Shared Client
    getHyprlandClient
  , getHyprlandClientWith

    -- * Hyprland Monad In TaffyIO
  , HyprlandIO
  , runHyprland

    -- * Shared Event Channel
  , getHyprlandEventChan
  , getHyprlandEventChanWith

    -- * Convenience Runners
  , runHyprlandCommandRawT
  , runHyprlandCommandJsonT
  ) where

import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson (FromJSON)
import qualified Data.ByteString as BS

import           Control.Monad.Trans.Reader (ReaderT)

import           System.Taffybar.Context (Context, TaffyIO, getStateDefault)
import           System.Taffybar.Information.Hyprland
  ( HyprlandClient
  , HyprlandClientConfig
  , HyprlandCommand
  , HyprlandError
  , HyprlandEventChan
  , HyprlandT
  , buildHyprlandEventChan
  , defaultHyprlandClientConfig
  , newHyprlandClient
  , runHyprlandCommandJson
  , runHyprlandCommandRaw
  , runHyprlandT
  )

-- | Get a shared 'HyprlandClient' from the 'Context' state.
--
-- Note: this uses 'getStateDefault', so the first call wins and subsequent
-- calls return the existing client.
getHyprlandClient :: TaffyIO HyprlandClient
getHyprlandClient = getHyprlandClientWith defaultHyprlandClientConfig

-- | Like 'getHyprlandClient', but allows supplying the initial config.
--
-- The config is only used if the client has not already been created and
-- stored in the 'Context' state.
getHyprlandClientWith :: HyprlandClientConfig -> TaffyIO HyprlandClient
getHyprlandClientWith cfg =
  getStateDefault $ liftIO $ newHyprlandClient cfg

-- | Hyprland actions in the 'TaffyIO' context.
--
-- Note: 'TaffyIO' is a type synonym, and type synonyms cannot be partially
-- applied. We therefore spell out the underlying 'ReaderT Context IO' here.
type HyprlandIO a = HyprlandT (ReaderT Context IO) a

runHyprland :: HyprlandIO a -> TaffyIO a
runHyprland action = getHyprlandClient >>= \client -> runHyprlandT client action

-- | Get a shared Hyprland event channel from the 'Context' state.
--
-- The channel is backed by a single event socket reader thread, and is safe to
-- use concurrently by multiple widgets via 'subscribeHyprlandEvents'.
getHyprlandEventChan :: TaffyIO HyprlandEventChan
getHyprlandEventChan = getHyprlandEventChanWith defaultHyprlandClientConfig

-- | Like 'getHyprlandEventChan', but allows supplying the initial client config.
--
-- The config is only used if the channel has not already been created and
-- stored in the 'Context' state.
getHyprlandEventChanWith :: HyprlandClientConfig -> TaffyIO HyprlandEventChan
getHyprlandEventChanWith cfg =
  getStateDefault $ do
    client <- getHyprlandClientWith cfg
    liftIO $ buildHyprlandEventChan client

runHyprlandCommandRawT :: HyprlandCommand -> TaffyIO (Either HyprlandError BS.ByteString)
runHyprlandCommandRawT cmd =
  getHyprlandClient >>= \client -> liftIO $ runHyprlandCommandRaw client cmd

runHyprlandCommandJsonT :: FromJSON a => HyprlandCommand -> TaffyIO (Either HyprlandError a)
runHyprlandCommandJsonT cmd =
  getHyprlandClient >>= \client -> liftIO $ runHyprlandCommandJson client cmd
