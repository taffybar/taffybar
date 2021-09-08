-----------------------------------------------------------------------------
-- |
-- Module      : System.Taffybar.Hooks
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan A. Malison
-- Stability   : unstable
-- Portability : unportable
--
-- This module provides various startup hooks that can be added to 'TaffyConfig'.
-----------------------------------------------------------------------------

module System.Taffybar.Hooks
  ( module System.Taffybar.DBus
  , module System.Taffybar.Hooks
  , ChromeTabImageData(..)
  , getChromeTabImageDataChannel
  , getChromeTabImageDataTable
  , getX11WindowToChromeTabId
  , refreshBatteriesOnPropChange
  ) where

import           BroadcastChan
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import qualified Data.MultiMap as MM
import           System.Log.Logger
import           System.Taffybar.Context
import           System.Taffybar.DBus
import           System.Taffybar.Information.Battery
import           System.Taffybar.Information.Chrome
import           System.Taffybar.Information.Network
import           System.Environment.XDG.DesktopEntry
import           System.Taffybar.LogFormatter
import           System.Taffybar.Util

-- | The type of the channel that provides network information in taffybar.
newtype NetworkInfoChan =
  NetworkInfoChan (BroadcastChan In [(String, (Rational, Rational))])

-- | Build a 'NetworkInfoChan' that refreshes at the provided interval.
buildNetworkInfoChan :: Double -> IO NetworkInfoChan
buildNetworkInfoChan interval = do
  chan <- newBroadcastChan
  _ <- forkIO $ monitorNetworkInterfaces interval (void . writeBChan chan)
  return $ NetworkInfoChan chan

-- | Get the 'NetworkInfoChan' from 'Context', creating it if it does not exist.
getNetworkChan :: TaffyIO NetworkInfoChan
getNetworkChan = getStateDefault $ lift $ buildNetworkInfoChan 2.0

-- | Set the log formatter used in the taffybar process
setTaffyLogFormatter :: String -> IO ()
setTaffyLogFormatter loggerName = do
  handler <- taffyLogHandler
  updateGlobalLogger loggerName $ setHandlers [handler]

-- | Add 'refreshrefreshBatteriesOnPropChange' to the 'startupHook' of the
-- provided 'TaffybarConfig'.
withBatteryRefresh :: TaffybarConfig -> TaffybarConfig
withBatteryRefresh = appendHook refreshBatteriesOnPropChange

-- | Load the 'DesktopEntry' cache from 'Context' state.
getDirectoryEntriesByClassName :: TaffyIO (MM.MultiMap String DesktopEntry)
getDirectoryEntriesByClassName =
  getStateDefault readDirectoryEntriesDefault

-- | Update the 'DesktopEntry' cache every 60 seconds.
updateDirectoryEntriesCache :: TaffyIO ()
updateDirectoryEntriesCache = ask >>= \ctx ->
  void $ lift $ foreverWithDelay (60 :: Double) $ flip runReaderT ctx $
       void $ putState readDirectoryEntriesDefault

-- | Read 'DesktopEntry' values into a 'MM.Multimap', where they are indexed by
-- the class name specified in the 'DesktopEntry'.
readDirectoryEntriesDefault :: TaffyIO (MM.MultiMap String DesktopEntry)
readDirectoryEntriesDefault = lift $
  indexDesktopEntriesByClassName <$> getDirectoryEntriesDefault
