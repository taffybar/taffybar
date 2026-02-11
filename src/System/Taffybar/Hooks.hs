-----------------------------------------------------------------------------

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
-- This module provides various startup hooks that can be added to
-- 'TaffybarConfig'.
module System.Taffybar.Hooks
  ( module System.Taffybar.DBus,
    module System.Taffybar.Hooks,
    module System.Taffybar.LogLevels,
    ChromeTabImageData (..),
    getChromeTabImageDataChannel,
    getChromeTabImageDataTable,
    getX11WindowToChromeTabId,
    refreshBatteriesOnPropChange,
  )
where

import Control.Concurrent
import Control.Concurrent.STM.TChan
import Control.Monad
import Control.Monad.STM (atomically)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import qualified Data.MultiMap as MM
import System.Environment.XDG.DesktopEntry
import System.Log.Logger
import System.Taffybar.Context
import System.Taffybar.DBus
import System.Taffybar.Information.Battery
import System.Taffybar.Information.Chrome
import System.Taffybar.Information.Network
import System.Taffybar.LogFormatter
import System.Taffybar.LogLevels
import System.Taffybar.Util

-- | The type of the channel that provides network information in taffybar.
newtype NetworkInfoChan
  = NetworkInfoChan (TChan [(String, (Rational, Rational))])

-- | Build a 'NetworkInfoChan' that refreshes at the provided interval.
buildNetworkInfoChan :: Double -> IO NetworkInfoChan
buildNetworkInfoChan interval = do
  chan <- newBroadcastTChanIO
  _ <- forkIO $ monitorNetworkInterfaces interval (void . atomically . writeTChan chan)
  return $ NetworkInfoChan chan

-- | Get the 'NetworkInfoChan' from 'Context', creating it if it does not exist.
getNetworkChan :: TaffyIO NetworkInfoChan
getNetworkChan = getStateDefault $ lift $ buildNetworkInfoChan 2.0

-- | Set the log formatter used in the taffybar process
setTaffyLogFormatter :: String -> IO ()
setTaffyLogFormatter loggerName = do
  handler <- taffyLogHandler
  updateGlobalLogger loggerName $ setHandlers [handler]

-- | Add 'refreshBatteriesOnPropChange' to the 'startupHook' of the
-- provided 'TaffybarConfig'. Use this if your system has issues with
-- the battery widget not updating or reporting the incorrect state.
--
-- This function 'withBatteryRefresh' is __not normally needed__
-- because the battery widget already subscribes to updates from
-- UPower, and UPower usually works correctly.
withBatteryRefresh :: TaffybarConfig -> TaffybarConfig
withBatteryRefresh = appendHook refreshBatteriesOnPropChange

-- | Load log levels from @~\/.config\/taffybar\/log-levels.yaml@ during
-- startup. The file should contain a YAML mapping from logger names to log
-- level strings. If the file does not exist, this is a no-op.
--
-- Example @log-levels.yaml@:
--
-- > System.Taffybar.DBus.Toggle: DEBUG
-- > Graphics.UI.GIGtkStrut: INFO
--
-- To use a custom path instead, call 'loadLogLevelsFromFile' directly.
withLogLevels :: TaffybarConfig -> TaffybarConfig
withLogLevels =
  appendHook $
    lift $
      defaultLogLevelsPath >>= loadLogLevelsFromFile

-- | Load the 'DesktopEntry' cache from 'Context' state.
getDirectoryEntriesByClassName :: TaffyIO (MM.MultiMap String DesktopEntry)
getDirectoryEntriesByClassName =
  getStateDefault readDirectoryEntriesDefault

-- | Update the 'DesktopEntry' cache every 60 seconds.
updateDirectoryEntriesCache :: TaffyIO ()
updateDirectoryEntriesCache =
  ask >>= \ctx ->
    void $
      lift $
        foreverWithDelay (60 :: Double) $
          flip runReaderT ctx $
            void $
              putState readDirectoryEntriesDefault

-- | Read 'DesktopEntry' values into a 'MM.Multimap', where they are indexed by
-- the class name specified in the 'DesktopEntry'.
readDirectoryEntriesDefault :: TaffyIO (MM.MultiMap String DesktopEntry)
readDirectoryEntriesDefault =
  lift $
    indexDesktopEntriesByClassName <$> getDirectoryEntriesDefault
