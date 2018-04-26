--------------------------------------------------------------------------------
-- |
-- Module      : System.Taffybar.Widget.CPUMonitor
-- Copyright   : (c) José A. Romero L.
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : José A. Romero L. <escherdragon@gmail.com>
-- Stability   : unstable
-- Portability : unportable
--
-- Simple CPU monitor that uses a PollingGraph to visualize variations in the
-- user and system CPU times in one selected core, or in all cores available.
--
--------------------------------------------------------------------------------
module System.Taffybar.Widget.CPUMonitor where

import Control.Monad.Trans
import Data.IORef
import Graphics.UI.Gtk
import System.Taffybar.Information.CPU2 (getCPUInfo)
import System.Taffybar.Information.StreamInfo (getAccLoad)
import System.Taffybar.Widget.Generic.PollingGraph

-- | Creates a new CPU monitor. This is a PollingGraph fed by regular calls to
-- getCPUInfo, associated to an IORef used to remember the values yielded by the
-- last call to this function.
cpuMonitorNew
  :: MonadIO m
  => GraphConfig -- ^ Configuration data for the Graph.
  -> Double -- ^ Polling period (in seconds).
  -> String -- ^ Name of the core to watch (e.g. \"cpu\", \"cpu0\").
  -> m Widget
cpuMonitorNew cfg interval cpu = liftIO $ do
    info <- getCPUInfo cpu
    sample <- newIORef info
    pollingGraphNew cfg interval $ probe sample cpu

probe :: IORef [Int] -> String -> IO [Double]
probe sample cpuName = do
    load <- getAccLoad sample $ getCPUInfo cpuName
    case load of
      l0:l1:l2:_ -> return [ l0 + l1, l2 ] -- user, system
      _ -> return []
