{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

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
-- Simple CPU monitor that uses a channel-driven graph to visualize variations
-- in the user and system CPU times in one selected core, or in all cores
-- available.
module System.Taffybar.Widget.CPUMonitor where

import Control.Monad.IO.Class
import qualified GI.Gtk
import System.Taffybar.Information.CPU2 (CPULoad (..), getCPULoadChan)
import System.Taffybar.Widget.Generic.ChannelGraph
import System.Taffybar.Widget.Generic.Graph
import System.Taffybar.Widget.Util (widgetSetClassGI)

-- | Creates a new CPU monitor. This is a channel-driven graph fed by CPU load
-- samples for one core (or all cores when "cpu" is selected).
cpuMonitorNew ::
  (MonadIO m) =>
  -- | Configuration data for the Graph.
  GraphConfig ->
  -- | Polling period (in seconds).
  Double ->
  -- | Name of the core to watch (e.g. \"cpu\", \"cpu0\").
  String ->
  m GI.Gtk.Widget
cpuMonitorNew cfg interval cpu = liftIO $ do
  chan <- getCPULoadChan cpu interval
  channelGraphNew cfg chan toSample
    >>= (`widgetSetClassGI` "cpu-monitor")

toSample :: CPULoad -> IO [Double]
toSample CPULoad {cpuTotalLoad = totalLoad, cpuSystemLoad = systemLoad} =
  return [totalLoad, systemLoad]
