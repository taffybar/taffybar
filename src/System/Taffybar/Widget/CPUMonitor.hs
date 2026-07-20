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

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (atomicModifyIORef', newIORef)
import qualified GI.Gdk as Gdk
import qualified GI.Gtk
import System.Taffybar.Context (TaffyIO)
import System.Taffybar.Information.CPU2
  ( CPULoad (..),
    acquireCPULoadFastRefresh,
    cpuLoadSourceChannel,
    forceCPULoadRefresh,
    getCPULoadSource,
  )
import System.Taffybar.Widget.Generic.ChannelGraph
import System.Taffybar.Widget.Generic.Graph
import System.Taffybar.Widget.Util (widgetSetClassGI)

-- | Creates a new CPU monitor. This is a channel-driven graph fed by CPU load
-- samples for one core (or all cores when "cpu" is selected).
cpuMonitorNew ::
  -- | Configuration data for the Graph.
  GraphConfig ->
  -- | Polling period (in seconds).
  Double ->
  -- | Name of the core to watch (e.g. \"cpu\", \"cpu0\").
  String ->
  TaffyIO GI.Gtk.Widget
cpuMonitorNew cfg interval cpu = do
  cpuMonitorNewWithHover cfg interval (min interval 0.5) cpu

-- | Create a CPU monitor that temporarily requests a faster coordinated
-- sampling cadence while the pointer is over the graph. Pointer leave and
-- widget unrealize both release the request, so the fast scheduler interval
-- does not remain active in the background.
cpuMonitorNewWithHover ::
  -- | Configuration data for the graph.
  GraphConfig ->
  -- | Normal polling period, in seconds.
  Double ->
  -- | Polling period while hovered, in seconds.
  Double ->
  -- | Name of the core to watch (for example, @"cpu"@ or @"cpu0"@).
  String ->
  TaffyIO GI.Gtk.Widget
cpuMonitorNewWithHover cfg interval hoverInterval cpu = do
  source <- getCPULoadSource cpu interval hoverInterval
  liftIO $ do
    graphWidget <- channelGraphNew cfg (cpuLoadSourceChannel source) toSample
    eventBox <- GI.Gtk.eventBoxNew
    GI.Gtk.eventBoxSetVisibleWindow eventBox False
    GI.Gtk.eventBoxSetAboveChild eventBox True
    GI.Gtk.containerAdd eventBox graphWidget
    widget <- GI.Gtk.toWidget eventBox >>= (`widgetSetClassGI` "cpu-monitor")
    releaseRef <- newIORef Nothing

    let beginFastRefresh = do
          currentRelease <- atomicModifyIORef' releaseRef $ \current -> (current, current)
          case currentRelease of
            Just _ -> pure ()
            Nothing -> do
              release <- acquireCPULoadFastRefresh source
              previous <- atomicModifyIORef' releaseRef $ \current -> (Just release, current)
              maybe (pure ()) id previous

        endFastRefresh = do
          release <- atomicModifyIORef' releaseRef $ \current -> (Nothing, current)
          maybe (pure ()) id release

    GI.Gtk.widgetAddEvents
      widget
      [ Gdk.EventMaskEnterNotifyMask,
        Gdk.EventMaskLeaveNotifyMask
      ]
    void $ GI.Gtk.onWidgetEnterNotifyEvent widget $ \_ -> beginFastRefresh >> pure False
    void $ GI.Gtk.onWidgetLeaveNotifyEvent widget $ \_ -> endFastRefresh >> pure False
    void $ GI.Gtk.onWidgetRealize graphWidget $ forceCPULoadRefresh source
    void $ GI.Gtk.onWidgetUnrealize widget endFastRefresh
    pure widget

toSample :: CPULoad -> IO [Double]
toSample CPULoad {cpuTotalLoad = totalLoad, cpuSystemLoad = systemLoad} =
  return [totalLoad, systemLoad]
