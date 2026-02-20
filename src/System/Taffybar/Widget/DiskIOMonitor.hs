{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------

-- |
-- Module      : System.Taffybar.Widget.DiskIOMonitor
-- Copyright   : (c) José A. Romero L.
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : José A. Romero L. <escherdragon@gmail.com>
-- Stability   : unstable
-- Portability : unportable
--
-- Simple Disk IO monitor that uses a PollingGraph to visualize the speed of
-- read/write operations in one selected disk or partition.
module System.Taffybar.Widget.DiskIOMonitor (dioMonitorNew) where

import qualified GI.Gtk
import System.Taffybar.Context (TaffyIO)
import System.Taffybar.Information.DiskIO (getDiskTransfer)
import System.Taffybar.Widget.Generic.PollingGraph (GraphConfig, pollingGraphNew)
import System.Taffybar.Widget.Util (widgetSetClassGI)

-- | Creates a new disk IO monitor widget. This is a 'PollingGraph' fed by
-- regular calls to 'getDiskTransfer'. The results of calling this function
-- are normalized to the maximum value of the obtained probe (either read or
-- write transfer).
dioMonitorNew ::
  -- | Configuration data for the Graph.
  GraphConfig ->
  -- | Polling period (in seconds).
  Double ->
  -- | Name of the disk or partition to watch (e.g. \"sda\", \"sdb1\").
  String ->
  TaffyIO GI.Gtk.Widget
dioMonitorNew cfg pollSeconds disk = do
  widget <- pollingGraphNew cfg pollSeconds (probeDisk disk)
  widgetSetClassGI widget "disk-io-monitor"

probeDisk :: String -> IO [Double]
probeDisk disk = do
  transfer <- getDiskTransfer disk
  let top = foldr max 1.0 transfer
  return $ map (/ top) transfer
