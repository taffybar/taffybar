--------------------------------------------------------------------------------
-- |
-- Module      : System.Taffybar.DiskIOMonitor
-- Copyright   : (c) José A. Romero L.
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : José A. Romero L. <escherdragon@gmail.com>
-- Stability   : unstable
-- Portability : unportable
--
-- Simple Disk IO monitor that uses a PollingGraph to visualize the speed of
-- read/write operations in one selected disk or partition.
--
--------------------------------------------------------------------------------

module System.Taffybar.DiskIOMonitor where

import Graphics.UI.Gtk
import System.Information.DiskIO (getDiskTransfer)
import System.Taffybar.Widgets.PollingGraph

-- | Creates a new disk IO monitor widget. This is a 'PollingGraph' fed by
-- regular calls to 'getDiskTransfer'. The results of calling this function
-- are normalized to the maximum value of the obtained probe (either read or
-- write transfer).
dioMonitorNew :: GraphConfig -- ^ Configuration data for the Graph.
              -> Double      -- ^ Polling period (in seconds).
              -> String      -- ^ Name of the disk or partition to watch (e.g. \"sda\", \"sdb1\").
              -> IO Widget
dioMonitorNew cfg pollSeconds disk = do
    pollingGraphNew cfg pollSeconds $ probe disk
    where
        probe :: String -> IO [Double]
        probe disk = do
            transfer <- getDiskTransfer disk
            let top = foldr max 1.0 transfer
            return $ map (/top) transfer
