-----------------------------------------------------------------------------
-- |
-- Module      : System.Taffybar.FSMonitor
-- Copyright   : (c) José A. Romero L.
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : José A. Romero L. <escherdragon@gmail.com>
-- Stability   : unstable
-- Portability : unportable
--
-- Simple text widget that monitors the current usage of selected disk
-- partitions by regularly parsing the output of the df command in Linux
-- systems.
--
-----------------------------------------------------------------------------

module System.Taffybar.FSMonitor ( fsMonitorNew ) where

import qualified Graphics.UI.Gtk as Gtk
import System.Process ( readProcess )
import System.Taffybar.Widgets.PollingLabel ( pollingLabelNew )

-- | Creates a new filesystem monitor widget. It contains one 'PollingLabel'
-- that displays the data returned by the df command. The usage level of all
-- requested partitions is extracted in one single operation.
fsMonitorNew :: Double -- ^ Polling interval (in seconds, e.g. 500)
             -> [String] -- ^ Names of the partitions to monitor (e.g. [\"\/\", \"\/home\"])
             -> IO Gtk.Widget
fsMonitorNew interval fsList = do
  label <- pollingLabelNew "" interval $ showFSInfo fsList
  Gtk.widgetShowAll label
  return $ Gtk.toWidget label

showFSInfo :: [String] -> IO String
showFSInfo fsList = do
  fsOut <- readProcess "df" (["-kP"] ++ fsList) ""
  let fss = map (take 2 . reverse . words) $ drop 1 $ lines fsOut
  return $ unwords $ map ((\s -> "[" ++ s ++ "]") . unwords) fss
