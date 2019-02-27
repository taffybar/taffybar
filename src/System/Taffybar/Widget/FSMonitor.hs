{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      : System.Taffybar.Widget.FSMonitor
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

module System.Taffybar.Widget.FSMonitor ( fsMonitorNew ) where

import           Control.Monad.IO.Class
import qualified GI.Gtk
import           System.Process ( readProcess )
import           System.Taffybar.Widget.Generic.PollingLabel ( pollingLabelNew )
import qualified Data.Text as T

-- | Creates a new filesystem monitor widget. It contains one 'PollingLabel'
-- that displays the data returned by the df command. The usage level of all
-- requested partitions is extracted in one single operation.
fsMonitorNew
  :: MonadIO m
  => Double -- ^ Polling interval (in seconds, e.g. 500)
  -> [String] -- ^ Names of the partitions to monitor (e.g. [\"\/\", \"\/home\"])
  -> m GI.Gtk.Widget
fsMonitorNew interval fsList = liftIO $ do
  label <- pollingLabelNew interval $ showFSInfo fsList
  GI.Gtk.widgetShowAll label
  GI.Gtk.toWidget label

showFSInfo :: [String] -> IO T.Text
showFSInfo fsList = do
  fsOut <- readProcess "df" ("-kP":fsList) ""
  let fss = map (take 2 . reverse . words) $ drop 1 $ lines fsOut
  return $ T.pack $ unwords $ map ((\s -> "[" ++ s ++ "]") . unwords) fss
