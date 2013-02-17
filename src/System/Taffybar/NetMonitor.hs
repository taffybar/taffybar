-----------------------------------------------------------------------------
-- |
-- Module      : System.Taffybar.NetMonitor
-- Copyright   : (c) José A. Romero L.
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : José A. Romero L. <escherdragon@gmail.com>
-- Stability   : unstable
-- Portability : unportable
--
-- Simple text widget that displays incoming\/outgoing network traffic over
-- one selected interface, as provided by the "System.Information.Network"
-- module.
--
-----------------------------------------------------------------------------

module System.Taffybar.NetMonitor (netMonitorNew) where

import Data.IORef
import Graphics.UI.Gtk
import System.Information.Network (getNetInfo)
import System.Taffybar.Widgets.PollingLabel
import Text.Printf (printf)

-- | Creates a new network monitor widget. It consists of two 'PollingLabel's,
-- one for incoming and one for outgoing traffic fed by regular calls to
-- 'getNetInfo'.
netMonitorNew :: Double -- ^ Polling interval (in seconds, e.g. 1.5)
              -> String -- ^ Name of the network interface to monitor (e.g. \"eth0\", \"wlan1\")
              -> IO Widget
netMonitorNew interval interface = do
    sample <- newIORef [0, 0]
    label  <- pollingLabelNew "" interval $ showInfo sample interval interface
    widgetShowAll label
    return $ toWidget label

showInfo :: IORef [Integer] -> Double -> String -> IO String
showInfo sample interval interface = do
    this <- getNetInfo interface
    last <- readIORef sample
    writeIORef sample this
    let deltas = map fromIntegral $ zipWith (-) this last
        [incoming, outgoing] = map (/(interval*1e3)) deltas
    return $ printf "▼ %.2fkb/s ▲ %.2fkb/s" incoming outgoing
