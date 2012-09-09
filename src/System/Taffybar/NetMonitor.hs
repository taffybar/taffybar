module System.Taffybar.NetMonitor (netMonitorNew) where

import Data.IORef
import Graphics.UI.Gtk
import System.Information.Network (getNetInfo)
import System.Taffybar.Widgets.PollingLabel
import Text.Printf (printf)

netMonitorNew :: Double -> String -> IO Widget
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
