module System.Taffybar.NetMonitor where

import Graphics.UI.Gtk
import System.Information.Network (getNetInfo)
import System.Information.StreamInfo (getTransfer)
import System.Taffybar.Widgets.PollingLabel
import Text.Printf (printf)

netMonitorNew :: Double -> String -> IO Widget
netMonitorNew interval interface = do
    label <- pollingLabelNew "" interval $ showNetInfo interface
    widgetShowAll label
    return $ toWidget label
    where
        showNetInfo :: String -> IO String
        showNetInfo interface = do
            trans <- getTransfer 0.3 $ getNetInfo interface
            let [incoming, outgoing] = map (/1e3) trans
            return $ printf "▼ %.2fkb/s ▲ %.2fkb/s" incoming outgoing
