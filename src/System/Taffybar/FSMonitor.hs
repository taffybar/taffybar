module System.Taffybar.FSMonitor where

import Graphics.UI.Gtk
import System.Process (readProcess)
import System.Taffybar.Widgets.PollingLabel
import Text.Printf (printf)

fsMonitorNew :: Double -> [String] -> IO Widget
fsMonitorNew interval fsList = do
    label <- pollingLabelNew "" interval $ showFSInfo fsList
    widgetShowAll label
    return $ toWidget label
    where
        showFSInfo :: [String] -> IO String
        showFSInfo fsDict = do
            fsOut <- readProcess "df" (["-kP"] ++ fsList) ""
            let fss = map ((take 2) . reverse . words) $ drop 1 $ lines fsOut
            return $ unwords $ map ((\s -> "[" ++ s ++ "]") . unwords) fss
