module System.Taffybar.TextMemoryMonitor(textMemoryMonitorNew) where

import Text.Printf
import Text.StringTemplate
import System.Information.Memory
import System.Taffybar.Widgets.PollingLabel
import Graphics.UI.Gtk

-- | Creates a simple textual memory monitor. It updates once every polling
-- period (in seconds).
textMemoryMonitorNew :: String -- ^ Format. You can use variables: "used", "total", "free", "buffer", "cache", "rest", "used".
                     -> Double -- ^ Polling period in seconds.
                     -> IO Widget
textMemoryMonitorNew fmt period = do
    label <- pollingLabelNew fmt period callback
    widgetShowAll label
    return label
    where
      callback = do
        info <- parseMeminfo 
        let template = newSTMP fmt
        let labels = ["used", "total", "free", "buffer", "cache", "rest", "used"]
        let actions = [memoryUsed, memoryTotal, memoryFree, memoryBuffer, memoryCache, memoryRest]
        let actions' = map ((show.round).) actions
        let stats = [f info | f <- actions']
        let template' = setManyAttrib (zip labels stats) template
        return $ render template'

