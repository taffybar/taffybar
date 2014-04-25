module System.Taffybar.Text.MemoryMonitor(textMemoryMonitorNew) where

import qualified Text.StringTemplate as ST
import System.Information.Memory
import System.Taffybar.Widgets.PollingLabel ( pollingLabelNew )
import qualified Graphics.UI.Gtk as Gtk

-- | Creates a simple textual memory monitor. It updates once every polling
-- period (in seconds).
textMemoryMonitorNew :: String -- ^ Format. You can use variables: "used", "total", "free", "buffer", "cache", "rest", "used".
                     -> Double -- ^ Polling period in seconds.
                     -> IO Gtk.Widget
textMemoryMonitorNew fmt period = do
    label <- pollingLabelNew fmt period callback
    Gtk.widgetShowAll label
    return label
    where
      callback = do
        info <- parseMeminfo
        let template = ST.newSTMP fmt
        let labels = ["used", "total", "free", "buffer", "cache", "rest", "used"]
        let actions = [memoryUsed, memoryTotal, memoryFree, memoryBuffer, memoryCache, memoryRest]
            actions' = map ((show . intRound).) actions
        let stats = [f info | f <- actions']
        let template' = ST.setManyAttrib (zip labels stats) template
        return $ ST.render template'

intRound :: Double -> Int
intRound = round
