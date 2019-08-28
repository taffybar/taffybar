module System.Taffybar.Widget.Text.MemoryMonitor (textMemoryMonitorNew) where

import qualified Text.StringTemplate as ST
import System.Taffybar.Context (TaffyIO)
import System.Taffybar.Information.Memory
import System.Taffybar.Widget.Generic.PollingLabel ( pollingLabelNew )
import qualified GI.Gtk

-- | Creates a simple textual memory monitor. It updates once every polling
-- period (in seconds).
textMemoryMonitorNew :: String -- ^ Format. You can use variables: "used", "total", "free", "buffer", "cache", "rest", "used".
                     -> Double -- ^ Polling period in seconds.
                     -> TaffyIO GI.Gtk.Widget
textMemoryMonitorNew fmt period = do
    label <- pollingLabelNew period callback
    GI.Gtk.toWidget label
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
