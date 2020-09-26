module System.Taffybar.Widget.Text.MemoryMonitor (textMemoryMonitorNew) where

import Control.Monad.IO.Class ( MonadIO )
import qualified Text.StringTemplate as ST
import System.Taffybar.Information.Memory
import System.Taffybar.Widget.Generic.PollingLabel ( pollingLabelNew )
import qualified GI.Gtk
import Text.Printf ( printf )

-- | Creates a simple textual memory monitor. It updates once every polling
-- period (in seconds).
textMemoryMonitorNew :: MonadIO m
                     => String -- ^ Format. You can use variables: "used", "total", "free", "buffer", "cache", "rest", "available".
                     -> Double -- ^ Polling period in seconds.
                     -> m GI.Gtk.Widget
textMemoryMonitorNew fmt period = do
    label <- pollingLabelNew period callback
    GI.Gtk.toWidget label
    where
      callback = do
        info <- parseMeminfo
        let template = ST.newSTMP fmt
        let labels = ["used", "total", "free", "buffer", "cache", "rest", "available"]
        let actions = [memoryUsed, memoryTotal, memoryFree, memoryBuffer, memoryCache, memoryRest, memoryAvailable]
            actions' = map ((toAuto 3).) actions
        let stats = [f info | f <- actions']
        let template' = ST.setManyAttrib (zip labels stats) template
        return $ ST.render template'

toAuto :: Int -> Double -> String
toAuto prec value = printf "%.*f%s" p v unit
  where value' = max 0 value
        mag :: Int
        mag = if value' == 0 then 0 else max 0 $ min 2 $ floor $ logBase 1024 value'
        v = value' / 1024 ** fromIntegral mag
        unit = case mag of
          0 -> "MiB"
          1 -> "GiB"
          2 -> "TiB"
          _ -> "??B" -- unreachable
        p :: Int
        p = max 0 $ floor $ fromIntegral prec - logBase 10 v
