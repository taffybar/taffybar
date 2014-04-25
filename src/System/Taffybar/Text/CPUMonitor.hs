module System.Taffybar.Text.CPUMonitor(textCpuMonitorNew) where

import Text.Printf ( printf )
import qualified Text.StringTemplate as ST
import System.Information.CPU
import System.Taffybar.Widgets.PollingLabel ( pollingLabelNew )
import qualified Graphics.UI.Gtk as Gtk

-- | Creates a simple textual CPU monitor. It updates once every polling
-- period (in seconds).
textCpuMonitorNew :: String -- ^ Format. You can use variables: $total$, $user$, $system$
                  -> Double -- ^ Polling period (in seconds)
                  -> IO Gtk.Widget
textCpuMonitorNew fmt period = do
  label <- pollingLabelNew fmt period callback
  Gtk.widgetShowAll label
  return label
  where
    callback = do
      (userLoad, systemLoad, totalLoad) <- cpuLoad
      let [userLoad', systemLoad', totalLoad'] = map (formatPercent.(*100)) [userLoad, systemLoad, totalLoad]
      let template = ST.newSTMP fmt
      let template' = ST.setManyAttrib [ ("user", userLoad'),
                                         ("system", systemLoad'),
                                         ("total", totalLoad') ] template
      return $ ST.render template'

formatPercent :: Double -> String
formatPercent = printf "%.2f"
