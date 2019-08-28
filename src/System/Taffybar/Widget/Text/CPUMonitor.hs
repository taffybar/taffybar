module System.Taffybar.Widget.Text.CPUMonitor (textCpuMonitorNew) where

import Text.Printf ( printf )
import qualified Text.StringTemplate as ST
import System.Taffybar.Context (TaffyIO)
import System.Taffybar.Information.CPU
import System.Taffybar.Widget.Generic.PollingLabel ( pollingLabelNew )
import qualified GI.Gtk

-- | Creates a simple textual CPU monitor. It updates once every polling
-- period (in seconds).
textCpuMonitorNew :: String -- ^ Format. You can use variables: $total$, $user$, $system$
                  -> Double -- ^ Polling period (in seconds)
                  -> TaffyIO GI.Gtk.Widget
textCpuMonitorNew fmt period = do
  label <- pollingLabelNew period callback
  GI.Gtk.toWidget label
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
