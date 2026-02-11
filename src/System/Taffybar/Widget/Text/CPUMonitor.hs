module System.Taffybar.Widget.Text.CPUMonitor (textCpuMonitorNew) where

import Control.Monad.IO.Class (MonadIO)
import qualified GI.Gtk
import System.Taffybar.Information.CPU
import System.Taffybar.Widget.Generic.PollingLabel (pollingLabelNew)
import Text.Printf (printf)
import qualified Text.StringTemplate as ST

-- | Creates a simple textual CPU monitor. It updates once every polling
-- period (in seconds).
textCpuMonitorNew ::
  (MonadIO m) =>
  -- | Format. You can use variables: $total$, $user$, $system$
  String ->
  -- | Polling period (in seconds)
  Double ->
  m GI.Gtk.Widget
textCpuMonitorNew fmt period = do
  label <- pollingLabelNew period callback
  GI.Gtk.toWidget label
  where
    callback = do
      (userLoad, systemLoad, totalLoad) <- cpuLoad
      let pct = formatPercent . (* 100)
      let template = ST.newSTMP fmt
      let template' =
            ST.setManyAttrib
              [ ("user", pct userLoad),
                ("system", pct systemLoad),
                ("total", pct totalLoad)
              ]
              template
      return $ ST.render template'

formatPercent :: Double -> String
formatPercent = printf "%.2f"
