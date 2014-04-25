module System.Taffybar.Text.CPUMonitor(textCpuMonitorNew) where

import Text.Printf
import Text.StringTemplate
import System.Information.CPU
import System.Taffybar.Widgets.PollingLabel
import Graphics.UI.Gtk

-- | Creates a simple textual CPU monitor. It updates once every polling
-- period (in seconds).
textCpuMonitorNew :: String -- ^ Format. You can use variables: $total$, $user$, $system$
                  -> Double -- ^ Polling period (in seconds)
                  -> IO Widget
textCpuMonitorNew fmt period = do
        label <- pollingLabelNew fmt period callback
        widgetShowAll label
        return label
        where
            callback = do
                (userLoad, systemLoad, totalLoad) <- cpuLoad
                let [userLoad', systemLoad', totalLoad'] = map (((printf "%.2f") :: Double -> String).(*100)) [userLoad, systemLoad, totalLoad]
                let template = newSTMP fmt
                let template' = setManyAttrib [ ("user", userLoad'),
                                              ("system", systemLoad'),
                                              ("total", totalLoad') ] template
                return $ render template'

