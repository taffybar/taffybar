module System.Taffybar.Volume (
  volumeTextNew
) where

import System.Information.Volume
import System.Taffybar.Widgets.PollingLabel
import Graphics.UI.Gtk

volumeTextNew :: String
                 -> String
                 -> Double
                 -> IO Widget
volumeTextNew mixer control pollSeconds = do
  l <- pollingLabelNew "" pollSeconds ((getVolume mixer control >>= return . show))
  widgetShowAll l
  return l
