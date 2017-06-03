module System.Taffybar.Volume (
  volumeTextNew,
  volumeControlNew
) where

import System.Information.Volume
import System.Taffybar.Widgets.PollingLabel
import Graphics.UI.Gtk

-- | Creates a new text volume meter
volumeTextNew :: String
                 -> String
                 -> Double
                 -> IO Widget
volumeTextNew mixer control pollSeconds = do
  l <- pollingLabelNew "" pollSeconds . fmap show $ getVolume mixer control
  widgetShowAll l
  return l

-- | Creates a new volume meter widget
volumeControlNew :: String -> String -> IO Widget
volumeControlNew mixer control = do
  b <- volumeButtonNew
  _ <- on b scaleButtonValueChanged $ \v ->
    setVolume mixer control (v * 100)
  let w = toWidget b
  widgetShowAll w
  return w
