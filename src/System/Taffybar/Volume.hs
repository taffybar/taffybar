module System.Taffybar.Volume (
  volumeTextNew,
  volumeControlNew
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

volumeControlNew :: String -> String -> IO Widget
volumeControlNew mixer control = do
  b <- volumeButtonNew

  _ <- on b scaleButtonValueChanged $ \v -> do
    setVolume mixer control (v * 100)

  let w = toWidget b
  widgetShowAll w
  return w
