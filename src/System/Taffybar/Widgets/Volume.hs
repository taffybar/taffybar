module System.Taffybar.Widgets.Volume
  ( volumeTextNew
  , volumeControlNew
  ) where

import Control.Monad.Trans
import Graphics.UI.Gtk
import System.Taffybar.Information.Volume
import System.Taffybar.Widgets.Generic.PollingLabel

-- | Creates a new text volume meter
volumeTextNew :: MonadIO m => String -> String -> Double -> m Widget
volumeTextNew mixer control pollSeconds = liftIO $ do
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
