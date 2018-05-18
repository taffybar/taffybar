module System.Taffybar.Widget.Brightness (brightnessBarNew) where

import Control.Concurrent.Chan (dupChan)
import Graphics.UI.Gtk (Widget)
import System.Taffybar.Widget.Generic.VerticalBar (BarConfig)
import System.Taffybar.Widget.Generic.PollingBar (verticalBarFromChannel)
import System.Taffybar.Information.Brightness (readBrightnessValues)

brightnessBarNew :: BarConfig -> IO Widget
brightnessBarNew cfg = do
  chan <- readBrightnessValues
  ourChan <- dupChan chan
  verticalBarFromChannel cfg ourChan
