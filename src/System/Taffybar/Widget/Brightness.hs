module System.Taffybar.Widget.Brightness (brightnessBarNew) where

import Graphics.UI.Gtk (Widget)
import System.Taffybar.Information.Brightness (readBrightnessValues)
import System.Taffybar.Widget.Generic.PollingBar (verticalBarFromChannel)

brightnessBarNew :: IO Widget
brightnessBarNew = do
  chan <- readBrightnessValues
  ourChan <- dupChan chan
  verticalBarFromChannel ourChan
