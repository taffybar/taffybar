-- | Like the vertical bar, but this widget automatically updates
-- itself with a callback at fixed intervals.
module System.Taffybar.Widget.Generic.PollingBar (
  -- * Types
  VerticalBarHandle,
  BarConfig(..),
  BarDirection(..),
  -- * Constructors and accessors
  pollingBarNew,
  verticalBarFromCallback,
  defaultBarConfig
  ) where

import Control.Concurrent
import Control.Exception.Enclosed ( tryAny )
import Graphics.UI.Gtk
import System.Taffybar.Util ( backgroundLoop )
import System.Taffybar.Widget.Util ( drawOn )

import System.Taffybar.Widget.Generic.VerticalBar

verticalBarFromChannel :: BarConfig -> Chan Double -> IO Widget
verticalBarFromChannel cfg chan = verticalBarFromCallback cfg $ readChan chan

verticalBarFromCallback :: BarConfig -> IO Double -> IO Widget
verticalBarFromCallback cfg action = do
  (drawArea, h) <- verticalBarNew cfg
  drawOn drawArea $
    backgroundLoop $ do
      esample <- tryAny action
      traverse (verticalBarSetPercent h) esample

pollingBarNew :: BarConfig -> Double -> IO Double -> IO Widget
pollingBarNew cfg pollSeconds action =
  verticalBarFromCallback cfg $ action <* delay
  where delay = threadDelay $ floor (pollSeconds * 1000000)
