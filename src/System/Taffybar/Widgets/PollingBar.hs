-- | Like the vertical bar, but this widget automatically updates
-- itself with a callback at fixed intervals.
module System.Taffybar.Widgets.PollingBar (
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
import System.Taffybar.Widgets.Util ( backgroundLoop, drawOn )

import System.Taffybar.Widgets.VerticalBar

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
