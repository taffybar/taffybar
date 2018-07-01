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
import qualified GI.Gtk
import System.Taffybar.Compat.GtkLibs
import System.Taffybar.Widget.Util ( backgroundLoop, drawOn )

import System.Taffybar.Widget.Generic.VerticalBar

verticalBarFromCallback :: BarConfig -> IO Double -> IO GI.Gtk.Widget
verticalBarFromCallback cfg action = do
  (drawArea_, h) <- verticalBarNew cfg
  drawArea <- fromGIWidget drawArea_
  _ <- drawOn drawArea $
    backgroundLoop $ do
      esample <- tryAny action
      traverse (verticalBarSetPercent h) esample
  return drawArea_

pollingBarNew :: BarConfig -> Double -> IO Double -> IO GI.Gtk.Widget
pollingBarNew cfg pollSeconds action =
  verticalBarFromCallback cfg $ action <* delay
  where delay = threadDelay $ floor (pollSeconds * 1000000)
