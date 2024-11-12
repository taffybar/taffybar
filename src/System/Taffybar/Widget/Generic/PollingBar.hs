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

import qualified GI.Gtk
import System.Taffybar.Widget.Util ( backgroundLoop )
import Control.Monad.IO.Class
import UnliftIO.Concurrent ( threadDelay )
import UnliftIO.Exception ( tryAny )

import System.Taffybar.Widget.Generic.VerticalBar

verticalBarFromCallback :: MonadIO m
                        => BarConfig -> IO Double -> m GI.Gtk.Widget
verticalBarFromCallback cfg action = liftIO $ do
  (drawArea, h) <- verticalBarNew cfg
  _ <- GI.Gtk.onWidgetRealize drawArea $ backgroundLoop $ do
      esample <- tryAny action
      traverse (verticalBarSetPercent h) esample
  return drawArea

pollingBarNew :: MonadIO m
              => BarConfig -> Double -> IO Double -> m GI.Gtk.Widget
pollingBarNew cfg pollSeconds action =
  liftIO $
  verticalBarFromCallback cfg $ action <* delay
  where delay = threadDelay $ floor (pollSeconds * 1000000)
