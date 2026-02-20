-- | Like the vertical bar, but this widget automatically updates
-- itself with a callback at fixed intervals.
module System.Taffybar.Widget.Generic.PollingBar
  ( -- * Types
    VerticalBarHandle,
    BarConfig (..),
    BarDirection (..),

    -- * Constructors and accessors
    pollingBarNew,
    verticalBarFromCallback,
    defaultBarConfig,
  )
where

import Control.Concurrent
import Control.Exception.Enclosed (tryAny)
import Control.Monad.IO.Class
import qualified GI.Gtk
import System.Taffybar.Widget.Generic.VerticalBar
import System.Taffybar.Widget.Util (backgroundLoop)

-- | Construct a bar widget driven directly by a sample callback.
verticalBarFromCallback ::
  (MonadIO m) =>
  BarConfig -> IO Double -> m GI.Gtk.Widget
verticalBarFromCallback cfg action = liftIO $ do
  (drawArea, h) <- verticalBarNew cfg
  _ <- GI.Gtk.onWidgetRealize drawArea $ backgroundLoop $ do
    esample <- tryAny action
    traverse (verticalBarSetPercent h) esample
  return drawArea

-- | Construct a polling bar with a fixed polling interval (seconds).
pollingBarNew ::
  (MonadIO m) =>
  BarConfig -> Double -> IO Double -> m GI.Gtk.Widget
pollingBarNew cfg pollSeconds action =
  liftIO $
    verticalBarFromCallback cfg $
      action <* delay
  where
    delay = threadDelay $ floor (pollSeconds * 1000000)
