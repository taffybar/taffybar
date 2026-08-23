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
import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Reader (ask, runReaderT)
import Data.Foldable (traverse_)
import qualified GI.Gtk
import System.Taffybar.Context (TaffyIO)
import System.Taffybar.Information.Wakeup (taffyForeverWithDelay)
import System.Taffybar.Widget.Generic.VerticalBar
import System.Taffybar.Widget.Util (manageWidgetThreads)

-- | Construct a bar widget driven directly by a sample callback.
verticalBarFromCallback ::
  (MonadIO m) =>
  BarConfig -> IO Double -> m GI.Gtk.Widget
verticalBarFromCallback cfg action = liftIO $ do
  (drawArea, h) <- verticalBarNew cfg
  manageWidgetThreads drawArea $ do
    sampleThread <- forkIO $ forever $ do
      esample <- tryAny action
      traverse (verticalBarSetPercent h) esample
    return [sampleThread]
  return drawArea

-- | Construct a polling bar with a fixed polling interval (seconds).
pollingBarNew ::
  BarConfig -> Double -> IO Double -> TaffyIO GI.Gtk.Widget
pollingBarNew cfg pollSeconds action = do
  context <- ask
  (drawArea, h) <- verticalBarNew cfg

  liftIO $
    manageWidgetThreads drawArea $ do
      sampleThread <-
        runReaderT
          ( taffyForeverWithDelay pollSeconds $
              liftIO $ do
                esample <- tryAny action
                traverse_ (verticalBarSetPercent h) esample
          )
          context
      return [sampleThread]

  return drawArea
