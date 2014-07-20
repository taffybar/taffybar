-- | Like the vertical bar, but this widget automatically updates
-- itself with a callback at fixed intervals.
module System.Taffybar.Widgets.PollingBar (
  -- * Types
  VerticalBarHandle,
  BarConfig(..),
  BarDirection(..),
  -- * Constructors and accessors
  pollingBarNew,
  defaultBarConfig
  ) where

import Control.Concurrent
import qualified Control.Exception.Enclosed as E
import Control.Monad ( forever )
import Graphics.UI.Gtk

import System.Taffybar.Widgets.VerticalBar

pollingBarNew :: BarConfig -> Double -> IO Double -> IO Widget
pollingBarNew cfg pollSeconds action = do
  (drawArea, h) <- verticalBarNew cfg

  _ <- on drawArea realize $ do
    _ <- forkIO $ forever $ do
      esample <- E.tryAny action
      case esample of
        Left _ -> return ()
        Right sample -> verticalBarSetPercent h sample
      threadDelay $ floor (pollSeconds * 1000000)
    return ()

  return drawArea
