-- | A variant of the Graph widget that automatically updates itself
-- with a callback at a fixed interval.
module System.Taffybar.Widget.Generic.PollingGraph (
  -- * Types
  GraphHandle,
  GraphConfig(..),
  GraphDirection(..),
  GraphStyle(..),
  -- * Constructors and accessors
  pollingGraphNew,
  defaultGraphConfig
  ) where

import           Control.Concurrent
import qualified Control.Exception.Enclosed as E
import           Control.Monad
import           Control.Monad.IO.Class
import           GI.Gtk
import           System.Taffybar.Util
import           System.Taffybar.Widget.Generic.Graph

pollingGraphNew
  :: MonadIO m
  => GraphConfig -> Double -> IO [Double] -> m GI.Gtk.Widget
pollingGraphNew cfg pollSeconds action = liftIO $ do
  (graphWidget, graphHandle) <- graphNew cfg

  _ <- onWidgetRealize graphWidget $ do
       sampleThread <- foreverWithDelay pollSeconds $ do
         esample <- E.tryAny action
         case esample of
           Left _ -> return ()
           Right sample -> graphAddSample graphHandle sample
       void $ onWidgetUnrealize graphWidget $ killThread sampleThread

  return graphWidget
