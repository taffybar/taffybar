-- | A variant of the Graph widget that automatically updates itself
-- with a callback at a fixed interval.
module System.Taffybar.Widget.Generic.PollingGraph
  ( -- * Types
    GraphHandle,
    GraphConfig (..),
    GraphDirection (..),
    GraphStyle (..),

    -- * Constructors and accessors
    pollingGraphNew,
    pollingGraphNewWithTooltip,
    defaultGraphConfig,
  )
where

import Control.Concurrent
import qualified Control.Exception.Enclosed as E
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Text as T
import GI.Gtk
import System.Taffybar.Util
import System.Taffybar.Widget.Generic.Graph

pollingGraphNewWithTooltip ::
  (MonadIO m) =>
  GraphConfig -> Double -> IO ([Double], Maybe T.Text) -> m GI.Gtk.Widget
pollingGraphNewWithTooltip cfg pollSeconds action = liftIO $ do
  (graphWidget, graphHandle) <- graphNew cfg

  _ <- onWidgetRealize graphWidget $ do
    sampleThread <- foreverWithDelay pollSeconds $ do
      esample <- E.tryAny action
      case esample of
        Left _ -> return ()
        Right (sample, tooltipStr) -> do
          graphAddSample graphHandle sample
          widgetSetTooltipMarkup graphWidget tooltipStr
    void $ onWidgetUnrealize graphWidget $ killThread sampleThread

  return graphWidget

pollingGraphNew ::
  (MonadIO m) =>
  GraphConfig -> Double -> IO [Double] -> m GI.Gtk.Widget
pollingGraphNew cfg pollSeconds action =
  pollingGraphNewWithTooltip cfg pollSeconds $ fmap (,Nothing) action
