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
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ask, runReaderT)
import qualified Data.Text as T
import GI.Gtk
import System.Taffybar.Context (TaffyIO)
import System.Taffybar.Information.Wakeup (taffyForeverWithDelay)
import System.Taffybar.Widget.Generic.Graph

-- | Construct a polling graph whose callback also supplies tooltip text.
pollingGraphNewWithTooltip ::
  GraphConfig -> Double -> IO ([Double], Maybe T.Text) -> TaffyIO GI.Gtk.Widget
pollingGraphNewWithTooltip cfg pollSeconds action = do
  context <- ask
  (graphWidget, graphHandle) <- graphNew cfg

  liftIO $ do
    _ <- onWidgetRealize graphWidget $ do
      sampleThread <-
        runReaderT
          ( taffyForeverWithDelay pollSeconds $
              liftIO $ do
                esample <- E.tryAny action
                case esample of
                  Left _ -> return ()
                  Right (sample, tooltipStr) -> do
                    graphAddSample graphHandle sample
                    widgetSetTooltipMarkup graphWidget tooltipStr
          )
          context
      void $ onWidgetUnrealize graphWidget $ killThread sampleThread
    return ()

  return graphWidget

-- | Construct a polling graph from a fixed-interval sample callback.
pollingGraphNew ::
  GraphConfig -> Double -> IO [Double] -> TaffyIO GI.Gtk.Widget
pollingGraphNew cfg pollSeconds action =
  pollingGraphNewWithTooltip cfg pollSeconds $ fmap (,Nothing) action
