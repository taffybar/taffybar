-- | This is a simple text widget that updates its contents by calling
-- a callback at a set interval.
module System.Taffybar.Widget.Generic.PollingLabel where

import           Control.Concurrent
import           Control.Exception.Enclosed as E
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.Text as T
import           GI.Gtk
import           System.Log.Logger
import           System.Taffybar.Util
import           System.Taffybar.Widget.Util
import           Text.Printf

-- | Create a new widget that updates itself at regular intervals.  The
-- function
--
-- > pollingLabelNew initialString cmd interval
--
-- returns a widget with initial text @initialString@. The widget forks a thread
-- to update its contents every @interval@ seconds. The command should return a
-- string with any HTML entities escaped. This is not checked by the function,
-- since Pango markup shouldn't be escaped. Proper input sanitization is up to
-- the caller.
--
-- If the IO action throws an exception, it will be swallowed and the label will
-- not update until the update interval expires.
pollingLabelNew
  :: MonadIO m
  => Double -- ^ Update interval (in seconds)
  -> IO T.Text -- ^ Command to run to get the input string
  -> m GI.Gtk.Widget
pollingLabelNew interval cmd =
  pollingLabelNewWithTooltip interval $ (, Nothing) <$> cmd

pollingLabelNewWithTooltip
  :: MonadIO m
  => Double -- ^ Update interval (in seconds)
  -> IO (T.Text, Maybe T.Text) -- ^ Command to run to get the input string
  -> m GI.Gtk.Widget
pollingLabelNewWithTooltip interval action =
  pollingLabelWithVariableDelay $ withInterval <$> action
    where withInterval (a, b) = (a, b, interval)

pollingLabelWithVariableDelay
  :: MonadIO m
  => IO (T.Text, Maybe T.Text, Double)
  -> m GI.Gtk.Widget
pollingLabelWithVariableDelay action =
  liftIO $ do
    grid <- gridNew
    label <- labelNew Nothing

    let updateLabel (labelStr, tooltipStr, delay) = do
          postGUIASync $ do
             labelSetMarkup label labelStr
             widgetSetTooltipMarkup label tooltipStr
          logM "System.Taffybar.Widget.Generic.PollingLabel" DEBUG $
               printf "Polling label delay was %s" $ show delay
          return delay
        updateLabelHandlingErrors =
          E.tryAny action >>= either (const $ return 1) updateLabel

    _ <- onWidgetRealize label $ do
      sampleThread <- foreverWithVariableDelay updateLabelHandlingErrors
      void $ onWidgetUnrealize label $ killThread sampleThread

    vFillCenter label
    vFillCenter grid
    containerAdd grid label
    widgetShowAll grid
    toWidget grid
