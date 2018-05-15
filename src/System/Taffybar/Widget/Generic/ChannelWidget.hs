module System.Taffybar.Widget.Generic.ChannelWidget where

import Control.Concurrent
import Control.Monad
import Control.Monad.Trans
import GI.Gtk

channelWidgetNew :: (MonadIO m, IsWidget w) => w -> Chan a -> (a -> IO ()) -> m w
channelWidgetNew widget channel updateWidget = do
  void $ onWidgetRealize widget $ do
    ourChan <- dupChan channel
    processingThreadId <- forkIO $ forever $ do
      readChan ourChan >>= updateWidget
    void $ onWidgetUnrealize widget $ killThread processingThreadId
  widgetShowAll widget
  return widget
