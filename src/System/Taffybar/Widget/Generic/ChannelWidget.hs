module System.Taffybar.Widget.Generic.ChannelWidget where

import BroadcastChan
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable (traverse_)
import GI.Gtk

channelWidgetNew ::
  (MonadIO m, IsWidget w) =>
  w -> BroadcastChan In a -> (a -> IO ()) -> m w
channelWidgetNew widget channel updateWidget = do
  void $ onWidgetRealize widget $ do
    ourChan <- newBChanListener channel
    processingThreadId <- forkIO $ forever $
      readBChan ourChan >>= traverse_ updateWidget
    void $ onWidgetUnrealize widget $ killThread processingThreadId
  widgetShowAll widget
  return widget
