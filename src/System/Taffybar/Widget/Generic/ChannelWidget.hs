-- | Connect widget updates to a broadcast 'TChan' on a dedicated worker thread.
module System.Taffybar.Widget.Generic.ChannelWidget where

import Control.Concurrent
import Control.Concurrent.STM.TChan
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.STM (atomically)
import GI.Gtk

-- | Given a widget, a broadcast 'TChan' and a function that consumes the values
-- yielded by the channel that is in 'IO', connect the function to the
-- 'TChan' on a dedicated haskell thread.
channelWidgetNew ::
  (MonadIO m, IsWidget w) =>
  w -> TChan a -> (a -> IO ()) -> m w
channelWidgetNew widget channel updateWidget = do
  void $ onWidgetRealize widget $ do
    ourChan <- atomically $ dupTChan channel
    processingThreadId <-
      forkIO $
        forever $
          atomically (readTChan ourChan) >>= updateWidget
    void $ onWidgetUnrealize widget $ killThread processingThreadId
  widgetShowAll widget
  return widget
