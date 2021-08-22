module System.Taffybar.Widget.Generic.ChannelWidget where

import BroadcastChan
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable (traverse_)
import GI.Gtk

-- | Given a widget, a 'BroadcastChan' and a function that consumes the values
-- yielded by the channel that is in 'IO', connect the function to the
-- 'BroadcastChan' on a dedicated haskell thread.
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
