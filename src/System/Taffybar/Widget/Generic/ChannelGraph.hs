module System.Taffybar.Widget.Generic.ChannelGraph where

import Control.Concurrent
import Control.Concurrent.STM.TChan
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.STM (atomically)
import GI.Gtk
import System.Taffybar.Widget.Generic.Graph

-- | Given a broadcast 'TChan' and an action to consume that broadcast chan and
-- turn it into graphable values, build a graph that will update as values are
-- broadcast over the channel.
channelGraphNew
  :: MonadIO m
  => GraphConfig -> TChan a -> (a -> IO [Double]) -> m GI.Gtk.Widget
channelGraphNew config chan sampleBuilder = do
  (graphWidget, graphHandle) <- graphNew config
  _ <- onWidgetRealize graphWidget $ do
       ourChan <- atomically $ dupTChan chan
       sampleThread <- forkIO $ forever $
         atomically (readTChan ourChan) >>=
         (graphAddSample graphHandle <=< sampleBuilder)
       void $ onWidgetUnrealize graphWidget $ killThread sampleThread
  return graphWidget
