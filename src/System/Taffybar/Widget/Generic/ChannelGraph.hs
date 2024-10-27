module System.Taffybar.Widget.Generic.ChannelGraph where

import BroadcastChan
import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable (traverse_)
import GI.Gtk
import System.Taffybar.Widget.Generic.Graph
import UnliftIO.Concurrent (forkIO, killThread)

-- | Given a 'BroadcastChan' and an action to consume that broadcast chan and
-- turn it into graphable values, build a graph that will update as values are
-- broadcast over the channel.
channelGraphNew
  :: MonadIO m
  => GraphConfig -> BroadcastChan In a -> (a -> IO [Double]) -> m GI.Gtk.Widget
channelGraphNew config chan sampleBuilder = do
  (graphWidget, graphHandle) <- graphNew config
  _ <- onWidgetRealize graphWidget $ do
       ourChan <- newBChanListener chan
       sampleThread <- forkIO $ forever $
         readBChan ourChan >>=
         traverse_ (graphAddSample graphHandle <=< sampleBuilder)
       void $ onWidgetUnrealize graphWidget $ killThread sampleThread
  return graphWidget
