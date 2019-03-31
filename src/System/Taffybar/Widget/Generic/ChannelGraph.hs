module System.Taffybar.Widget.Generic.ChannelGraph where

import BroadcastChan
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable (traverse_)
import GI.Gtk
import System.Taffybar.Widget.Generic.Graph

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
