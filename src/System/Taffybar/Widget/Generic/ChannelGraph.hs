module System.Taffybar.Widget.Generic.ChannelGraph where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import GI.Gtk
import System.Taffybar.Widget.Generic.Graph

channelGraphNew
  :: MonadIO m
  => GraphConfig -> Chan a -> (a -> IO [Double]) -> m GI.Gtk.Widget
channelGraphNew config chan sampleBuilder = do
  (graphWidget, graphHandle) <- graphNew config
  _ <- onWidgetRealize graphWidget $ do
       ourChan <- dupChan chan
       sampleThread <- forkIO $ forever $ do
         value <- readChan ourChan
         sampleBuilder value >>= graphAddSample graphHandle
       void $ onWidgetUnrealize graphWidget $ killThread sampleThread
  return graphWidget
