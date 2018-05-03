module System.Taffybar.Widget.Generic.ChannelGraph where

import Control.Concurrent
import Control.Monad
import Control.Monad.Trans
import Graphics.UI.Gtk
import System.Taffybar.Widget.Generic.Graph

channelGraphNew
  :: MonadIO m
  => GraphConfig -> Chan a -> (a -> IO [Double]) -> m Widget
channelGraphNew config chan sampleBuilder = liftIO $ do
  (graphWidget, graphHandle) <- graphNew config
  _ <- on graphWidget realize $ do
       ourChan <- dupChan chan
       sampleThread <- forkIO $ forever $ do
         value <- readChan ourChan
         sampleBuilder value >>= graphAddSample graphHandle
       void $ on graphWidget unrealize $ killThread sampleThread
  return graphWidget
