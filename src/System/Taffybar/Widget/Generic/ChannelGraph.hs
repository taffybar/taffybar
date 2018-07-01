module System.Taffybar.Widget.Generic.ChannelGraph where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Graphics.UI.Gtk
import qualified GI.Gtk
import System.Taffybar.Compat.GtkLibs
import System.Taffybar.Widget.Generic.Graph

channelGraphNew
  :: MonadIO m
  => GraphConfig -> Chan a -> (a -> IO [Double]) -> m GI.Gtk.Widget
channelGraphNew config chan sampleBuilder = liftIO $ do
  (graphWidget, graphHandle) <- graphNew config
  _ <- on graphWidget realize $ do
       ourChan <- dupChan chan
       sampleThread <- forkIO $ forever $ do
         value <- readChan ourChan
         sampleBuilder value >>= graphAddSample graphHandle
       void $ on graphWidget unrealize $ killThread sampleThread
  toGIWidget graphWidget
