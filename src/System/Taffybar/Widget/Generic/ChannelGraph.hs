module System.Taffybar.Widget.Generic.ChannelGraph where

import Control.Concurrent
import Control.Monad
import Control.Monad.Trans
import GI.Gtk
import System.Taffybar.Compat.GtkLibs
import System.Taffybar.Widget.Generic.Graph
import System.Taffybar.Widget.Generic.ChannelWidget

channelGraphNew
  :: MonadIO m
  => GraphConfig -> Chan a -> (a -> IO [Double]) -> m Widget
channelGraphNew config chan sampleBuilder = liftIO $ do
  (graphWidget', graphHandle) <- graphNew config
  graphWidget <- toGIWidget graphWidget'
  channelWidgetNew graphWidget chan $ (>>= graphAddSample graphHandle) . sampleBuilder
