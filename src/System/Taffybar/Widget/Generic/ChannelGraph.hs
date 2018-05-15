module System.Taffybar.Widget.Generic.ChannelGraph where

import Control.Monad
import Control.Concurrent
import Control.Monad.Trans
import GI.Gtk
import System.Taffybar.Compat.GtkLibs
import System.Taffybar.Widget.Generic.Graph
import System.Taffybar.Widget.Generic.ChannelWidget

channelGraphNew
  :: MonadIO m
  => GraphConfig -> Chan a -> (a -> IO [Double]) -> m Widget
channelGraphNew config chan sampleBuilder = do
  (graphWidget', graphHandle) <- graphNew config
  graphWidget <- toGIWidget graphWidget'
  channelWidgetNew graphWidget chan $
    sampleBuilder >=> graphAddSample graphHandle
