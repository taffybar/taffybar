module System.Taffybar.Widget.NetworkGraph where

import Control.Monad.Trans
import Graphics.UI.Gtk
import System.Log.Logger
import System.Taffybar.Compat.GtkLibs
import System.Taffybar.Context
import System.Taffybar.Hooks
import System.Taffybar.Information.Network
import System.Taffybar.Util
import System.Taffybar.Widget.Generic.ChannelGraph
import System.Taffybar.Widget.Generic.Graph

logScale :: Double -> Double -> Double -> Double
logScale base maxValue value =
  logBase base (min value maxValue) / actualMax
    where actualMax = logBase base maxValue

networkGraphNew :: GraphConfig -> Maybe [String] -> TaffyIO Widget
networkGraphNew config interfaces = fromGIWidget =<< do
  NetworkInfoChan chan <- getNetworkChan
  let filterFn = maybe (const True) (flip elem) interfaces
      getUpDown = sumSpeeds . map snd . filter (filterFn . fst)
      toLogScale = logScale 2 (2 ** 32)
      toSample (up, down) = map (toLogScale . fromRational) [up, down]
      sampleBuilder = return . toSample . getUpDown
  channelGraphNew config chan sampleBuilder
