module System.Taffybar.Widget.NetworkGraph where

import Data.Foldable (traverse_)
import qualified GI.Gtk
import GI.Gtk.Objects.Widget (widgetSetTooltipMarkup)
import System.Taffybar.Context
import System.Taffybar.Hooks
import System.Taffybar.Information.Network
import System.Taffybar.Util (postGUIASync)
import System.Taffybar.Widget.Generic.ChannelGraph
import System.Taffybar.Widget.Generic.ChannelWidget
import System.Taffybar.Widget.Generic.Graph
import System.Taffybar.Widget.Text.NetworkMonitor

data NetworkGraphConfig = NetworkGraphConfig
  { networkGraphGraphConfig :: GraphConfig
  , networkGraphTooltipFormat :: Maybe (String, Int)
  , networkGraphScale :: Double -> Double
  }

defaultNetworkGraphConfig :: NetworkGraphConfig
defaultNetworkGraphConfig = NetworkGraphConfig
  { networkGraphGraphConfig = defaultGraphConfig
  , networkGraphTooltipFormat = Just (defaultNetFormat, 3)
  , networkGraphScale = logBase $ 2 ** 32
  }

networkGraphNew :: GraphConfig -> Maybe [String] -> TaffyIO GI.Gtk.Widget
networkGraphNew config =
  networkGraphNewWith defaultNetworkGraphConfig
                        { networkGraphGraphConfig = config }

networkGraphNewWith :: NetworkGraphConfig -> Maybe [String] -> TaffyIO GI.Gtk.Widget
networkGraphNewWith config interfaces = do
  NetworkInfoChan chan <- getNetworkChan
  let filterFn = maybe (const True) (flip elem) interfaces
      getUpDown = sumSpeeds . map snd . filter (filterFn . fst)
      toSample (up, down) = map (networkGraphScale config . fromRational) [up, down]
      sampleBuilder = return . toSample . getUpDown
  widget <- channelGraphNew (networkGraphGraphConfig config) chan sampleBuilder
  flip traverse_ (networkGraphTooltipFormat config) $ \(format, precision) ->
    channelWidgetNew widget chan $ \speedInfo ->
      let (up, down) = sumSpeeds $ map snd speedInfo
          tooltip = showInfo format precision (fromRational down, fromRational up)
      in postGUIASync $ widgetSetTooltipMarkup widget $ Just tooltip
  return widget
