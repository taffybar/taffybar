-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      : System.Taffybar.Widget.NetworkGraph
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan A. Malison
-- Stability   : unstable
-- Portability : unportable
--
-- This module provides a channel based network graph widget.
module System.Taffybar.Widget.NetworkGraph where

import Data.Default (Default (..))
import Data.Foldable (for_)
import qualified Data.Text as T
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
import System.Taffybar.Widget.Util (widgetSetClassGI)

-- | 'NetworkGraphConfig' configures the network graph widget.
data NetworkGraphConfig = NetworkGraphConfig
  { -- | The configuration of the graph itself.
    -- | A tooltip format string, together with the precision that should be used
    -- for numbers in the string.
    networkGraphGraphConfig :: GraphConfig,
    networkGraphTooltipFormat :: Maybe (String, Int),
    -- | A function to scale the y axis of the network config. The default is
    -- `logBase $ 2 ** 32`.
    networkGraphScale :: Double -> Double,
    -- | A filter function that determines whether a given interface will be
    -- included in the network stats.
    interfacesFilter :: String -> Bool
  }

-- | Default configuration paramters for the network graph.
defaultNetworkGraphConfig :: NetworkGraphConfig
defaultNetworkGraphConfig =
  NetworkGraphConfig
    { networkGraphGraphConfig = def,
      networkGraphTooltipFormat = Just (defaultNetFormat, 3),
      networkGraphScale = logBase $ 2 ** 32,
      interfacesFilter = const True
    }

instance Default NetworkGraphConfig where
  def = defaultNetworkGraphConfig

-- | 'networkGraphNew' instantiates a network graph widget from a 'GraphConfig'
-- and a list of interfaces.
networkGraphNew :: GraphConfig -> Maybe [String] -> TaffyIO GI.Gtk.Widget
networkGraphNew config interfaces =
  networkGraphNewWith
    def
      { networkGraphGraphConfig = config,
        interfacesFilter = maybe (const True) (flip elem) interfaces
      }

-- | 'networkGraphNewWith' instantiates a network graph widget from a
-- 'NetworkGraphConfig'.
networkGraphNewWith :: NetworkGraphConfig -> TaffyIO GI.Gtk.Widget
networkGraphNewWith config = do
  NetworkInfoChan chan <- getNetworkChan
  let toSample (up, down) = map (networkGraphScale config . fromRational) [up, down]
      sampleBuilder = return . toSample . networkGraphSpeeds config
  widget <- channelGraphNew (networkGraphGraphConfig config) chan sampleBuilder
  _ <- widgetSetClassGI widget (T.pack "network-graph")
  for_ (networkGraphTooltipFormat config) $ \_ ->
    channelWidgetNew widget chan $ \speedInfo ->
      postGUIASync $ widgetSetTooltipMarkup widget $ networkGraphTooltip config speedInfo
  return widget

networkGraphSpeeds :: NetworkGraphConfig -> [(String, (Rational, Rational))] -> (Rational, Rational)
networkGraphSpeeds config = sumSpeeds . map snd . filter (interfacesFilter config . fst)

networkGraphTooltip :: NetworkGraphConfig -> [(String, (Rational, Rational))] -> Maybe T.Text
networkGraphTooltip config speedInfo = do
  (format, precision) <- networkGraphTooltipFormat config
  let (up, down) = networkGraphSpeeds config speedInfo
  pure $ showInfo format precision (fromRational down, fromRational up)
