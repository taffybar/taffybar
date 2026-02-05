{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      : System.Taffybar.Widget.Wifi
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan A. Malison
-- Stability   : unstable
-- Portability : unportable
--
-- This module provides a simple WiFi status widget backed by NetworkManager's
-- DBus API.
-----------------------------------------------------------------------------
module System.Taffybar.Widget.Wifi
  ( WifiWidgetConfig(..)
  , defaultWifiWidgetConfig
  , wifiLabelNew
  , wifiLabelNewWith
  ) where

import           Control.Applicative ((<|>))
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Data.Default (Default(..))
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified GI.GLib as G
import           GI.Gtk
import           System.Taffybar.Context
import           System.Taffybar.Information.NetworkManager
import           System.Taffybar.Util
import           System.Taffybar.Widget.Generic.ChannelWidget
import           Text.StringTemplate

data WifiWidgetConfig = WifiWidgetConfig
  { wifiConnectedFormat :: String
  , wifiDisconnectedFormat :: String
  , wifiDisabledFormat :: String
  , wifiUnknownFormat :: String
  , wifiTooltipFormat :: Maybe String
  }

defaultWifiWidgetConfig :: WifiWidgetConfig
defaultWifiWidgetConfig =
  WifiWidgetConfig
    { wifiConnectedFormat = "wifi: $ssid$ $strength$%"
    , wifiDisconnectedFormat = "wifi: disconnected"
    , wifiDisabledFormat = "wifi: off"
    , wifiUnknownFormat = "wifi: unknown"
    , wifiTooltipFormat =
        Just "SSID: $ssid$\nStrength: $strength$%\nConnection: $connection$\nState: $state$"
    }

instance Default WifiWidgetConfig where
  def = defaultWifiWidgetConfig

wifiLabelNew :: TaffyIO Widget
wifiLabelNew = wifiLabelNewWith defaultWifiWidgetConfig

wifiLabelNewWith :: WifiWidgetConfig -> TaffyIO Widget
wifiLabelNewWith config = do
  chan <- getWifiInfoChan
  ctx <- ask
  liftIO $ do
    label <- labelNew Nothing
    let updateWidget info = do
          (labelText, tooltipText) <- formatWifiWidget config info
          postGUIASync $ do
            labelSetMarkup label labelText
            widgetSetTooltipMarkup label tooltipText
    void $ onWidgetRealize label $
      runReaderT getWifiInfoState ctx >>= updateWidget
    toWidget =<< channelWidgetNew label chan updateWidget

formatWifiWidget
  :: WifiWidgetConfig
  -> WifiInfo
  -> IO (T.Text, Maybe T.Text)
formatWifiWidget config info = do
  attrs <- buildAttrs info
  let labelTemplate = case wifiState info of
        WifiConnected -> wifiConnectedFormat config
        WifiDisconnected -> wifiDisconnectedFormat config
        WifiDisabled -> wifiDisabledFormat config
        WifiUnknown -> wifiUnknownFormat config
      labelText = renderTemplate labelTemplate attrs
      tooltipText = fmap (`renderTemplate` attrs) (wifiTooltipFormat config)
  return (T.pack labelText, T.pack <$> tooltipText)

renderTemplate :: String -> [(String, String)] -> String
renderTemplate template attrs = render $ setManyAttrib attrs (newSTMP template)

buildAttrs :: WifiInfo -> IO [(String, String)]
buildAttrs info = do
  let
    displayName =
      fromMaybe "" (wifiSsid info <|> wifiConnectionId info)
    ssidText = if T.null displayName then "unknown" else displayName
    strengthText = maybe "?" show (wifiStrength info)
    stateText = wifiStateText (wifiState info)
    connectionText = fromMaybe "" (wifiConnectionId info)
  ssid <- escapeText ssidText
  strength <- escapeText (T.pack strengthText)
  state <- escapeText (T.pack stateText)
  connection <- escapeText connectionText
  return
    [ ("ssid", ssid)
    , ("strength", strength)
    , ("state", state)
    , ("connection", connection)
    ]

escapeText :: T.Text -> IO String
escapeText input = T.unpack <$> G.markupEscapeText input (-1)

wifiStateText :: WifiState -> String
wifiStateText WifiDisabled = "disabled"
wifiStateText WifiDisconnected = "disconnected"
wifiStateText WifiConnected = "connected"
wifiStateText WifiUnknown = "unknown"
