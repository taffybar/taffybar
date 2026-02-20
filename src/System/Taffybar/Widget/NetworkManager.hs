{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      : System.Taffybar.Widget.NetworkManager
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan A. Malison
-- Stability   : unstable
-- Portability : unportable
--
-- Network widgets backed by NetworkManager's DBus API.
module System.Taffybar.Widget.NetworkManager
  ( WifiWidgetConfig (..),
    defaultWifiWidgetConfig,
    networkManagerWifiLabelNew,
    networkManagerWifiLabelNewWith,
    NetworkManagerWifiIconConfig (..),
    defaultNetworkManagerWifiIconConfig,
    networkManagerWifiIconNew,
    networkManagerWifiIconNewWith,
    networkManagerWifiNew,
    networkManagerWifiNewWith,
    -- Wifi text icon (nerd font label)
    networkManagerWifiTextIconNew,
    networkManagerWifiTextIconNewWith,
    -- Wifi combined icon-label
    networkManagerWifiIconLabelNew,
    networkManagerWifiIconLabelNewWith,
    NetworkWidgetConfig (..),
    defaultNetworkWidgetConfig,
    networkManagerNetworkLabelNew,
    networkManagerNetworkLabelNewWith,
    NetworkManagerNetworkIconConfig (..),
    defaultNetworkManagerNetworkIconConfig,
    networkManagerNetworkIconNew,
    networkManagerNetworkIconNewWith,
    networkManagerNetworkNew,
    networkManagerNetworkNewWith,
    -- Network text icon (nerd font label)
    networkManagerNetworkTextIconNew,
    networkManagerNetworkTextIconNewWith,
    -- Network combined icon-label
    networkManagerNetworkIconLabelNew,
    networkManagerNetworkIconLabelNewWith,
  )
where

import Control.Applicative ((<|>))
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Default (Default (..))
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Int (Int32)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified GI.GLib as G
import GI.Gtk
import System.Taffybar.Context
import System.Taffybar.Information.NetworkManager
import System.Taffybar.Util
import System.Taffybar.Widget.Generic.ChannelWidget
import System.Taffybar.Widget.Generic.ScalingImage (scalingImage)
import System.Taffybar.Widget.Util (buildIconLabelBox)
import Text.StringTemplate

-- | Formatting configuration for WiFi label widgets.
data WifiWidgetConfig = WifiWidgetConfig
  { wifiConnectedFormat :: String,
    wifiDisconnectedFormat :: String,
    wifiDisabledFormat :: String,
    wifiUnknownFormat :: String,
    wifiTooltipFormat :: Maybe String
  }

-- | Default 'WifiWidgetConfig'.
defaultWifiWidgetConfig :: WifiWidgetConfig
defaultWifiWidgetConfig =
  WifiWidgetConfig
    { wifiConnectedFormat = "$ssid$ $strength$%",
      wifiDisconnectedFormat = "disconnected",
      wifiDisabledFormat = "off",
      wifiUnknownFormat = "unknown",
      wifiTooltipFormat =
        Just "SSID: $ssid$\nStrength: $strength$%\nConnection: $connection$\nState: $state$"
    }

instance Default WifiWidgetConfig where
  def = defaultWifiWidgetConfig

-- | Build a WiFi status label widget with default formatting.
networkManagerWifiLabelNew :: TaffyIO Widget
networkManagerWifiLabelNew = networkManagerWifiLabelNewWith defaultWifiWidgetConfig

-- | Build a WiFi status label widget with custom formatting.
networkManagerWifiLabelNewWith :: WifiWidgetConfig -> TaffyIO Widget
networkManagerWifiLabelNewWith config = do
  chan <- getWifiInfoChan
  ctx <- ask
  liftIO $ do
    label <- labelNew Nothing
    let updateWidget info = do
          (labelText, tooltipText) <- formatWifiWidget config info
          postGUIASync $ do
            labelSetMarkup label labelText
            widgetSetTooltipMarkup label tooltipText
    void $
      onWidgetRealize label $
        runReaderT getWifiInfoState ctx >>= updateWidget
    toWidget =<< channelWidgetNew label chan updateWidget

-- | Icon configuration for WiFi icon widgets.
data NetworkManagerWifiIconConfig = NetworkManagerWifiIconConfig
  { wifiIconNone :: String,
    wifiIconWeak :: String,
    wifiIconOk :: String,
    wifiIconGood :: String,
    wifiIconExcellent :: String,
    wifiIconDisconnected :: String,
    wifiIconDisabled :: String,
    wifiIconUnknown :: String,
    wifiIconTooltipFormat :: Maybe String
  }

-- | Default 'NetworkManagerWifiIconConfig'.
defaultNetworkManagerWifiIconConfig :: NetworkManagerWifiIconConfig
defaultNetworkManagerWifiIconConfig =
  NetworkManagerWifiIconConfig
    { wifiIconNone = "network-wireless-signal-none-symbolic",
      wifiIconWeak = "network-wireless-signal-weak-symbolic",
      wifiIconOk = "network-wireless-signal-ok-symbolic",
      wifiIconGood = "network-wireless-signal-good-symbolic",
      wifiIconExcellent = "network-wireless-signal-excellent-symbolic",
      wifiIconDisconnected = "network-wireless-offline-symbolic",
      wifiIconDisabled = "network-wireless-disabled-symbolic",
      wifiIconUnknown = "network-wireless-symbolic",
      wifiIconTooltipFormat =
        Just "SSID: $ssid$\nStrength: $strength$%\nConnection: $connection$\nState: $state$"
    }

themeLoadFlags :: [IconLookupFlags]
themeLoadFlags = [IconLookupFlagsGenericFallback, IconLookupFlagsUseBuiltin]

-- | Build a WiFi icon widget with default icon names and formatting.
networkManagerWifiIconNew :: TaffyIO Widget
networkManagerWifiIconNew = networkManagerWifiIconNewWith defaultNetworkManagerWifiIconConfig

-- | Build a WiFi icon widget with custom icon names and tooltip formatting.
networkManagerWifiIconNewWith :: NetworkManagerWifiIconConfig -> TaffyIO Widget
networkManagerWifiIconNewWith config = do
  chan <- getWifiInfoChan
  ctx <- ask
  defaultTheme <- liftIO iconThemeGetDefault
  initialInfo <- liftIO $ runReaderT getWifiInfoState ctx
  infoVar <- liftIO $ newMVar initialInfo
  imageWidgetRef <- liftIO $ newIORef (error "imageWidget not initialised")
  let setIconForSize size = do
        iw <- readIORef imageWidgetRef
        styleCtx <- widgetGetStyleContext iw
        info <- readMVar infoVar
        let iconNames = wifiIconCandidates config info
        iconInfo <- lookupFirstIcon defaultTheme size iconNames
        traverse (extractPixbuf styleCtx) iconInfo
      extractPixbuf styleCtx iconInfo =
        fst <$> iconInfoLoadSymbolicForContext iconInfo styleCtx
  (imageWidget, updateImage) <- scalingImage setIconForSize OrientationHorizontal
  liftIO $ do
    writeIORef imageWidgetRef imageWidget
    let updateWidget info = do
          _ <- swapMVar infoVar info
          (_, tooltipText) <- formatWifiWidget (iconTooltipAsLabelConfig config) info
          postGUIASync $ do
            widgetSetTooltipMarkup imageWidget tooltipText
            updateImage
    void $ onWidgetRealize imageWidget $ updateWidget initialInfo
    toWidget =<< channelWidgetNew imageWidget chan updateWidget

iconTooltipAsLabelConfig :: NetworkManagerWifiIconConfig -> WifiWidgetConfig
iconTooltipAsLabelConfig cfg =
  defaultWifiWidgetConfig {wifiTooltipFormat = wifiIconTooltipFormat cfg}

lookupFirstIcon :: IconTheme -> Int32 -> [String] -> IO (Maybe IconInfo)
lookupFirstIcon _ _ [] = return Nothing
lookupFirstIcon theme size (name : names) = do
  info <- iconThemeLookupIcon theme (T.pack name) size themeLoadFlags
  case info of
    Just _ -> return info
    Nothing -> lookupFirstIcon theme size names

wifiIconCandidates :: NetworkManagerWifiIconConfig -> WifiInfo -> [String]
wifiIconCandidates cfg info =
  case wifiState info of
    WifiDisabled ->
      [ wifiIconDisabled cfg,
        wifiIconDisconnected cfg,
        wifiIconNone cfg
      ]
    WifiDisconnected ->
      [ wifiIconDisconnected cfg,
        wifiIconNone cfg
      ]
    WifiUnknown ->
      [ wifiIconUnknown cfg,
        wifiIconNone cfg
      ]
    WifiConnected ->
      strengthIconName cfg (wifiStrength info)
        : [ wifiIconGood cfg,
            wifiIconOk cfg,
            wifiIconWeak cfg,
            wifiIconNone cfg
          ]

strengthIconName :: NetworkManagerWifiIconConfig -> Maybe Int -> String
strengthIconName cfg strength =
  case strength of
    Nothing -> wifiIconNone cfg
    Just s
      | s < 20 -> wifiIconWeak cfg
      | s < 40 -> wifiIconOk cfg
      | s < 70 -> wifiIconGood cfg
      | otherwise -> wifiIconExcellent cfg

-- | Build a combined WiFi icon and label widget with default configs.
networkManagerWifiNew :: TaffyIO Widget
networkManagerWifiNew = networkManagerWifiNewWith defaultWifiWidgetConfig defaultNetworkManagerWifiIconConfig

-- | Build a combined WiFi icon and label widget with custom configs.
networkManagerWifiNewWith ::
  WifiWidgetConfig ->
  NetworkManagerWifiIconConfig ->
  TaffyIO Widget
networkManagerWifiNewWith labelCfg iconCfg = do
  iconWidget <- networkManagerWifiIconNewWith iconCfg
  labelWidget <- networkManagerWifiLabelNewWith labelCfg
  liftIO $ do
    box <- boxNew OrientationHorizontal 0
    containerAdd box iconWidget
    containerAdd box labelWidget
    widgetShowAll box
    toWidget box

formatWifiWidget ::
  WifiWidgetConfig ->
  WifiInfo ->
  IO (T.Text, Maybe T.Text)
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
  let displayName =
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
    [ ("ssid", ssid),
      ("strength", strength),
      ("state", state),
      ("connection", connection)
    ]

escapeText :: T.Text -> IO String
escapeText input = T.unpack <$> G.markupEscapeText input (-1)

wifiStateText :: WifiState -> String
wifiStateText WifiDisabled = "disabled"
wifiStateText WifiDisconnected = "disconnected"
wifiStateText WifiConnected = "connected"
wifiStateText WifiUnknown = "unknown"

-- | A text "icon" intended for label widgets. Uses Font Awesome-ish codepoints.
wifiTextIcon :: WifiInfo -> T.Text
wifiTextIcon info =
  case wifiState info of
    WifiConnected ->
      T.pack "\xF1EB" -- 
    WifiDisconnected -> T.pack "\xF05E" -- 
    WifiDisabled -> T.pack "\xF05E" -- 
    WifiUnknown -> T.pack "\xF059" -- 

-- Network (WiFi + wired + vpn + disconnected)

-- | Formatting configuration for generic network label widgets.
data NetworkWidgetConfig = NetworkWidgetConfig
  { networkWifiFormat :: String,
    networkWiredFormat :: String,
    networkVpnFormat :: String,
    networkOtherFormat :: String,
    networkDisconnectedFormat :: String,
    networkWifiDisabledFormat :: String,
    networkUnknownFormat :: String,
    networkTooltipFormat :: Maybe String
  }

-- | Default 'NetworkWidgetConfig'.
defaultNetworkWidgetConfig :: NetworkWidgetConfig
defaultNetworkWidgetConfig =
  NetworkWidgetConfig
    { networkWifiFormat = "$ssid$ $strength$%",
      networkWiredFormat = "$connection$",
      networkVpnFormat = "$connection$",
      networkOtherFormat = "$type$ $connection$",
      networkDisconnectedFormat = "disconnected",
      networkWifiDisabledFormat = "disconnected (wifi off)",
      networkUnknownFormat = "unknown",
      networkTooltipFormat =
        Just "Type: $type$\nConnection: $connection$\nSSID: $ssid$\nStrength: $strength$%\nState: $state$"
    }

instance Default NetworkWidgetConfig where
  def = defaultNetworkWidgetConfig

-- | Build a network status label widget with default formatting.
networkManagerNetworkLabelNew :: TaffyIO Widget
networkManagerNetworkLabelNew =
  networkManagerNetworkLabelNewWith defaultNetworkWidgetConfig

-- | Build a network status label widget with custom formatting.
networkManagerNetworkLabelNewWith :: NetworkWidgetConfig -> TaffyIO Widget
networkManagerNetworkLabelNewWith config = do
  chan <- getNetworkInfoChan
  ctx <- ask
  liftIO $ do
    label <- labelNew Nothing
    let updateWidget info = do
          (labelText, tooltipText) <- formatNetworkWidget config info
          postGUIASync $ do
            labelSetMarkup label labelText
            widgetSetTooltipMarkup label tooltipText
    void $
      onWidgetRealize label $
        runReaderT getNetworkInfoState ctx >>= updateWidget
    toWidget =<< channelWidgetNew label chan updateWidget

-- | Icon configuration for generic network icon widgets.
data NetworkManagerNetworkIconConfig = NetworkManagerNetworkIconConfig
  { netIconWired :: String,
    netIconVpn :: String,
    netIconOther :: String,
    netIconDisconnected :: String,
    netIconUnknown :: String,
    netIconWifiDisabled :: String,
    netWifiIconNone :: String,
    netWifiIconWeak :: String,
    netWifiIconOk :: String,
    netWifiIconGood :: String,
    netWifiIconExcellent :: String,
    netIconTooltipFormat :: Maybe String
  }

-- | Default 'NetworkManagerNetworkIconConfig'.
defaultNetworkManagerNetworkIconConfig :: NetworkManagerNetworkIconConfig
defaultNetworkManagerNetworkIconConfig =
  NetworkManagerNetworkIconConfig
    { netIconWired = "network-wired-symbolic",
      netIconVpn = "network-vpn-symbolic",
      netIconOther = "network-transmit-receive-symbolic",
      netIconDisconnected = "network-offline-symbolic",
      netIconUnknown = "network-error-symbolic",
      netIconWifiDisabled = "network-wireless-disabled-symbolic",
      netWifiIconNone = "network-wireless-signal-none-symbolic",
      netWifiIconWeak = "network-wireless-signal-weak-symbolic",
      netWifiIconOk = "network-wireless-signal-ok-symbolic",
      netWifiIconGood = "network-wireless-signal-good-symbolic",
      netWifiIconExcellent = "network-wireless-signal-excellent-symbolic",
      netIconTooltipFormat =
        Just "Type: $type$\nConnection: $connection$\nSSID: $ssid$\nStrength: $strength$%\nState: $state$"
    }

-- | Build a network icon widget with default icon names and formatting.
networkManagerNetworkIconNew :: TaffyIO Widget
networkManagerNetworkIconNew =
  networkManagerNetworkIconNewWith defaultNetworkManagerNetworkIconConfig

-- | Build a network icon widget with custom icon names and tooltip formatting.
networkManagerNetworkIconNewWith :: NetworkManagerNetworkIconConfig -> TaffyIO Widget
networkManagerNetworkIconNewWith config = do
  chan <- getNetworkInfoChan
  ctx <- ask
  defaultTheme <- liftIO iconThemeGetDefault
  initialInfo <- liftIO $ runReaderT getNetworkInfoState ctx
  infoVar <- liftIO $ newMVar initialInfo
  imageWidgetRef <- liftIO $ newIORef (error "imageWidget not initialised")
  let setIconForSize size = do
        iw <- readIORef imageWidgetRef
        styleCtx <- widgetGetStyleContext iw
        info <- readMVar infoVar
        let iconNames = networkIconCandidates config info
        iconInfo <- lookupFirstIcon defaultTheme size iconNames
        traverse (extractPixbuf styleCtx) iconInfo
      extractPixbuf styleCtx iconInfo =
        fst <$> iconInfoLoadSymbolicForContext iconInfo styleCtx
  (imageWidget, updateImage) <- scalingImage setIconForSize OrientationHorizontal
  liftIO $ do
    writeIORef imageWidgetRef imageWidget
    let updateWidget info = do
          _ <- swapMVar infoVar info
          (_, tooltipText) <- formatNetworkWidget (iconTooltipAsNetworkLabelConfig config) info
          postGUIASync $ do
            widgetSetTooltipMarkup imageWidget tooltipText
            updateImage
    void $ onWidgetRealize imageWidget $ updateWidget initialInfo
    toWidget =<< channelWidgetNew imageWidget chan updateWidget

iconTooltipAsNetworkLabelConfig :: NetworkManagerNetworkIconConfig -> NetworkWidgetConfig
iconTooltipAsNetworkLabelConfig cfg =
  defaultNetworkWidgetConfig {networkTooltipFormat = netIconTooltipFormat cfg}

networkIconCandidates :: NetworkManagerNetworkIconConfig -> NetworkInfo -> [String]
networkIconCandidates cfg info =
  case networkState info of
    NetworkUnknown ->
      [ netIconUnknown cfg,
        netIconOther cfg,
        netIconDisconnected cfg
      ]
    NetworkDisconnected ->
      case networkWirelessEnabled info of
        Just False ->
          [ netIconWifiDisabled cfg,
            netIconDisconnected cfg
          ]
        _ ->
          [ netIconDisconnected cfg,
            netIconOther cfg
          ]
    NetworkConnected ->
      case networkType info of
        Just NetworkWired ->
          [ netIconWired cfg,
            netIconOther cfg
          ]
        Just NetworkVpn ->
          [ netIconVpn cfg,
            netIconOther cfg
          ]
        Just NetworkWifi ->
          strengthToWifiIcon cfg (networkStrength info)
            : [ netWifiIconGood cfg,
                netWifiIconOk cfg,
                netWifiIconWeak cfg,
                netWifiIconNone cfg
              ]
        Just (NetworkOther _) ->
          [ netIconOther cfg,
            netIconWired cfg
          ]
        Nothing ->
          [ netIconOther cfg,
            netIconWired cfg,
            netIconDisconnected cfg
          ]

strengthToWifiIcon :: NetworkManagerNetworkIconConfig -> Maybe Int -> String
strengthToWifiIcon cfg strength =
  case strength of
    Nothing -> netWifiIconNone cfg
    Just s
      | s < 20 -> netWifiIconWeak cfg
      | s < 40 -> netWifiIconOk cfg
      | s < 70 -> netWifiIconGood cfg
      | otherwise -> netWifiIconExcellent cfg

-- | Build a combined network icon and label widget with default configs.
networkManagerNetworkNew :: TaffyIO Widget
networkManagerNetworkNew =
  networkManagerNetworkNewWith defaultNetworkWidgetConfig defaultNetworkManagerNetworkIconConfig

-- | Build a combined network icon and label widget with custom configs.
networkManagerNetworkNewWith ::
  NetworkWidgetConfig ->
  NetworkManagerNetworkIconConfig ->
  TaffyIO Widget
networkManagerNetworkNewWith labelCfg iconCfg = do
  iconWidget <- networkManagerNetworkIconNewWith iconCfg
  labelWidget <- networkManagerNetworkLabelNewWith labelCfg
  liftIO $ do
    box <- boxNew OrientationHorizontal 0
    containerAdd box iconWidget
    containerAdd box labelWidget
    widgetShowAll box
    toWidget box

formatNetworkWidget ::
  NetworkWidgetConfig ->
  NetworkInfo ->
  IO (T.Text, Maybe T.Text)
formatNetworkWidget config info = do
  attrs <- buildNetworkAttrs info
  let labelTemplate =
        case networkState info of
          NetworkConnected ->
            case networkType info of
              Just NetworkWifi -> networkWifiFormat config
              Just NetworkWired -> networkWiredFormat config
              Just NetworkVpn -> networkVpnFormat config
              Just (NetworkOther _) -> networkOtherFormat config
              Nothing -> networkOtherFormat config
          NetworkDisconnected ->
            case networkWirelessEnabled info of
              Just False -> networkWifiDisabledFormat config
              _ -> networkDisconnectedFormat config
          NetworkUnknown -> networkUnknownFormat config
      labelText = renderTemplate labelTemplate attrs
      tooltipText = fmap (`renderTemplate` attrs) (networkTooltipFormat config)
  return (T.pack labelText, T.pack <$> tooltipText)

buildNetworkAttrs :: NetworkInfo -> IO [(String, String)]
buildNetworkAttrs info = do
  let rawConnText = fromMaybe "" (networkConnectionId info)
      connText = if T.null rawConnText then "unknown" else rawConnText
      typeText = networkTypeText (networkType info)
      displaySsid = fromMaybe "" (networkSsid info)
      ssidText = if T.null displaySsid then "unknown" else displaySsid
      strengthText = maybe "?" show (networkStrength info)
      stateText = networkStateText (networkState info)
  conn <- escapeText connText
  typ <- escapeText typeText
  ssid <- escapeText ssidText
  strength <- escapeText (T.pack strengthText)
  state <- escapeText (T.pack stateText)
  return
    [ ("connection", conn),
      ("type", typ),
      ("ssid", ssid),
      ("strength", strength),
      ("state", state)
    ]

networkTypeText :: Maybe NetworkType -> T.Text
networkTypeText Nothing = "unknown"
networkTypeText (Just NetworkWifi) = "wifi"
networkTypeText (Just NetworkWired) = "wired"
networkTypeText (Just NetworkVpn) = "vpn"
networkTypeText (Just (NetworkOther t)) = t

networkStateText :: NetworkState -> String
networkStateText NetworkConnected = "connected"
networkStateText NetworkDisconnected = "disconnected"
networkStateText NetworkUnknown = "unknown"

networkTextIcon :: NetworkInfo -> T.Text
networkTextIcon info =
  case networkState info of
    NetworkUnknown -> T.pack "\xF059" -- 
    NetworkDisconnected ->
      case networkWirelessEnabled info of
        Just False -> T.pack "\xF05E\xF1EB" --  (wifi off-ish)
        _ -> T.pack "\xF05E" -- 
    NetworkConnected ->
      case networkType info of
        Just NetworkWifi ->
          T.pack "\xF1EB" -- 
        Just NetworkWired -> T.pack "\xF1E6" -- 
        Just NetworkVpn -> T.pack "\xF023" -- 
        Just (NetworkOther _) -> T.pack "\xF0AC" -- 
        Nothing -> T.pack "\xF0AC" -- 

-- Wifi text icon

-- | Build a WiFi text-icon label widget with default formatting config.
networkManagerWifiTextIconNew :: TaffyIO Widget
networkManagerWifiTextIconNew =
  networkManagerWifiTextIconNewWith defaultWifiWidgetConfig

-- | Build a WiFi text-icon label widget.
networkManagerWifiTextIconNewWith :: WifiWidgetConfig -> TaffyIO Widget
networkManagerWifiTextIconNewWith _config = do
  chan <- getWifiInfoChan
  ctx <- ask
  liftIO $ do
    label <- labelNew Nothing
    let updateIcon info = do
          let iconText = wifiTextIcon info
          postGUIASync $ labelSetText label iconText
    void $
      onWidgetRealize label $
        runReaderT getWifiInfoState ctx >>= updateIcon
    toWidget =<< channelWidgetNew label chan updateIcon

-- Wifi icon-label

-- | Build a combined WiFi text-icon and label widget with default formatting.
networkManagerWifiIconLabelNew :: TaffyIO Widget
networkManagerWifiIconLabelNew =
  networkManagerWifiIconLabelNewWith defaultWifiWidgetConfig

-- | Build a combined WiFi text-icon and label widget.
networkManagerWifiIconLabelNewWith :: WifiWidgetConfig -> TaffyIO Widget
networkManagerWifiIconLabelNewWith config = do
  iconWidget <- networkManagerWifiTextIconNewWith config
  labelWidget <- networkManagerWifiLabelNewWith config
  liftIO $ buildIconLabelBox iconWidget labelWidget

-- Network text icon

-- | Build a network text-icon label widget with default formatting config.
networkManagerNetworkTextIconNew :: TaffyIO Widget
networkManagerNetworkTextIconNew =
  networkManagerNetworkTextIconNewWith defaultNetworkWidgetConfig

-- | Build a network text-icon label widget.
networkManagerNetworkTextIconNewWith :: NetworkWidgetConfig -> TaffyIO Widget
networkManagerNetworkTextIconNewWith _config = do
  chan <- getNetworkInfoChan
  ctx <- ask
  liftIO $ do
    label <- labelNew Nothing
    let updateIcon info = do
          let iconText = networkTextIcon info
          postGUIASync $ labelSetText label iconText
    void $
      onWidgetRealize label $
        runReaderT getNetworkInfoState ctx >>= updateIcon
    toWidget =<< channelWidgetNew label chan updateIcon

-- Network icon-label

-- | Build a combined network text-icon and label widget with default formatting.
networkManagerNetworkIconLabelNew :: TaffyIO Widget
networkManagerNetworkIconLabelNew =
  networkManagerNetworkIconLabelNewWith defaultNetworkWidgetConfig

-- | Build a combined network text-icon and label widget.
networkManagerNetworkIconLabelNewWith :: NetworkWidgetConfig -> TaffyIO Widget
networkManagerNetworkIconLabelNewWith config = do
  iconWidget <- networkManagerNetworkTextIconNewWith config
  labelWidget <- networkManagerNetworkLabelNewWith config
  liftIO $ buildIconLabelBox iconWidget labelWidget
