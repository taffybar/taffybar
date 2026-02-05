{-# LANGUAGE OverloadedStrings #-}
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
-----------------------------------------------------------------------------
module System.Taffybar.Widget.NetworkManager
  ( WifiWidgetConfig(..)
  , defaultWifiWidgetConfig
  , networkManagerWifiLabelNew
  , networkManagerWifiLabelNewWith

  , NetworkManagerWifiIconConfig(..)
  , defaultNetworkManagerWifiIconConfig
  , networkManagerWifiIconNew
  , networkManagerWifiIconNewWith

  , networkManagerWifiNew
  , networkManagerWifiNewWith

  , NetworkWidgetConfig(..)
  , defaultNetworkWidgetConfig
  , networkManagerNetworkLabelNew
  , networkManagerNetworkLabelNewWith

  , NetworkManagerNetworkIconConfig(..)
  , defaultNetworkManagerNetworkIconConfig
  , networkManagerNetworkIconNew
  , networkManagerNetworkIconNewWith

  , networkManagerNetworkNew
  , networkManagerNetworkNewWith
  ) where

import           Control.Applicative ((<|>))
import           Control.Concurrent.MVar
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Data.Char (ord)
import           Data.Default (Default(..))
import           Data.Int (Int32)
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified GI.GLib as G
import           GI.Gtk
import           StatusNotifier.Tray (scalePixbufToSize)
import           System.Taffybar.Context
import           System.Taffybar.Information.NetworkManager
import           System.Taffybar.Util
import           System.Taffybar.Widget.Generic.AutoSizeImage
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
    { wifiConnectedFormat = "$icon$ $ssid$ $strength$%"
    , wifiDisconnectedFormat = "$icon$ disconnected"
    , wifiDisabledFormat = "$icon$ off"
    , wifiUnknownFormat = "$icon$ unknown"
    , wifiTooltipFormat =
        Just "SSID: $ssid$\nStrength: $strength$%\nConnection: $connection$\nState: $state$"
    }

instance Default WifiWidgetConfig where
  def = defaultWifiWidgetConfig

networkManagerWifiLabelNew :: TaffyIO Widget
networkManagerWifiLabelNew = networkManagerWifiLabelNewWith defaultWifiWidgetConfig

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
    void $ onWidgetRealize label $
      runReaderT getWifiInfoState ctx >>= updateWidget
    toWidget =<< channelWidgetNew label chan updateWidget

data NetworkManagerWifiIconConfig = NetworkManagerWifiIconConfig
  { wifiIconNone :: String
  , wifiIconWeak :: String
  , wifiIconOk :: String
  , wifiIconGood :: String
  , wifiIconExcellent :: String
  , wifiIconDisconnected :: String
  , wifiIconDisabled :: String
  , wifiIconUnknown :: String
  , wifiIconTooltipFormat :: Maybe String
  }

defaultNetworkManagerWifiIconConfig :: NetworkManagerWifiIconConfig
defaultNetworkManagerWifiIconConfig =
  NetworkManagerWifiIconConfig
    { wifiIconNone = "network-wireless-signal-none-symbolic"
    , wifiIconWeak = "network-wireless-signal-weak-symbolic"
    , wifiIconOk = "network-wireless-signal-ok-symbolic"
    , wifiIconGood = "network-wireless-signal-good-symbolic"
    , wifiIconExcellent = "network-wireless-signal-excellent-symbolic"
    , wifiIconDisconnected = "network-wireless-offline-symbolic"
    , wifiIconDisabled = "network-wireless-disabled-symbolic"
    , wifiIconUnknown = "network-wireless-symbolic"
    , wifiIconTooltipFormat =
        Just "SSID: $ssid$\nStrength: $strength$%\nConnection: $connection$\nState: $state$"
    }

themeLoadFlags :: [IconLookupFlags]
themeLoadFlags = [IconLookupFlagsGenericFallback, IconLookupFlagsUseBuiltin]

networkManagerWifiIconNew :: TaffyIO Widget
networkManagerWifiIconNew = networkManagerWifiIconNewWith defaultNetworkManagerWifiIconConfig

networkManagerWifiIconNewWith :: NetworkManagerWifiIconConfig -> TaffyIO Widget
networkManagerWifiIconNewWith config = do
  chan <- getWifiInfoChan
  ctx <- ask
  liftIO $ do
    image <- imageNew
    styleCtx <- widgetGetStyleContext =<< toWidget image
    defaultTheme <- iconThemeGetDefault

    initialInfo <- runReaderT getWifiInfoState ctx
    infoVar <- newMVar initialInfo

    let
      setIconForSize size = do
        info <- readMVar infoVar
        let iconNames = wifiIconCandidates config info
        iconInfo <- lookupFirstIcon defaultTheme size iconNames
        traverse (extractPixbuf styleCtx) iconInfo >>=
          traverse (scalePixbufToSize size OrientationHorizontal)

    updateImage <- autoSizeImage image setIconForSize OrientationHorizontal
    let
      updateWidget info = do
        _ <- swapMVar infoVar info
        (_, tooltipText) <- formatWifiWidget (iconTooltipAsLabelConfig config) info
        postGUIASync $ do
          widgetSetTooltipMarkup image tooltipText
          updateImage
    void $ onWidgetRealize image $ updateWidget initialInfo
    toWidget =<< channelWidgetNew image chan updateWidget
  where
    extractPixbuf styleCtx iconInfo =
      fst <$> iconInfoLoadSymbolicForContext iconInfo styleCtx

iconTooltipAsLabelConfig :: NetworkManagerWifiIconConfig -> WifiWidgetConfig
iconTooltipAsLabelConfig cfg =
  defaultWifiWidgetConfig { wifiTooltipFormat = wifiIconTooltipFormat cfg }

lookupFirstIcon :: IconTheme -> Int32 -> [String] -> IO (Maybe IconInfo)
lookupFirstIcon _ _ [] = return Nothing
lookupFirstIcon theme size (name:names) = do
  info <- iconThemeLookupIcon theme (T.pack name) size themeLoadFlags
  case info of
    Just _ -> return info
    Nothing -> lookupFirstIcon theme size names

wifiIconCandidates :: NetworkManagerWifiIconConfig -> WifiInfo -> [String]
wifiIconCandidates cfg info =
  case wifiState info of
    WifiDisabled ->
      [ wifiIconDisabled cfg
      , wifiIconDisconnected cfg
      , wifiIconNone cfg
      ]
    WifiDisconnected ->
      [ wifiIconDisconnected cfg
      , wifiIconNone cfg
      ]
    WifiUnknown ->
      [ wifiIconUnknown cfg
      , wifiIconNone cfg
      ]
    WifiConnected ->
      strengthIconName cfg (wifiStrength info) :
      [ wifiIconGood cfg
      , wifiIconOk cfg
      , wifiIconWeak cfg
      , wifiIconNone cfg
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

networkManagerWifiNew :: TaffyIO Widget
networkManagerWifiNew = networkManagerWifiNewWith defaultWifiWidgetConfig defaultNetworkManagerWifiIconConfig

networkManagerWifiNewWith
  :: WifiWidgetConfig
  -> NetworkManagerWifiIconConfig
  -> TaffyIO Widget
networkManagerWifiNewWith labelCfg iconCfg = do
  iconWidget <- networkManagerWifiIconNewWith iconCfg
  labelWidget <- networkManagerWifiLabelNewWith labelCfg
  liftIO $ do
    box <- boxNew OrientationHorizontal 0
    containerAdd box iconWidget
    containerAdd box labelWidget
    widgetShowAll box
    toWidget box

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
    iconText = wifiTextIcon info
  ssid <- escapeText ssidText
  strength <- escapeText (T.pack strengthText)
  state <- escapeText (T.pack stateText)
  connection <- escapeText connectionText
  icon <- escapeIconText iconText
  return
    [ ("ssid", ssid)
    , ("strength", strength)
    , ("state", state)
    , ("connection", connection)
    , ("icon", icon)
    ]

escapeText :: T.Text -> IO String
escapeText input = T.unpack <$> G.markupEscapeText input (-1)

-- Font Awesome / Nerd Font glyphs live in the Private Use Area and can render
-- much smaller than the surrounding text depending on which fallback font gets
-- selected. Enlarge only the PUA glyphs so the "strength bar" characters (and
-- other plain text) remain at the normal size.
escapeIconText :: T.Text -> IO String
escapeIconText input =
  concat
    <$>
      mapM
        (\c -> do
            esc <- escapeText (T.singleton c)
            pure $
              if isPUA c
                then "<span size=\"large\">" ++ esc ++ "</span>"
                else esc
        )
        (T.unpack input)

isPUA :: Char -> Bool
isPUA c =
  let o = ord c
  in o >= 0xE000 && o <= 0xF8FF

wifiStateText :: WifiState -> String
wifiStateText WifiDisabled = "disabled"
wifiStateText WifiDisconnected = "disconnected"
wifiStateText WifiConnected = "connected"
wifiStateText WifiUnknown = "unknown"

-- | A text "icon" intended for label widgets. Uses Font Awesome-ish codepoints
-- plus a bar whose height varies with strength. If your font lacks the FA glyph
-- it will degrade to just showing the bar.
wifiTextIcon :: WifiInfo -> T.Text
wifiTextIcon info =
  case wifiState info of
    WifiConnected ->
      T.pack "\xF1EB" <> wifiStrengthBar (wifiStrength info) -- 
    WifiDisconnected -> T.pack "\xF05E" -- 
    WifiDisabled -> T.pack "\xF05E" -- 
    WifiUnknown -> T.pack "\xF059" -- 

wifiStrengthBar :: Maybe Int -> T.Text
wifiStrengthBar strength =
  T.pack $
    case strength of
      Nothing -> "\x2591" -- ░
      Just s
        | s < 20 -> "\x2581" -- ▁
        | s < 40 -> "\x2583" -- ▃
        | s < 70 -> "\x2586" -- ▆
        | otherwise -> "\x2588" -- █

-- Network (WiFi + wired + vpn + disconnected)

data NetworkWidgetConfig = NetworkWidgetConfig
  { networkWifiFormat :: String
  , networkWiredFormat :: String
  , networkVpnFormat :: String
  , networkOtherFormat :: String
  , networkDisconnectedFormat :: String
  , networkWifiDisabledFormat :: String
  , networkUnknownFormat :: String
  , networkTooltipFormat :: Maybe String
  }

defaultNetworkWidgetConfig :: NetworkWidgetConfig
defaultNetworkWidgetConfig =
  NetworkWidgetConfig
    { networkWifiFormat = "$icon$ $ssid$ $strength$%"
    , networkWiredFormat = "$icon$ $connection$"
    , networkVpnFormat = "$icon$ $connection$"
    , networkOtherFormat = "$icon$ $type$ $connection$"
    , networkDisconnectedFormat = "$icon$ disconnected"
    , networkWifiDisabledFormat = "$icon$ disconnected (wifi off)"
    , networkUnknownFormat = "$icon$ unknown"
    , networkTooltipFormat =
        Just "Type: $type$\nConnection: $connection$\nSSID: $ssid$\nStrength: $strength$%\nState: $state$"
    }

instance Default NetworkWidgetConfig where
  def = defaultNetworkWidgetConfig

networkManagerNetworkLabelNew :: TaffyIO Widget
networkManagerNetworkLabelNew =
  networkManagerNetworkLabelNewWith defaultNetworkWidgetConfig

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
    void $ onWidgetRealize label $
      runReaderT getNetworkInfoState ctx >>= updateWidget
    toWidget =<< channelWidgetNew label chan updateWidget

data NetworkManagerNetworkIconConfig = NetworkManagerNetworkIconConfig
  { netIconWired :: String
  , netIconVpn :: String
  , netIconOther :: String
  , netIconDisconnected :: String
  , netIconUnknown :: String
  , netIconWifiDisabled :: String
  , netWifiIconNone :: String
  , netWifiIconWeak :: String
  , netWifiIconOk :: String
  , netWifiIconGood :: String
  , netWifiIconExcellent :: String
  , netIconTooltipFormat :: Maybe String
  }

defaultNetworkManagerNetworkIconConfig :: NetworkManagerNetworkIconConfig
defaultNetworkManagerNetworkIconConfig =
  NetworkManagerNetworkIconConfig
    { netIconWired = "network-wired-symbolic"
    , netIconVpn = "network-vpn-symbolic"
    , netIconOther = "network-transmit-receive-symbolic"
    , netIconDisconnected = "network-offline-symbolic"
    , netIconUnknown = "network-error-symbolic"
    , netIconWifiDisabled = "network-wireless-disabled-symbolic"
    , netWifiIconNone = "network-wireless-signal-none-symbolic"
    , netWifiIconWeak = "network-wireless-signal-weak-symbolic"
    , netWifiIconOk = "network-wireless-signal-ok-symbolic"
    , netWifiIconGood = "network-wireless-signal-good-symbolic"
    , netWifiIconExcellent = "network-wireless-signal-excellent-symbolic"
    , netIconTooltipFormat =
        Just "Type: $type$\nConnection: $connection$\nSSID: $ssid$\nStrength: $strength$%\nState: $state$"
    }

networkManagerNetworkIconNew :: TaffyIO Widget
networkManagerNetworkIconNew =
  networkManagerNetworkIconNewWith defaultNetworkManagerNetworkIconConfig

networkManagerNetworkIconNewWith :: NetworkManagerNetworkIconConfig -> TaffyIO Widget
networkManagerNetworkIconNewWith config = do
  chan <- getNetworkInfoChan
  ctx <- ask
  liftIO $ do
    image <- imageNew
    styleCtx <- widgetGetStyleContext =<< toWidget image
    defaultTheme <- iconThemeGetDefault

    initialInfo <- runReaderT getNetworkInfoState ctx
    infoVar <- newMVar initialInfo

    let
      setIconForSize size = do
        info <- readMVar infoVar
        let iconNames = networkIconCandidates config info
        iconInfo <- lookupFirstIcon defaultTheme size iconNames
        traverse (extractPixbuf styleCtx) iconInfo >>=
          traverse (scalePixbufToSize size OrientationHorizontal)

    updateImage <- autoSizeImage image setIconForSize OrientationHorizontal
    let
      updateWidget info = do
        _ <- swapMVar infoVar info
        (_, tooltipText) <- formatNetworkWidget (iconTooltipAsNetworkLabelConfig config) info
        postGUIASync $ do
          widgetSetTooltipMarkup image tooltipText
          updateImage
    void $ onWidgetRealize image $ updateWidget initialInfo
    toWidget =<< channelWidgetNew image chan updateWidget
  where
    extractPixbuf styleCtx iconInfo =
      fst <$> iconInfoLoadSymbolicForContext iconInfo styleCtx

iconTooltipAsNetworkLabelConfig :: NetworkManagerNetworkIconConfig -> NetworkWidgetConfig
iconTooltipAsNetworkLabelConfig cfg =
  defaultNetworkWidgetConfig { networkTooltipFormat = netIconTooltipFormat cfg }

networkIconCandidates :: NetworkManagerNetworkIconConfig -> NetworkInfo -> [String]
networkIconCandidates cfg info =
  case networkState info of
    NetworkUnknown ->
      [ netIconUnknown cfg
      , netIconOther cfg
      , netIconDisconnected cfg
      ]
    NetworkDisconnected ->
      case networkWirelessEnabled info of
        Just False ->
          [ netIconWifiDisabled cfg
          , netIconDisconnected cfg
          ]
        _ ->
          [ netIconDisconnected cfg
          , netIconOther cfg
          ]
    NetworkConnected ->
      case networkType info of
        Just NetworkWired ->
          [ netIconWired cfg
          , netIconOther cfg
          ]
        Just NetworkVpn ->
          [ netIconVpn cfg
          , netIconOther cfg
          ]
        Just NetworkWifi ->
          strengthToWifiIcon cfg (networkStrength info) :
          [ netWifiIconGood cfg
          , netWifiIconOk cfg
          , netWifiIconWeak cfg
          , netWifiIconNone cfg
          ]
        Just (NetworkOther _) ->
          [ netIconOther cfg
          , netIconWired cfg
          ]
        Nothing ->
          [ netIconOther cfg
          , netIconWired cfg
          , netIconDisconnected cfg
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

networkManagerNetworkNew :: TaffyIO Widget
networkManagerNetworkNew =
  networkManagerNetworkNewWith defaultNetworkWidgetConfig defaultNetworkManagerNetworkIconConfig

networkManagerNetworkNewWith
  :: NetworkWidgetConfig
  -> NetworkManagerNetworkIconConfig
  -> TaffyIO Widget
networkManagerNetworkNewWith labelCfg iconCfg = do
  iconWidget <- networkManagerNetworkIconNewWith iconCfg
  labelWidget <- networkManagerNetworkLabelNewWith labelCfg
  liftIO $ do
    box <- boxNew OrientationHorizontal 0
    containerAdd box iconWidget
    containerAdd box labelWidget
    widgetShowAll box
    toWidget box

formatNetworkWidget
  :: NetworkWidgetConfig
  -> NetworkInfo
  -> IO (T.Text, Maybe T.Text)
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
  let
    rawConnText = fromMaybe "" (networkConnectionId info)
    connText = if T.null rawConnText then "unknown" else rawConnText
    typeText = networkTypeText (networkType info)
    displaySsid = fromMaybe "" (networkSsid info)
    ssidText = if T.null displaySsid then "unknown" else displaySsid
    strengthText = maybe "?" show (networkStrength info)
    stateText = networkStateText (networkState info)
    iconText = networkTextIcon info
  conn <- escapeText connText
  typ <- escapeText typeText
  ssid <- escapeText ssidText
  strength <- escapeText (T.pack strengthText)
  state <- escapeText (T.pack stateText)
  icon <- escapeIconText iconText
  return
    [ ("connection", conn)
    , ("type", typ)
    , ("ssid", ssid)
    , ("strength", strength)
    , ("state", state)
    , ("icon", icon)
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
          T.pack "\xF1EB" <> wifiStrengthBar (networkStrength info) -- 
        Just NetworkWired -> T.pack "\xF1E6" -- 
        Just NetworkVpn -> T.pack "\xF023" -- 
        Just (NetworkOther _) -> T.pack "\xF0AC" -- 
        Nothing -> T.pack "\xF0AC" -- 
