{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      : System.Taffybar.Widget.Bluetooth
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan A. Malison
-- Stability   : unstable
-- Portability : unportable
--
-- This module provides a Bluetooth status widget that displays the current
-- Bluetooth state, connected devices, and optionally battery percentages.
--
-- The widget uses the 'TChan'-based system from
-- "System.Taffybar.Information.Bluetooth" for receiving updates.
-----------------------------------------------------------------------------
module System.Taffybar.Widget.Bluetooth
  ( BluetoothWidgetConfig(..)
  , defaultBluetoothWidgetConfig
  , bluetoothNew
  , bluetoothNewWith
  ) where

import Control.Monad (void)
import Control.Monad.IO.Class
import Data.Char (ord)
import Data.Default (Default(..))
import Data.List (intercalate)
import qualified Data.Text as T
import qualified GI.GLib as G
import qualified GI.Gtk as Gtk
import System.Taffybar.Context (TaffyIO)
import System.Taffybar.Information.Bluetooth
import System.Taffybar.Util (postGUIASync)
import System.Taffybar.Widget.Generic.ChannelWidget
import Text.StringTemplate

-- | Configuration for the Bluetooth widget.
data BluetoothWidgetConfig = BluetoothWidgetConfig
  { -- | Format string when Bluetooth is connected.
    -- Available variables: $status$, $device_alias$, $device_battery$,
    -- $num_connections$, $controller_alias$, $icon$
    bluetoothFormatConnected :: String
    -- | Format string when Bluetooth is on but not connected.
  , bluetoothFormatOn :: String
    -- | Format string when Bluetooth is off (powered down).
  , bluetoothFormatOff :: String
    -- | Format string when no Bluetooth controller is found.
  , bluetoothFormatNoController :: String
    -- | Optional tooltip format.
    -- Additional variable: $device_list$
  , bluetoothTooltipFormat :: Maybe String
    -- | Format for each device in the tooltip device list.
    -- Variables: $device_alias$, $device_battery$
  , bluetoothDeviceListFormat :: String
  }

defaultBluetoothWidgetConfig :: BluetoothWidgetConfig
defaultBluetoothWidgetConfig = BluetoothWidgetConfig
  { bluetoothFormatConnected = "$icon$ $device_alias$"
  , bluetoothFormatOn = "$icon$"
  , bluetoothFormatOff = "$icon$"
  , bluetoothFormatNoController = "$icon$"
  , bluetoothTooltipFormat = Just "Bluetooth: $status$\nController: $controller_alias$\n$device_list$"
  , bluetoothDeviceListFormat = "$device_alias$ ($device_battery$%)"
  }

instance Default BluetoothWidgetConfig where
  def = defaultBluetoothWidgetConfig

-- | Create a new Bluetooth widget with default configuration.
bluetoothNew :: TaffyIO Gtk.Widget
bluetoothNew = bluetoothNewWith defaultBluetoothWidgetConfig

-- | Create a new Bluetooth widget with custom configuration.
bluetoothNewWith :: BluetoothWidgetConfig -> TaffyIO Gtk.Widget
bluetoothNewWith config = do
  chan <- getBluetoothInfoChan
  initialInfo <- getBluetoothInfoState

  liftIO $ do
    label <- Gtk.labelNew Nothing

    let updateLabel info = do
          (labelText, tooltipText) <- formatBluetoothWidget config info
          postGUIASync $ do
            Gtk.labelSetMarkup label labelText
            Gtk.widgetSetTooltipMarkup label tooltipText
            -- Add CSS classes based on status
            updateStyleClasses label info

    void $ Gtk.onWidgetRealize label $ updateLabel initialInfo

    Gtk.widgetShowAll label
    Gtk.toWidget =<< channelWidgetNew label chan updateLabel

-- | Update CSS classes on the widget based on Bluetooth status.
updateStyleClasses :: Gtk.Label -> BluetoothInfo -> IO ()
updateStyleClasses label info = do
  styleCtx <- Gtk.widgetGetStyleContext label

  -- Remove all bluetooth status classes first
  Gtk.styleContextRemoveClass styleCtx "bluetooth-connected"
  Gtk.styleContextRemoveClass styleCtx "bluetooth-on"
  Gtk.styleContextRemoveClass styleCtx "bluetooth-off"
  Gtk.styleContextRemoveClass styleCtx "bluetooth-no-controller"

  -- Add the appropriate class
  case bluetoothStatus info of
    BluetoothConnected -> Gtk.styleContextAddClass styleCtx "bluetooth-connected"
    BluetoothOn -> Gtk.styleContextAddClass styleCtx "bluetooth-on"
    BluetoothOff -> Gtk.styleContextAddClass styleCtx "bluetooth-off"
    BluetoothNoController -> Gtk.styleContextAddClass styleCtx "bluetooth-no-controller"

-- | Format the Bluetooth widget based on current state.
formatBluetoothWidget ::
  BluetoothWidgetConfig ->
  BluetoothInfo ->
  IO (T.Text, Maybe T.Text)
formatBluetoothWidget config info = do
  attrs <- buildAttrs info
  let format = case bluetoothStatus info of
        BluetoothConnected -> bluetoothFormatConnected config
        BluetoothOn -> bluetoothFormatOn config
        BluetoothOff -> bluetoothFormatOff config
        BluetoothNoController -> bluetoothFormatNoController config
      labelText = renderTemplate format attrs
      tooltipText = fmap (`renderTemplate` attrs) (bluetoothTooltipFormat config)
  return (T.pack labelText, T.pack <$> tooltipText)

-- | Build template attributes from Bluetooth info.
buildAttrs :: BluetoothInfo -> IO [(String, String)]
buildAttrs info = do
  let statusText = case bluetoothStatus info of
        BluetoothConnected -> "connected"
        BluetoothOn -> "on"
        BluetoothOff -> "off"
        BluetoothNoController -> "no-controller"

      -- Get the first connected device for primary display
      primaryDevice = case bluetoothConnectedDevices info of
        [] -> Nothing
        (d:_) -> Just d

      deviceAliasText = maybe "" deviceAlias primaryDevice
      deviceBatteryText = maybe "?" (maybe "?" show . deviceBatteryPercentage) primaryDevice
      numConnections = length $ bluetoothConnectedDevices info
      controllerAliasText = maybe "none" controllerAlias (bluetoothController info)

      -- Build device list for tooltip
      deviceListText = intercalate "\n" $
        map formatDeviceEntry (bluetoothConnectedDevices info)

      formatDeviceEntry dev =
        let battery = maybe "?" show (deviceBatteryPercentage dev)
        in deviceAlias dev ++ " (" ++ battery ++ "%)"

      iconText = bluetoothTextIcon (bluetoothStatus info)

  status <- escapeText $ T.pack statusText
  deviceAliasEsc <- escapeText $ T.pack deviceAliasText
  deviceBatteryEsc <- escapeText $ T.pack deviceBatteryText
  controllerAliasEsc <- escapeText $ T.pack controllerAliasText
  deviceListEsc <- escapeText $ T.pack deviceListText
  iconEsc <- escapeIconText iconText

  return
    [ ("status", status)
    , ("device_alias", deviceAliasEsc)
    , ("device_battery", deviceBatteryEsc)
    , ("num_connections", show numConnections)
    , ("controller_alias", controllerAliasEsc)
    , ("device_list", deviceListEsc)
    , ("icon", iconEsc)
    ]

-- | Get the appropriate icon for the Bluetooth status.
bluetoothTextIcon :: BluetoothStatus -> T.Text
bluetoothTextIcon status = case status of
  BluetoothConnected -> T.pack "\xF293"  -- Bluetooth connected (nf-fa-bluetooth_b)
  BluetoothOn -> T.pack "\xF293"         -- Bluetooth on
  BluetoothOff -> T.pack "\xF294"        -- Bluetooth off (nf-fa-bluetooth)
  BluetoothNoController -> T.pack "\xF294"

-- | Render a template with the given attributes.
renderTemplate :: String -> [(String, String)] -> String
renderTemplate template attrs = render $ setManyAttrib attrs (newSTMP template)

-- | Escape text for Pango markup.
escapeText :: T.Text -> IO String
escapeText input = T.unpack <$> G.markupEscapeText input (-1)

-- | Escape icon text with special font handling for Private Use Area characters.
-- This ensures Nerd Font icons render at the correct size.
escapeIconText :: T.Text -> IO String
escapeIconText input =
  let iconSpan s =
        "<span font_family=\"Iosevka Nerd Font\" font_weight=\"normal\" size=\"large\">"
        ++ s ++ "</span>"
  in do
    rendered <-
      concat
        <$> mapM
          (\c -> do
              esc <- escapeText (T.singleton c)
              pure $
                if isPUA c
                  then iconSpan esc
                  else esc
          )
          (T.unpack input)
    -- Add a trailing space in the icon's font/size so it doesn't look cramped.
    pure $
      if any isPUA (T.unpack input)
        then rendered ++ iconSpan "&#x2004;" -- three-per-em space
        else rendered

-- | Check if a character is in the Private Use Area (PUA).
isPUA :: Char -> Bool
isPUA c =
  let o = ord c
  in o >= 0xE000 && o <= 0xF8FF
