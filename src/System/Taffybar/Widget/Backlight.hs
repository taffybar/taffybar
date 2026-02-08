{-# LANGUAGE OverloadedStrings #-}
----------------------------------------------------------------------------- 
-- |
-- Module      : System.Taffybar.Widget.Backlight
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan A. Malison
-- Stability   : unstable
-- Portability : unportable
--
-- Simple backlight widget using /sys/class/backlight and optional
-- brightnessctl adjustments.
--
-----------------------------------------------------------------------------

module System.Taffybar.Widget.Backlight
  ( BacklightWidgetConfig(..)
  , defaultBacklightWidgetConfig
  , backlightIconNew
  , backlightIconNewWith
  , backlightLabelNew
  , backlightLabelNewWith
  , backlightLabelNewChan
  , backlightLabelNewChanWith
  , backlightNew
  , backlightNewWith
  , backlightNewChan
  , backlightNewChanWith
  ) where

import Control.Exception (SomeException, catch)
import Control.Monad (void)
import Control.Monad.IO.Class
import Data.Default (Default(..))
import qualified Data.Text as T
import qualified GI.Gdk.Structs.EventScroll as GdkEvent
import qualified GI.Gdk.Enums as Gdk
import qualified GI.Gtk as Gtk
import qualified GI.GLib as G
import System.Taffybar.Context (TaffyIO)
import qualified System.Taffybar.Information.Backlight as BL
import System.Taffybar.Util (postGUIASync, runCommand)
import System.Taffybar.Widget.Generic.ChannelWidget
import System.Taffybar.Widget.Generic.PollingLabel
import System.Taffybar.Widget.Util (buildIconLabelBox)
import Text.StringTemplate

-- | Configuration for the backlight widget.
data BacklightWidgetConfig = BacklightWidgetConfig
  { backlightPollingInterval :: Double
  , backlightDevice :: Maybe FilePath
  , backlightFormat :: String
  , backlightUnknownFormat :: String
  , backlightTooltipFormat :: Maybe String
  , backlightScrollStepPercent :: Maybe Int
  , backlightBrightnessctlPath :: FilePath
  , backlightIcon :: T.Text
  -- ^ Nerd font icon character (default U+F0EB, \xF0EB).
  }

-- | Default backlight widget configuration.
defaultBacklightWidgetConfig :: BacklightWidgetConfig
defaultBacklightWidgetConfig =
  BacklightWidgetConfig
    { backlightPollingInterval = 2
    , backlightDevice = Nothing
    , backlightFormat = "bl: $percent$%"
    , backlightUnknownFormat = "bl: n/a"
    , backlightTooltipFormat =
        Just "Device: $device$\nBrightness: $brightness$/$max$ ($percent$%)"
    , backlightScrollStepPercent = Just 5
    , backlightBrightnessctlPath = "brightnessctl"
    , backlightIcon = T.pack "\xF0EB"
    }

instance Default BacklightWidgetConfig where
  def = defaultBacklightWidgetConfig

-- | Create a backlight icon widget with default configuration.
backlightIconNew :: MonadIO m => m Gtk.Widget
backlightIconNew = backlightIconNewWith defaultBacklightWidgetConfig

-- | Create a backlight icon widget with the provided configuration.
backlightIconNewWith :: MonadIO m => BacklightWidgetConfig -> m Gtk.Widget
backlightIconNewWith config = liftIO $ do
  label <- Gtk.labelNew (Just (backlightIcon config))
  Gtk.widgetShowAll label
  Gtk.toWidget label

-- | Create a combined icon+label backlight widget with default configuration.
backlightNew :: MonadIO m => m Gtk.Widget
backlightNew = backlightNewWith defaultBacklightWidgetConfig

-- | Create a combined icon+label backlight widget.
backlightNewWith :: MonadIO m => BacklightWidgetConfig -> m Gtk.Widget
backlightNewWith config = liftIO $ do
  iconWidget <- backlightIconNewWith config
  labelWidget <- backlightLabelNewWith config
  buildIconLabelBox iconWidget labelWidget

-- | Create a combined icon+label backlight widget (channel-driven).
backlightNewChan :: TaffyIO Gtk.Widget
backlightNewChan = backlightNewChanWith defaultBacklightWidgetConfig

-- | Create a combined icon+label backlight widget (channel-driven).
backlightNewChanWith :: BacklightWidgetConfig -> TaffyIO Gtk.Widget
backlightNewChanWith config = do
  iconWidget <- liftIO $ backlightIconNewWith config
  labelWidget <- backlightLabelNewChanWith config
  liftIO $ buildIconLabelBox iconWidget labelWidget

-- | Create a backlight widget with default configuration.
backlightLabelNew :: MonadIO m => m Gtk.Widget
backlightLabelNew = backlightLabelNewWith defaultBacklightWidgetConfig

-- | Create a backlight widget with the provided configuration.
backlightLabelNewWith :: MonadIO m => BacklightWidgetConfig -> m Gtk.Widget
backlightLabelNewWith config = liftIO $ do
  widget <- pollingLabelNewWithTooltip
    (backlightPollingInterval config)
    (formatBacklightWidget config)

  case backlightScrollStepPercent config of
    Nothing -> return ()
    Just step | step <= 0 -> return ()
    Just step -> do
      _ <- Gtk.onWidgetScrollEvent widget $ \scrollEvent -> do
        dir <- GdkEvent.getEventScrollDirection scrollEvent
        case dir of
          Gdk.ScrollDirectionUp -> adjustBacklight config step >> return True
          Gdk.ScrollDirectionDown -> adjustBacklight config (-step) >> return True
          Gdk.ScrollDirectionLeft -> adjustBacklight config step >> return True
          Gdk.ScrollDirectionRight -> adjustBacklight config (-step) >> return True
          _ -> return False
      return ()

  Gtk.widgetShowAll widget
  return widget

-- | Create a backlight widget driven by a 'TChan' (no polling in the widget).
--
-- Monitoring is handled in 'System.Taffybar.Information.Backlight' (udev when
-- possible, with interval refresh as a fallback).
backlightLabelNewChan :: TaffyIO Gtk.Widget
backlightLabelNewChan = backlightLabelNewChanWith defaultBacklightWidgetConfig

-- | Create a backlight widget driven by a 'TChan' (no polling in the widget).
backlightLabelNewChanWith :: BacklightWidgetConfig -> TaffyIO Gtk.Widget
backlightLabelNewChanWith config = do
  chan <- BL.getBacklightInfoChanWithInterval
    (backlightDevice config)
    (backlightPollingInterval config)
  initialInfo <- BL.getBacklightInfoState (backlightDevice config)

  liftIO $ do
    label <- Gtk.labelNew Nothing

    let
      updateLabel info = do
        (labelText, tooltipText) <- formatBacklightWidgetFromInfo config info
        postGUIASync $ do
          Gtk.labelSetMarkup label labelText
          Gtk.widgetSetTooltipMarkup label tooltipText

      refreshNow = BL.getBacklightInfo (backlightDevice config) >>= updateLabel

    void $ Gtk.onWidgetRealize label $ updateLabel initialInfo

    case backlightScrollStepPercent config of
      Nothing -> return ()
      Just step | step <= 0 -> return ()
      Just step -> do
        _ <- Gtk.onWidgetScrollEvent label $ \scrollEvent -> do
          dir <- GdkEvent.getEventScrollDirection scrollEvent
          let doAdjust delta = do
                adjustBacklight config delta
                refreshNow
                return True
          case dir of
            Gdk.ScrollDirectionUp -> doAdjust step
            Gdk.ScrollDirectionDown -> doAdjust (-step)
            Gdk.ScrollDirectionLeft -> doAdjust step
            Gdk.ScrollDirectionRight -> doAdjust (-step)
            _ -> return False
        return ()

    Gtk.widgetShowAll label
    Gtk.toWidget =<< channelWidgetNew label chan updateLabel

formatBacklightWidget
  :: BacklightWidgetConfig
  -> IO (T.Text, Maybe T.Text)
formatBacklightWidget config = do
  info <- BL.getBacklightInfo (backlightDevice config)
  formatBacklightWidgetFromInfo config info

formatBacklightWidgetFromInfo
  :: BacklightWidgetConfig
  -> Maybe BL.BacklightInfo
  -> IO (T.Text, Maybe T.Text)
formatBacklightWidgetFromInfo config info =
  case info of
    Nothing -> return (T.pack $ backlightUnknownFormat config, Nothing)
    Just bl -> do
      attrs <- buildAttrs bl
      let labelText = renderTemplate (backlightFormat config) attrs
          tooltipText = fmap (`renderTemplate` attrs) (backlightTooltipFormat config)
      return (T.pack labelText, T.pack <$> tooltipText)

buildAttrs :: BL.BacklightInfo -> IO [(String, String)]
buildAttrs info = do
  let
    brightnessText = show $ BL.backlightBrightness info
    maxText = show $ BL.backlightMaxBrightness info
    percentText = show $ BL.backlightPercent info
    deviceText = BL.backlightDevice info
  brightness <- escapeText $ T.pack brightnessText
  maxBrightness <- escapeText $ T.pack maxText
  percent <- escapeText $ T.pack percentText
  device <- escapeText $ T.pack deviceText
  return
    [ ("brightness", brightness)
    , ("max", maxBrightness)
    , ("percent", percent)
    , ("device", device)
    ]

renderTemplate :: String -> [(String, String)] -> String
renderTemplate template attrs = render $ setManyAttrib attrs (newSTMP template)

escapeText :: T.Text -> IO String
escapeText input = T.unpack <$> G.markupEscapeText input (-1)

adjustBacklight :: BacklightWidgetConfig -> Int -> IO ()
adjustBacklight config delta =
  void $ safeRunCommand (backlightBrightnessctlPath config) (brightnessctlArgs delta)
  where
    brightnessctlArgs change =
      let
        step = abs change
        direction = if change >= 0 then "+" else "-"
        deviceArgs = maybe [] (\dev -> ["-d", dev]) (backlightDevice config)
      in deviceArgs ++ ["set", show step ++ "%" ++ direction]

safeRunCommand :: FilePath -> [String] -> IO (Either String String)
safeRunCommand cmd args =
  catch (runCommand cmd args) $ \(e :: SomeException) ->
    return $ Left $ show e
