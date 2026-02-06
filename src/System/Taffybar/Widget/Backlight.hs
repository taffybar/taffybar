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
  , backlightLabelNew
  , backlightLabelNewWith
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
import qualified System.Taffybar.Information.Backlight as BL
import System.Taffybar.Util (runCommand)
import System.Taffybar.Widget.Generic.PollingLabel
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
    }

instance Default BacklightWidgetConfig where
  def = defaultBacklightWidgetConfig

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

formatBacklightWidget
  :: BacklightWidgetConfig
  -> IO (T.Text, Maybe T.Text)
formatBacklightWidget config = do
  info <- BL.getBacklightInfo (backlightDevice config)
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
