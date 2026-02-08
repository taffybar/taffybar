{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      : System.Taffybar.Widget.PulseAudio
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan A. Malison
-- Stability   : unstable
-- Portability : unportable
--
-- Simple volume widget using PulseAudio's DBus interface.
--
-- Note: PulseAudio's DBus socket is not always enabled by default.
-- If the widget shows "vol: n/a", ensure the PulseAudio server has loaded
-- @module-dbus-protocol@ (so @/run/user/$UID/pulse/dbus-socket@ exists).
-----------------------------------------------------------------------------

module System.Taffybar.Widget.PulseAudio
  ( PulseAudioWidgetConfig(..)
  , defaultPulseAudioWidgetConfig
  , pulseAudioIconNew
  , pulseAudioIconNewWith
  , pulseAudioLabelNew
  , pulseAudioLabelNewWith
  , pulseAudioNew
  , pulseAudioNewWith
  ) where

import Control.Monad (void, when)
import Control.Monad.IO.Class
import Data.Default (Default(..))
import qualified Data.Text as T
import qualified GI.Gdk.Enums as Gdk
import qualified GI.Gdk.Structs.EventScroll as GdkEvent
import qualified GI.GLib as G
import qualified GI.Gtk as Gtk
import System.Taffybar.Context (TaffyIO)
import System.Taffybar.Information.PulseAudio
import System.Taffybar.Util (postGUIASync)
import System.Taffybar.Widget.Generic.ChannelWidget
import System.Taffybar.Widget.Util (buildIconLabelBox)
import Text.StringTemplate

data PulseAudioWidgetConfig = PulseAudioWidgetConfig
  { pulseAudioSink :: String
  , pulseAudioFormat :: String
  , pulseAudioMuteFormat :: String
  , pulseAudioUnknownFormat :: String
  , pulseAudioTooltipFormat :: Maybe String
  , pulseAudioScrollStepPercent :: Maybe Int
  , pulseAudioToggleMuteOnClick :: Bool
  }

defaultPulseAudioWidgetConfig :: PulseAudioWidgetConfig
defaultPulseAudioWidgetConfig =
  PulseAudioWidgetConfig
    { pulseAudioSink = "@DEFAULT_SINK@"
    , pulseAudioFormat = "$volume$%"
    , pulseAudioMuteFormat = "muted"
    , pulseAudioUnknownFormat = "n/a"
    , pulseAudioTooltipFormat =
        Just "Sink: $sink$\nVolume: $volume$%\nMuted: $muted$"
    , pulseAudioScrollStepPercent = Just 5
    , pulseAudioToggleMuteOnClick = True
    }

instance Default PulseAudioWidgetConfig where
  def = defaultPulseAudioWidgetConfig

pulseAudioIconNew :: TaffyIO Gtk.Widget
pulseAudioIconNew = pulseAudioIconNewWith defaultPulseAudioWidgetConfig

pulseAudioIconNewWith :: PulseAudioWidgetConfig -> TaffyIO Gtk.Widget
pulseAudioIconNewWith config = do
  let sinkSpec = pulseAudioSink config
  chan <- getPulseAudioInfoChan sinkSpec
  initialInfo <- getPulseAudioInfoState sinkSpec
  liftIO $ do
    label <- Gtk.labelNew Nothing
    let updateIcon info = do
          let iconText = case info of
                Nothing -> T.pack "\xF026"
                Just i -> pulseAudioTextIcon (pulseAudioMuted i) (pulseAudioVolumePercent i)
          postGUIASync $ Gtk.labelSetText label iconText
    void $ Gtk.onWidgetRealize label $ updateIcon initialInfo
    Gtk.widgetShowAll label
    Gtk.toWidget =<< channelWidgetNew label chan updateIcon

pulseAudioLabelNew :: TaffyIO Gtk.Widget
pulseAudioLabelNew = pulseAudioLabelNewWith defaultPulseAudioWidgetConfig

pulseAudioLabelNewWith :: PulseAudioWidgetConfig -> TaffyIO Gtk.Widget
pulseAudioLabelNewWith config = do
  let sinkSpec = pulseAudioSink config
  chan <- getPulseAudioInfoChan sinkSpec
  initialInfo <- getPulseAudioInfoState sinkSpec

  liftIO $ do
    label <- Gtk.labelNew Nothing

    let
      updateLabel info = do
        (labelText, tooltipText) <- formatPulseAudioWidget config info
        postGUIASync $ do
          Gtk.labelSetMarkup label labelText
          Gtk.widgetSetTooltipMarkup label tooltipText

      refreshNow = getPulseAudioInfo sinkSpec >>= updateLabel

      whenToggleMute widget =
        when (pulseAudioToggleMuteOnClick config) $
          void $ Gtk.onWidgetButtonPressEvent widget $ \_ -> do
            void $ togglePulseAudioMute sinkSpec
            refreshNow
            return True

      whenScrollAdjust widget =
        case pulseAudioScrollStepPercent config of
          Nothing -> return ()
          Just step | step <= 0 -> return ()
          Just step -> do
            _ <- Gtk.onWidgetScrollEvent widget $ \scrollEvent -> do
              dir <- GdkEvent.getEventScrollDirection scrollEvent
              let doAdjust delta = do
                    void $ adjustPulseAudioVolume sinkSpec delta
                    refreshNow
                    return True
              case dir of
                Gdk.ScrollDirectionUp -> doAdjust step
                Gdk.ScrollDirectionDown -> doAdjust (-step)
                Gdk.ScrollDirectionLeft -> doAdjust step
                Gdk.ScrollDirectionRight -> doAdjust (-step)
                _ -> return False
            return ()

    void $ Gtk.onWidgetRealize label $ updateLabel initialInfo

    whenToggleMute label
    whenScrollAdjust label

    Gtk.widgetShowAll label
    Gtk.toWidget =<< channelWidgetNew label chan updateLabel

pulseAudioNew :: TaffyIO Gtk.Widget
pulseAudioNew = pulseAudioNewWith defaultPulseAudioWidgetConfig

pulseAudioNewWith :: PulseAudioWidgetConfig -> TaffyIO Gtk.Widget
pulseAudioNewWith config = do
  iconWidget <- pulseAudioIconNewWith config
  labelWidget <- pulseAudioLabelNewWith config
  liftIO $ buildIconLabelBox iconWidget labelWidget

formatPulseAudioWidget
  :: PulseAudioWidgetConfig
  -> Maybe PulseAudioInfo
  -> IO (T.Text, Maybe T.Text)
formatPulseAudioWidget config info = do
  attrs <- maybe buildUnknownAttrs buildAttrs info
  let
    labelTemplate =
      case info of
        Nothing -> pulseAudioUnknownFormat config
        Just audio ->
          case pulseAudioMuted audio of
            Just True -> pulseAudioMuteFormat config
            _ -> pulseAudioFormat config
    labelText = renderTemplate labelTemplate attrs
    tooltipText = fmap (`renderTemplate` attrs) (pulseAudioTooltipFormat config)
  return (T.pack labelText, T.pack <$> tooltipText)

buildAttrs :: PulseAudioInfo -> IO [(String, String)]
buildAttrs info = do
  let
    volumeText = maybe "?" show (pulseAudioVolumePercent info)
    mutedText = case pulseAudioMuted info of
      Just True -> "yes"
      Just False -> "no"
      Nothing -> "unknown"
    sinkText = pulseAudioSinkName info
  volume <- escapeText $ T.pack volumeText
  muted <- escapeText $ T.pack mutedText
  sink <- escapeText $ T.pack sinkText
  return
    [ ("volume", volume)
    , ("muted", muted)
    , ("sink", sink)
    ]

buildUnknownAttrs :: IO [(String, String)]
buildUnknownAttrs =
  return
    [ ("volume", "?")
    , ("muted", "unknown")
    , ("sink", "unknown")
    ]

pulseAudioTextIcon :: Maybe Bool -> Maybe Int -> T.Text
pulseAudioTextIcon muted volumePercent =
  case muted of
    Just True -> T.pack "\xF026" --
    _ ->
      case volumePercent of
        Just v | v <= 0 -> T.pack "\xF026" --
        Just v | v <= 33 -> T.pack "\xF027" --
        Just _ -> T.pack "\xF028" --
        Nothing -> T.pack "\xF028" --

renderTemplate :: String -> [(String, String)] -> String
renderTemplate template attrs = render $ setManyAttrib attrs (newSTMP template)

escapeText :: T.Text -> IO String
escapeText input = T.unpack <$> G.markupEscapeText input (-1)
