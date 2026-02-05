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
  , pulseAudioLabelNew
  , pulseAudioLabelNewWith
  ) where

import Control.Monad (void, when)
import Control.Monad.IO.Class
import Data.Char (ord)
import Data.Default (Default(..))
import qualified Data.Text as T
import qualified GI.Gdk.Enums as Gdk
import qualified GI.Gdk.Structs.EventScroll as GdkEvent
import qualified GI.GLib as G
import qualified GI.Gtk as Gtk
import System.Taffybar.Information.PulseAudio
import System.Taffybar.Util (postGUIASync)
import System.Taffybar.Widget.Generic.ChannelWidget
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
    , pulseAudioFormat = "$icon$ $volume$%"
    , pulseAudioMuteFormat = "$icon$ muted"
    , pulseAudioUnknownFormat = "$icon$ n/a"
    , pulseAudioTooltipFormat =
        Just "Sink: $sink$\nVolume: $volume$%\nMuted: $muted$"
    , pulseAudioScrollStepPercent = Just 5
    , pulseAudioToggleMuteOnClick = True
    }

instance Default PulseAudioWidgetConfig where
  def = defaultPulseAudioWidgetConfig

pulseAudioLabelNew :: MonadIO m => m Gtk.Widget
pulseAudioLabelNew = pulseAudioLabelNewWith defaultPulseAudioWidgetConfig

pulseAudioLabelNewWith :: MonadIO m => PulseAudioWidgetConfig -> m Gtk.Widget
pulseAudioLabelNewWith config = liftIO $ do
  label <- Gtk.labelNew Nothing

  let
    sinkSpec = pulseAudioSink config

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

  chan <- getPulseAudioInfoChan sinkSpec
  initialInfo <- getPulseAudioInfoState sinkSpec
  void $ Gtk.onWidgetRealize label $ updateLabel initialInfo

  whenToggleMute label
  whenScrollAdjust label

  Gtk.widgetShowAll label
  Gtk.toWidget =<< channelWidgetNew label chan updateLabel

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
    iconText = pulseAudioTextIcon (pulseAudioMuted info) (pulseAudioVolumePercent info)
  volume <- escapeText $ T.pack volumeText
  muted <- escapeText $ T.pack mutedText
  sink <- escapeText $ T.pack sinkText
  icon <- escapeIconText iconText
  return
    [ ("volume", volume)
    , ("muted", muted)
    , ("sink", sink)
    , ("icon", icon)
    ]

buildUnknownAttrs :: IO [(String, String)]
buildUnknownAttrs = do
  icon <- escapeIconText (T.pack "\xF026") -- 
  return
    [ ("volume", "?")
    , ("muted", "unknown")
    , ("sink", "unknown")
    , ("icon", icon)
    ]

pulseAudioTextIcon :: Maybe Bool -> Maybe Int -> T.Text
pulseAudioTextIcon muted volumePercent =
  case muted of
    Just True -> T.pack "\xF026" -- 
    _ ->
      case volumePercent of
        Just v | v <= 0 -> T.pack "\xF026" -- 
        Just v | v <= 33 -> T.pack "\xF027" -- 
        Just _ -> T.pack "\xF028" -- 
        Nothing -> T.pack "\xF028" -- 

renderTemplate :: String -> [(String, String)] -> String
renderTemplate template attrs = render $ setManyAttrib attrs (newSTMP template)

escapeText :: T.Text -> IO String
escapeText input = T.unpack <$> G.markupEscapeText input (-1)

-- Font Awesome / Nerd Font glyphs live in the Private Use Area and can render
-- much smaller than the surrounding text depending on which fallback font gets
-- selected. Enlarge only the PUA glyphs so digits/letters remain untouched.
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
