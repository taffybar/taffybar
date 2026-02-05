{-# LANGUAGE OverloadedStrings #-}
----------------------------------------------------------------------------- 
-- |
-- Module      : System.Taffybar.Widget.Audio
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan A. Malison
-- Stability   : unstable
-- Portability : unportable
--
-- Simple audio volume widget using PulseAudio's DBus interface.
--
-----------------------------------------------------------------------------

module System.Taffybar.Widget.Audio
  ( AudioWidgetConfig(..)
  , defaultAudioWidgetConfig
  , audioLabelNew
  , audioLabelNewWith
  ) where

import Control.Monad (void, when)
import Control.Monad.IO.Class
import Data.Default (Default(..))
import qualified Data.Text as T
import DBus
import DBus.Client
import qualified GI.Gdk.Structs.EventScroll as GdkEvent
import qualified GI.Gdk.Enums as Gdk
import qualified GI.Gtk as Gtk
import qualified GI.GLib as G
import System.Taffybar.DBus.Client.Params (paCoreObjectPath)
import System.Taffybar.Information.Audio
import System.Taffybar.Util (postGUIASync)
import Text.StringTemplate

-- | Configuration for the audio widget.
data AudioWidgetConfig = AudioWidgetConfig
  { audioPollingInterval :: Double
  , audioSink :: String
  , audioFormat :: String
  , audioMuteFormat :: String
  , audioUnknownFormat :: String
  , audioTooltipFormat :: Maybe String
  , audioScrollStepPercent :: Maybe Int
  , audioToggleMuteOnClick :: Bool
  }

-- | Default audio widget configuration.
defaultAudioWidgetConfig :: AudioWidgetConfig
defaultAudioWidgetConfig =
  AudioWidgetConfig
    { audioPollingInterval = 2
    , audioSink = "@DEFAULT_SINK@"
    , audioFormat = "vol: $volume$%"
    , audioMuteFormat = "vol: muted"
    , audioUnknownFormat = "vol: n/a"
    , audioTooltipFormat =
        Just "Sink: $sink$\nVolume: $volume$%\nMuted: $muted$"
    , audioScrollStepPercent = Just 5
    , audioToggleMuteOnClick = True
    }

instance Default AudioWidgetConfig where
  def = defaultAudioWidgetConfig

-- | Create an audio widget with default configuration.
audioLabelNew :: MonadIO m => m Gtk.Widget
audioLabelNew = audioLabelNewWith defaultAudioWidgetConfig

-- | Create an audio widget with the provided configuration.
audioLabelNewWith :: MonadIO m => AudioWidgetConfig -> m Gtk.Widget
audioLabelNewWith config = liftIO $ do
  label <- Gtk.labelNew Nothing

  let updateLabel info = do
        (labelText, tooltipText) <- formatAudioWidget config info
        postGUIASync $ do
          Gtk.labelSetMarkup label labelText
          Gtk.widgetSetTooltipMarkup label tooltipText

      refresh client =
        getAudioInfoFromClient client (audioSink config) >>= updateLabel

      refreshUnknown = updateLabel Nothing

  void $ Gtk.onWidgetRealize label $ do
    mClient <- connectPulseAudio
    case mClient of
      Nothing -> refreshUnknown
      Just client -> do
        refresh client
        let matcher =
              matchAny
                { matchPathNamespace = Just paCoreObjectPath
                , matchInterface = Just "org.freedesktop.DBus.Properties"
                , matchMember = Just "PropertiesChanged"
                }
        handler <- addMatch client matcher (const $ refresh client)
        void $ Gtk.onWidgetUnrealize label $ do
          removeMatch client handler
          disconnect client

  whenToggleMute label
  whenScrollAdjust label

  Gtk.widgetShowAll label
  return (Gtk.toWidget label)
  where
    whenToggleMute widget =
      if audioToggleMuteOnClick config
        then void $ Gtk.onWidgetButtonPressEvent widget $ \_ ->
          toggleMute config >> return True
        else return ()

    whenScrollAdjust widget =
      case audioScrollStepPercent config of
        Nothing -> return ()
        Just step | step <= 0 -> return ()
        Just step -> do
          _ <- Gtk.onWidgetScrollEvent widget $ \scrollEvent -> do
            dir <- GdkEvent.getEventScrollDirection scrollEvent
            case dir of
              Gdk.ScrollDirectionUp -> adjustVolume config step >> return True
              Gdk.ScrollDirectionDown -> adjustVolume config (-step) >> return True
              Gdk.ScrollDirectionLeft -> adjustVolume config step >> return True
              Gdk.ScrollDirectionRight -> adjustVolume config (-step) >> return True
              _ -> return False
          return ()

formatAudioWidget
  :: AudioWidgetConfig
  -> Maybe AudioInfo
  -> IO (T.Text, Maybe T.Text)
formatAudioWidget config info =
  case info of
    Nothing -> return (T.pack $ audioUnknownFormat config, Nothing)
    Just audio -> do
      attrs <- buildAttrs audio
      let
        labelTemplate = case audioMuted audio of
          Just True -> audioMuteFormat config
          _ -> audioFormat config
        labelText = renderTemplate labelTemplate attrs
        tooltipText = fmap (`renderTemplate` attrs) (audioTooltipFormat config)
      return (T.pack labelText, T.pack <$> tooltipText)

buildAttrs :: AudioInfo -> IO [(String, String)]
buildAttrs info = do
  let
    volumeText = maybe "?" show (audioVolumePercent info)
    mutedText = case audioMuted info of
      Just True -> "yes"
      Just False -> "no"
      Nothing -> "unknown"
    sinkText = audioSinkName info
  volume <- escapeText $ T.pack volumeText
  muted <- escapeText $ T.pack mutedText
  sink <- escapeText $ T.pack sinkText
  return
    [ ("volume", volume)
    , ("muted", muted)
    , ("sink", sink)
    ]

renderTemplate :: String -> [(String, String)] -> String
renderTemplate template attrs = render $ setManyAttrib attrs (newSTMP template)

escapeText :: T.Text -> IO String
escapeText input = T.unpack <$> G.markupEscapeText input (-1)

toggleMute :: AudioWidgetConfig -> IO ()
toggleMute config =
  void $ toggleAudioMute (audioSink config)

adjustVolume :: AudioWidgetConfig -> Int -> IO ()
adjustVolume config delta =
  void $ adjustAudioVolume (audioSink config) delta
