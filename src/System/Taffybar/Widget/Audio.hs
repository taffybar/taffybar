{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : System.Taffybar.Widget.Audio
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan A. Malison
-- Stability   : unstable
-- Portability : unportable
--
-- Audio widget that automatically uses PulseAudio DBus when available and
-- falls back to WirePlumber/PipeWire through libwireplumber otherwise.
module System.Taffybar.Widget.Audio
  ( AudioWidgetConfig (..),
    defaultAudioWidgetConfig,
    audioIconNew,
    audioIconNewWith,
    audioLabelNew,
    audioLabelNewWith,
    audioNew,
    audioNewWith,
  )
where

import Control.Concurrent.MVar (MVar, readMVar)
import Control.Concurrent.STM.TChan (TChan)
import Control.Monad (void, when)
import Control.Monad.IO.Class
import Data.Char (ord)
import Data.Default (Default (..))
import qualified Data.Text as T
import qualified GI.GLib as G
import qualified GI.Gdk.Enums as Gdk
import qualified GI.Gdk.Structs.EventScroll as GdkEvent
import qualified GI.Gtk as Gtk
import System.Taffybar.Context (TaffyIO)
import System.Taffybar.Information.Audio
import System.Taffybar.Util (postGUIASync)
import System.Taffybar.Widget.Generic.ChannelWidget
import System.Taffybar.Widget.Util (buildIconLabelBox, widgetSetClassGI)
import Text.StringTemplate

data AudioWidgetConfig = AudioWidgetConfig
  { audioPulseSink :: String,
    audioWirePlumberNode :: String,
    audioFormat :: String,
    audioMuteFormat :: String,
    audioUnknownFormat :: String,
    audioTooltipFormat :: Maybe String,
    audioScrollStepPercent :: Maybe Int,
    audioToggleMuteOnClick :: Bool
  }

defaultAudioWidgetConfig :: AudioWidgetConfig
defaultAudioWidgetConfig =
  AudioWidgetConfig
    { audioPulseSink = defaultAudioPulseSink,
      audioWirePlumberNode = defaultAudioWirePlumberNode,
      audioFormat = "$volume$%",
      audioMuteFormat = "muted",
      audioUnknownFormat = "n/a",
      audioTooltipFormat =
        Just "Backend: $backend$\nNode: $node$\nVolume: $volume$%\nMuted: $muted$",
      audioScrollStepPercent = Just 5,
      audioToggleMuteOnClick = True
    }

instance Default AudioWidgetConfig where
  def = defaultAudioWidgetConfig

audioIconNew :: TaffyIO Gtk.Widget
audioIconNew = audioIconNewWith defaultAudioWidgetConfig

audioIconNewWith :: AudioWidgetConfig -> TaffyIO Gtk.Widget
audioIconNewWith config = do
  let pulseSink = audioPulseSink config
      wirePlumberNode = audioWirePlumberNode config
  (chan, var) <- getAudioInfoChanAndVar pulseSink wirePlumberNode
  audioIconNewWithState chan var

audioIconNewWithState ::
  TChan (Maybe AudioInfo) ->
  MVar (Maybe AudioInfo) ->
  TaffyIO Gtk.Widget
audioIconNewWithState chan var =
  liftIO $ do
    label <- Gtk.labelNew Nothing
    _ <- widgetSetClassGI label "audio-icon"
    let updateIcon info = do
          let iconText = case info of
                Nothing -> T.pack "\xF026"
                Just i -> audioTextIcon (audioMuted i) (audioVolumePercent i)
          markup <- escapeIconText iconText
          postGUIASync $ Gtk.labelSetMarkup label $ T.pack markup
    void $ Gtk.onWidgetRealize label $ readMVar var >>= updateIcon
    Gtk.widgetShowAll label
    (Gtk.toWidget =<< channelWidgetNew label chan updateIcon)
      >>= (`widgetSetClassGI` "audio-icon")

audioLabelNew :: TaffyIO Gtk.Widget
audioLabelNew = audioLabelNewWith defaultAudioWidgetConfig

audioLabelNewWith :: AudioWidgetConfig -> TaffyIO Gtk.Widget
audioLabelNewWith config = do
  let pulseSink = audioPulseSink config
      wirePlumberNode = audioWirePlumberNode config
  (chan, var) <- getAudioInfoChanAndVar pulseSink wirePlumberNode
  audioLabelNewWithState config chan var

audioLabelNewWithState ::
  AudioWidgetConfig ->
  TChan (Maybe AudioInfo) ->
  MVar (Maybe AudioInfo) ->
  TaffyIO Gtk.Widget
audioLabelNewWithState config chan var = do
  let pulseSink = audioPulseSink config
      wirePlumberNode = audioWirePlumberNode config

  liftIO $ do
    label <- Gtk.labelNew Nothing
    _ <- widgetSetClassGI label "audio-label"

    let updateLabel info = do
          (labelText, tooltipText) <- formatAudioWidget config info
          postGUIASync $ do
            Gtk.labelSetMarkup label labelText
            Gtk.widgetSetTooltipMarkup label tooltipText

        refreshNow = getAudioInfo pulseSink wirePlumberNode >>= updateLabel

        whenToggleMute widget =
          when (audioToggleMuteOnClick config) $
            void $
              Gtk.onWidgetButtonPressEvent widget $ \_ -> do
                void $ toggleAudioMute pulseSink wirePlumberNode
                refreshNow
                return True

        whenScrollAdjust widget =
          case audioScrollStepPercent config of
            Nothing -> return ()
            Just step | step <= 0 -> return ()
            Just step -> do
              _ <- Gtk.onWidgetScrollEvent widget $ \scrollEvent -> do
                dir <- GdkEvent.getEventScrollDirection scrollEvent
                let doAdjust delta = do
                      void $ adjustAudioVolume pulseSink wirePlumberNode delta
                      refreshNow
                      return True
                case dir of
                  Gdk.ScrollDirectionUp -> doAdjust step
                  Gdk.ScrollDirectionDown -> doAdjust (-step)
                  Gdk.ScrollDirectionLeft -> doAdjust step
                  Gdk.ScrollDirectionRight -> doAdjust (-step)
                  _ -> return False
              return ()

    void $ Gtk.onWidgetRealize label $ readMVar var >>= updateLabel

    whenToggleMute label
    whenScrollAdjust label

    Gtk.widgetShowAll label
    (Gtk.toWidget =<< channelWidgetNew label chan updateLabel)
      >>= (`widgetSetClassGI` "audio-label")

audioNew :: TaffyIO Gtk.Widget
audioNew = audioNewWith defaultAudioWidgetConfig

audioNewWith :: AudioWidgetConfig -> TaffyIO Gtk.Widget
audioNewWith config = do
  let pulseSink = audioPulseSink config
      wirePlumberNode = audioWirePlumberNode config
  (chan, var) <- getAudioInfoChanAndVar pulseSink wirePlumberNode
  iconWidget <- audioIconNewWithState chan var
  labelWidget <- audioLabelNewWithState config chan var
  liftIO $
    buildIconLabelBox iconWidget labelWidget
      >>= (`widgetSetClassGI` "audio")

formatAudioWidget ::
  AudioWidgetConfig ->
  Maybe AudioInfo ->
  IO (T.Text, Maybe T.Text)
formatAudioWidget config info = do
  attrs <- maybe buildUnknownAttrs buildAttrs info
  let labelTemplate =
        case info of
          Nothing -> audioUnknownFormat config
          Just audio ->
            case audioMuted audio of
              Just True -> audioMuteFormat config
              _ -> audioFormat config
      labelText = renderTemplate labelTemplate attrs
      tooltipText = fmap (`renderTemplate` attrs) (audioTooltipFormat config)
  return (T.pack labelText, T.pack <$> tooltipText)

buildAttrs :: AudioInfo -> IO [(String, String)]
buildAttrs info = do
  let volumeText = maybe "?" show (audioVolumePercent info)
      mutedText = case audioMuted info of
        Just True -> "yes"
        Just False -> "no"
        Nothing -> "unknown"
      nodeText = audioNodeName info
      backendText = case audioBackend info of
        PulseAudioBackend -> "pulseaudio"
        WirePlumberBackend -> "wireplumber"
      iconText = audioTextIcon (audioMuted info) (audioVolumePercent info)
  volume <- escapeText $ T.pack volumeText
  muted <- escapeText $ T.pack mutedText
  node <- escapeText $ T.pack nodeText
  backend <- escapeText $ T.pack backendText
  icon <- escapeIconText iconText
  return
    [ ("volume", volume),
      ("muted", muted),
      ("node", node),
      ("backend", backend),
      ("icon", icon)
    ]

buildUnknownAttrs :: IO [(String, String)]
buildUnknownAttrs = do
  icon <- escapeIconText (T.pack "\xF026")
  return
    [ ("volume", "?"),
      ("muted", "unknown"),
      ("node", "unknown"),
      ("backend", "unknown"),
      ("icon", icon)
    ]

audioTextIcon :: Maybe Bool -> Maybe Int -> T.Text
audioTextIcon muted volumePercent =
  case muted of
    Just True -> T.pack "\xF026"
    _ ->
      case volumePercent of
        Just v | v <= 0 -> T.pack "\xF026"
        Just v | v <= 33 -> T.pack "\xF027"
        _ -> T.pack "\xF028"

renderTemplate :: String -> [(String, String)] -> String
renderTemplate template attrs = render $ setManyAttrib attrs (newSTMP template)

escapeText :: T.Text -> IO String
escapeText input = T.unpack <$> G.markupEscapeText input (-1)

escapeIconText :: T.Text -> IO String
escapeIconText input =
  let iconSpan s =
        "<span font_family=\"Iosevka Nerd Font\" font_weight=\"normal\" size=\"large\">" ++ s ++ "</span>"
   in do
        rendered <-
          concat
            <$> mapM
              ( \c -> do
                  esc <- escapeText (T.singleton c)
                  pure $
                    if isPUA c
                      then iconSpan esc
                      else esc
              )
              (T.unpack input)
        pure $
          if any isPUA (T.unpack input)
            then rendered ++ iconSpan "&#x2004;"
            else rendered

isPUA :: Char -> Bool
isPUA c =
  let o = ord c
   in o >= 0xE000 && o <= 0xF8FF
