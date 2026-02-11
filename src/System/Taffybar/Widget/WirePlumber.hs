{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------

-- |
-- Module      : System.Taffybar.Widget.WirePlumber
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan A. Malison
-- Stability   : unstable
-- Portability : unportable
--
-- Volume widget using WirePlumber's @wpctl@ command-line tool.
--
-- This widget displays the current volume level and mute state, and supports
-- scroll to adjust volume and click to toggle mute.
--
-- Three widget variants are provided:
--
-- * 'wirePlumberIconNew' – icon only (dynamic, updates with volume\/mute state)
-- * 'wirePlumberLabelNew' – text label only (with scroll\/click interaction)
-- * 'wirePlumberNew' – combined icon + label in a horizontal box
--
-- Note: Requires @wpctl@ to be available in PATH.
module System.Taffybar.Widget.WirePlumber
  ( WirePlumberWidgetConfig (..),
    defaultWirePlumberWidgetConfig,
    wirePlumberIconNew,
    wirePlumberIconNewWith,
    wirePlumberLabelNew,
    wirePlumberLabelNewWith,
    wirePlumberNew,
    wirePlumberNewWith,
    wirePlumberSourceNew,
    wirePlumberSourceNewWith,
  )
where

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
import System.Taffybar.Information.WirePlumber
import System.Taffybar.Util (postGUIASync)
import System.Taffybar.Widget.Generic.ChannelWidget
import System.Taffybar.Widget.Util (buildIconLabelBox)
import Text.StringTemplate

-- | Configuration for the WirePlumber widget.
data WirePlumberWidgetConfig = WirePlumberWidgetConfig
  { -- | Node specification for wpctl (e.g., "@DEFAULT_AUDIO_SINK@")
    wirePlumberNode :: String,
    -- | Format template for normal volume display.
    -- Available variables: $icon$, $volume$
    wirePlumberFormat :: String,
    -- | Format template when muted.
    -- Available variables: $icon$, $volume$
    wirePlumberMuteFormat :: String,
    -- | Format template when wpctl is unavailable.
    -- Available variables: $icon$
    wirePlumberUnknownFormat :: String,
    -- | Optional tooltip format template.
    -- Available variables: $node$, $volume$, $muted$
    wirePlumberTooltipFormat :: Maybe String,
    -- | Volume adjustment step when scrolling (percentage).
    -- Set to Nothing to disable scroll adjustment.
    wirePlumberScrollStepPercent :: Maybe Int,
    -- | Whether to toggle mute on click.
    wirePlumberToggleMuteOnClick :: Bool
  }

-- | Default configuration for the WirePlumber widget (default audio sink).
defaultWirePlumberWidgetConfig :: WirePlumberWidgetConfig
defaultWirePlumberWidgetConfig =
  WirePlumberWidgetConfig
    { wirePlumberNode = "@DEFAULT_AUDIO_SINK@",
      wirePlumberFormat = "$volume$%",
      wirePlumberMuteFormat = "muted",
      wirePlumberUnknownFormat = "n/a",
      wirePlumberTooltipFormat =
        Just "Node: $node$\nVolume: $volume$%\nMuted: $muted$",
      wirePlumberScrollStepPercent = Just 5,
      wirePlumberToggleMuteOnClick = True
    }

-- | Default configuration for the WirePlumber widget for microphone/source.
defaultWirePlumberSourceWidgetConfig :: WirePlumberWidgetConfig
defaultWirePlumberSourceWidgetConfig =
  defaultWirePlumberWidgetConfig
    { wirePlumberNode = "@DEFAULT_AUDIO_SOURCE@"
    }

instance Default WirePlumberWidgetConfig where
  def = defaultWirePlumberWidgetConfig

-- | Create a WirePlumber icon-only widget for the default audio sink.
-- The icon updates dynamically based on volume level and mute state.
wirePlumberIconNew :: TaffyIO Gtk.Widget
wirePlumberIconNew = wirePlumberIconNewWith defaultWirePlumberWidgetConfig

-- | Create a WirePlumber icon-only widget with custom configuration.
wirePlumberIconNewWith :: WirePlumberWidgetConfig -> TaffyIO Gtk.Widget
wirePlumberIconNewWith config = do
  let nodeSpec = wirePlumberNode config
  chan <- getWirePlumberInfoChan nodeSpec
  initialInfo <- getWirePlumberInfoState nodeSpec
  liftIO $ do
    label <- Gtk.labelNew Nothing
    let updateIcon info = do
          let iconText = case info of
                Nothing -> T.pack "\xF026"
                Just i ->
                  wirePlumberTextIcon
                    (wirePlumberMuted i)
                    (Just $ round (wirePlumberVolume i * 100))
          postGUIASync $ Gtk.labelSetText label iconText
    void $ Gtk.onWidgetRealize label $ updateIcon initialInfo
    Gtk.widgetShowAll label
    Gtk.toWidget =<< channelWidgetNew label chan updateIcon

-- | Create a WirePlumber label-only widget for the default audio sink.
-- Includes scroll-to-adjust and click-to-mute interaction.
wirePlumberLabelNew :: TaffyIO Gtk.Widget
wirePlumberLabelNew = wirePlumberLabelNewWith defaultWirePlumberWidgetConfig

-- | Create a WirePlumber label-only widget with custom configuration.
wirePlumberLabelNewWith :: WirePlumberWidgetConfig -> TaffyIO Gtk.Widget
wirePlumberLabelNewWith config = do
  let nodeSpec = wirePlumberNode config
  chan <- getWirePlumberInfoChan nodeSpec
  initialInfo <- getWirePlumberInfoState nodeSpec

  liftIO $ do
    label <- Gtk.labelNew Nothing

    let updateLabel info = do
          (labelText, tooltipText) <- formatWirePlumberWidget config info
          postGUIASync $ do
            Gtk.labelSetMarkup label labelText
            Gtk.widgetSetTooltipMarkup label tooltipText

        refreshNow = getWirePlumberInfo nodeSpec >>= updateLabel

        whenToggleMute widget =
          when (wirePlumberToggleMuteOnClick config) $
            void $
              Gtk.onWidgetButtonPressEvent widget $ \_ -> do
                void $ toggleWirePlumberMute nodeSpec
                refreshNow
                return True

        whenScrollAdjust widget =
          case wirePlumberScrollStepPercent config of
            Nothing -> return ()
            Just step | step <= 0 -> return ()
            Just step -> do
              _ <- Gtk.onWidgetScrollEvent widget $ \scrollEvent -> do
                dir <- GdkEvent.getEventScrollDirection scrollEvent
                let doAdjust delta = do
                      void $ adjustWirePlumberVolume nodeSpec delta
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

-- | Create a combined icon + label WirePlumber widget for the default audio sink.
wirePlumberNew :: TaffyIO Gtk.Widget
wirePlumberNew = wirePlumberNewWith defaultWirePlumberWidgetConfig

-- | Create a combined icon + label WirePlumber widget with custom configuration.
wirePlumberNewWith :: WirePlumberWidgetConfig -> TaffyIO Gtk.Widget
wirePlumberNewWith config = do
  iconWidget <- wirePlumberIconNewWith config
  labelWidget <- wirePlumberLabelNewWith config
  liftIO $ buildIconLabelBox iconWidget labelWidget

-- | Create a new WirePlumber widget for the default audio source (microphone).
wirePlumberSourceNew :: TaffyIO Gtk.Widget
wirePlumberSourceNew = wirePlumberNewWith defaultWirePlumberSourceWidgetConfig

-- | Create a new WirePlumber widget for audio source with custom configuration.
wirePlumberSourceNewWith :: WirePlumberWidgetConfig -> TaffyIO Gtk.Widget
wirePlumberSourceNewWith = wirePlumberNewWith

formatWirePlumberWidget ::
  WirePlumberWidgetConfig ->
  Maybe WirePlumberInfo ->
  IO (T.Text, Maybe T.Text)
formatWirePlumberWidget config info = do
  attrs <- maybe buildUnknownAttrs buildAttrs info
  let labelTemplate =
        case info of
          Nothing -> wirePlumberUnknownFormat config
          Just audio ->
            if wirePlumberMuted audio
              then wirePlumberMuteFormat config
              else wirePlumberFormat config
      labelText = renderTemplate labelTemplate attrs
      tooltipText = fmap (`renderTemplate` attrs) (wirePlumberTooltipFormat config)
  return (T.pack labelText, T.pack <$> tooltipText)

buildAttrs :: WirePlumberInfo -> IO [(String, String)]
buildAttrs info = do
  let volumePercent = round (wirePlumberVolume info * 100) :: Int
      volumeText = show volumePercent
      mutedText =
        if wirePlumberMuted info
          then "yes"
          else "no"
      nodeText = T.unpack $ wirePlumberNodeName info
      iconText = wirePlumberTextIcon (wirePlumberMuted info) (Just volumePercent)
  volume <- escapeText $ T.pack volumeText
  muted <- escapeText $ T.pack mutedText
  node <- escapeText $ T.pack nodeText
  icon <- escapeIconText iconText
  return
    [ ("volume", volume),
      ("muted", muted),
      ("node", node),
      ("icon", icon)
    ]

buildUnknownAttrs :: IO [(String, String)]
buildUnknownAttrs = do
  icon <- escapeIconText (T.pack "\xF026") -- volume off icon
  return
    [ ("volume", "?"),
      ("muted", "unknown"),
      ("node", "unknown"),
      ("icon", icon)
    ]

wirePlumberTextIcon :: Bool -> Maybe Int -> T.Text
wirePlumberTextIcon muted volumePercent =
  case muted of
    True -> T.pack "\xF026" -- volume off
    False ->
      case volumePercent of
        Just v | v <= 0 -> T.pack "\xF026" -- volume off
        Just v | v <= 33 -> T.pack "\xF027" -- volume low
        _ -> T.pack "\xF028" -- volume high

renderTemplate :: String -> [(String, String)] -> String
renderTemplate template attrs = render $ setManyAttrib attrs (newSTMP template)

escapeText :: T.Text -> IO String
escapeText input = T.unpack <$> G.markupEscapeText input (-1)

-- Font Awesome / Nerd Font glyphs live in the Private Use Area and can render
-- much smaller than the surrounding text depending on which fallback font gets
-- selected. Force a Nerd Font for PUA glyphs (and bump size slightly) so
-- digits/letters remain untouched.
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
        -- Add a trailing space in the icon's font/size so it doesn't look cramped.
        pure $
          if any isPUA (T.unpack input)
            then rendered ++ iconSpan "&#x2004;" -- three-per-em space
            else rendered

isPUA :: Char -> Bool
isPUA c =
  let o = ord c
   in o >= 0xE000 && o <= 0xF8FF
