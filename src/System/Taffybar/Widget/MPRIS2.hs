{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      : System.Taffybar.Widget.MPRIS2
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan A. Malison
-- Stability   : unstable
-- Portability : unportable
--
-- This is a "Now Playing" widget that listens for MPRIS2 events on DBus. You
-- can find the MPRIS2 specification here at
-- (<https://specifications.freedesktop.org/mpris-spec/latest/>).
module System.Taffybar.Widget.MPRIS2 where

import Control.Arrow
import qualified Control.Concurrent.MVar as MV
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import DBus
import DBus.Client
import qualified DBus.TH as DBus
import Data.Default (Default (..))
import Data.GI.Base.Overloading (IsDescendantOf)
import Data.Int
import Data.List
import qualified Data.Map as M
import qualified Data.Text as T
import qualified GI.GLib as G
import GI.GdkPixbuf.Objects.Pixbuf as Gdk
import qualified GI.Gtk as Gtk
import System.Environment.XDG.DesktopEntry
import System.Log.Logger
import System.Taffybar.Context
import qualified System.Taffybar.DBus.Client.MPRIS2 as MPRIS2DBus
import System.Taffybar.Information.MPRIS2
import System.Taffybar.Util
import System.Taffybar.Widget.Generic.AutoSizeImage
import System.Taffybar.Widget.Util
import System.Taffybar.WindowIcon
import Text.Printf

-- | Log helper for this module.
mprisLog :: (MonadIO m, Show t) => Priority -> String -> t -> m ()
mprisLog = logPrintF "System.Taffybar.Widget.MPRIS2"

-- | A type representing a function that produces an IO action that adds the
-- provided widget to some container.
type WidgetAdder a m =
  ( IsDescendantOf Gtk.Widget a,
    MonadIO m,
    Gtk.GObject a
  ) =>
  a -> m ()

-- | The type of a customization function that is used to update a widget with
-- the provided now playing info. The type a should be the internal state used
-- for the widget (typically just references to the child widgets that may need
-- to be updated ). When the provided value is nothing, it means that the widget
-- does not exist yet and it should be instantiated. When the provided
-- NowPlaying value is Nothing, the dbus client is no longer, and typically the
-- widget should be hidden.
type UpdateMPRIS2PlayerWidget a =
  (forall w. WidgetAdder w IO) -> Maybe a -> Maybe NowPlaying -> TaffyIO a

-- | Configuration for an MPRIS2 Widget
data MPRIS2Config a
  = MPRIS2Config
  { -- | A function that will be used to wrap the outer MPRIS2 grid widget
    mprisWidgetWrapper :: Gtk.Widget -> IO Gtk.Widget,
    -- | This function will be called to instantiate and update the player widgets
    -- of each dbus player client. See the docstring for `UpdateMPRIS2PlayerWidget`
    -- for more details.
    updatePlayerWidget :: UpdateMPRIS2PlayerWidget a
  }

-- | Default MPRIS2 widget configuration using 'simplePlayerWidget'.
defaultMPRIS2Config :: MPRIS2Config MPRIS2PlayerWidget
defaultMPRIS2Config =
  MPRIS2Config
    { mprisWidgetWrapper = return,
      updatePlayerWidget = simplePlayerWidget def
    }

-- | Internal widget state for the default simple player renderer.
data MPRIS2PlayerWidget = MPRIS2PlayerWidget
  { playerLabel :: Gtk.Label,
    playerWidget :: Gtk.Widget
  }

data MPRIS2PlayerControlsWidget = MPRIS2PlayerControlsWidget
  { controlsPlayerLabel :: Gtk.Label,
    controlsPlayerWidget :: Gtk.Widget,
    controlsNowPlayingVar :: MV.MVar NowPlaying,
    controlsPreviousButton :: Gtk.Button,
    controlsPlayPauseButton :: Gtk.Button,
    controlsPlayPauseButtonLabel :: Gtk.Label,
    controlsNextButton :: Gtk.Button
  }

defaultMPRIS2ControlsConfig :: MPRIS2Config MPRIS2PlayerControlsWidget
defaultMPRIS2ControlsConfig =
  MPRIS2Config
    { mprisWidgetWrapper = return,
      updatePlayerWidget = simplePlayerWidgetWithControls def
    }

-- | Configuration for 'simplePlayerWidget'.
data SimpleMPRIS2PlayerConfig = SimpleMPRIS2PlayerConfig
  { setNowPlayingLabel :: NowPlaying -> IO T.Text,
    showPlayerWidgetFn :: NowPlaying -> IO Bool
  }

-- | Default 'SimpleMPRIS2PlayerConfig'.
defaultPlayerConfig :: SimpleMPRIS2PlayerConfig
defaultPlayerConfig =
  SimpleMPRIS2PlayerConfig
    { setNowPlayingLabel = playingText 20 30,
      showPlayerWidgetFn =
        \NowPlaying {npStatus = status} -> return $ status /= "Stopped"
    }

instance Default SimpleMPRIS2PlayerConfig where
  def = defaultPlayerConfig

-- | Lift a @Maybe@-producing IO function into an 'ExceptT' with a custom
-- failure message.
makeExcept :: String -> (a -> IO (Maybe b)) -> a -> ExceptT String IO b
makeExcept errorString actionBuilder =
  ExceptT . fmap (maybeToEither errorString) . actionBuilder

-- | Resolve an icon for a player bus name and load it at the requested size.
-- Falls back to a default icon (or a blank pixbuf) on errors.
loadIconAtSize ::
  Client -> BusName -> Int32 -> IO Gdk.Pixbuf
loadIconAtSize client busName size =
  let failure err =
        mprisLog WARNING "Failed to load default image: %s" err
          >> pixBufFromColor size 0
      loadDefault =
        loadIcon size "play.svg" >>= either failure return
      logErrorAndLoadDefault err =
        mprisLog WARNING "Failed to get MPRIS icon: %s" err
          >> mprisLog WARNING "MPRIS failure for: %s" busName
          >> loadDefault
      chromeSpecialCase l@(Left _) =
        if "chrom" `isInfixOf` formatBusName busName
          then Right "google-chrome"
          else l
      chromeSpecialCase x = x
   in either logErrorAndLoadDefault return
        =<< runExceptT
          ( ExceptT (left show . chromeSpecialCase <$> MPRIS2DBus.getDesktopEntry client busName)
              >>= makeExcept
                "Failed to get desktop entry"
                getDirectoryEntryDefault
              >>= makeExcept
                "Failed to get image"
                (getImageForDesktopEntry size)
          )

backIconText :: T.Text
backIconText = "⏮"

playIconText :: T.Text
playIconText = "▶"

pauseIconText :: T.Text
pauseIconText = "⏸"

nextIconText :: T.Text
nextIconText = "⏭"

toggleIconText :: T.Text
toggleIconText = "⏯"

isPlaying :: NowPlaying -> Bool
isPlaying NowPlaying {npStatus = status} = status == "Playing"

canTogglePlayback :: NowPlaying -> Bool
canTogglePlayback nowPlaying = npCanPause nowPlaying || npCanPlay nowPlaying

playPauseIconText :: NowPlaying -> T.Text
playPauseIconText nowPlaying
  | isPlaying nowPlaying && npCanPause nowPlaying = pauseIconText
  | npCanPlay nowPlaying = playIconText
  | otherwise = toggleIconText

runPlayPauseAction :: Client -> NowPlaying -> IO ()
runPlayPauseAction client nowPlaying
  | isPlaying nowPlaying && npCanPause nowPlaying =
      void $ MPRIS2DBus.pause client (npBusName nowPlaying)
  | not (isPlaying nowPlaying) && npCanPlay nowPlaying =
      void $ MPRIS2DBus.play client (npBusName nowPlaying)
  | canTogglePlayback nowPlaying =
      void $ MPRIS2DBus.playPause client (npBusName nowPlaying)
  | otherwise = return ()

updateControlButtons :: MPRIS2PlayerControlsWidget -> NowPlaying -> IO ()
updateControlButtons
  MPRIS2PlayerControlsWidget
    { controlsPreviousButton = previousButton,
      controlsPlayPauseButton = playPauseButton,
      controlsPlayPauseButtonLabel = playPauseButtonLabel,
      controlsNextButton = nextButton
    }
  nowPlaying = do
    Gtk.widgetSetVisible previousButton (npCanGoPrevious nowPlaying)
    Gtk.widgetSetSensitive previousButton (npCanGoPrevious nowPlaying)
    Gtk.widgetSetVisible playPauseButton (canTogglePlayback nowPlaying)
    Gtk.widgetSetSensitive playPauseButton (canTogglePlayback nowPlaying)
    Gtk.labelSetText playPauseButtonLabel (playPauseIconText nowPlaying)
    Gtk.widgetSetVisible nextButton (npCanGoNext nowPlaying)
    Gtk.widgetSetSensitive nextButton (npCanGoNext nowPlaying)

newControlButton :: T.Text -> IO (Gtk.Button, Gtk.Label)
newControlButton iconText = do
  button <- Gtk.buttonNew
  label <- Gtk.labelNew $ Just iconText
  Gtk.containerAdd button label
  Gtk.widgetShowAll button
  return (button, label)

-- | This is the default player widget constructor that is used to build mpris
-- widgets. It provides only an icon and NowPlaying text.
simplePlayerWidget ::
  SimpleMPRIS2PlayerConfig -> UpdateMPRIS2PlayerWidget MPRIS2PlayerWidget
simplePlayerWidget
  _
  _
  (Just p@MPRIS2PlayerWidget {playerWidget = widget})
  Nothing =
    lift $ Gtk.widgetHide widget >> return p
simplePlayerWidget
  c
  addToParent
  Nothing
  np@(Just NowPlaying {npBusName = busName}) = do
    ctx <- ask
    client <- asks sessionDBusClient
    lift $ do
      mprisLog DEBUG "Building widget for %s" busName
      image <- autoSizeImageNew (loadIconAtSize client busName) Gtk.OrientationHorizontal
      playerBox <- Gtk.gridNew
      label <- Gtk.labelNew Nothing
      ebox <- Gtk.eventBoxNew
      _ <-
        Gtk.onWidgetButtonPressEvent ebox $
          const $
            MPRIS2DBus.playPause client busName >> return True
      Gtk.containerAdd playerBox image
      Gtk.containerAdd playerBox label
      Gtk.containerAdd ebox playerBox
      vFillCenter playerBox
      addToParent ebox
      Gtk.widgetSetVexpand playerBox True
      Gtk.widgetSetName playerBox $ T.pack $ formatBusName busName
      Gtk.widgetShowAll ebox
      Gtk.widgetHide ebox
      widget <- Gtk.toWidget ebox
      let widgetData =
            MPRIS2PlayerWidget {playerLabel = label, playerWidget = widget}
      flip runReaderT ctx $
        simplePlayerWidget c addToParent (Just widgetData) np
simplePlayerWidget
  config
  _
  ( Just
      w@MPRIS2PlayerWidget
        { playerLabel = label,
          playerWidget = widget
        }
    )
  (Just nowPlaying) = lift $ do
    mprisLog DEBUG "Setting state %s" nowPlaying
    Gtk.labelSetMarkup label =<< setNowPlayingLabel config nowPlaying
    shouldShow <- showPlayerWidgetFn config nowPlaying
    if shouldShow
      then Gtk.widgetShowAll widget
      else Gtk.widgetHide widget
    return w
simplePlayerWidget _ _ _ _ =
  mprisLog
    WARNING
    "widget update called with no widget or %s"
    ("nowplaying" :: String)
    >> return undefined

-- | This player widget constructor extends the default MPRIS2 row with previous,
-- play/pause, and next buttons.
simplePlayerWidgetWithControls ::
  SimpleMPRIS2PlayerConfig -> UpdateMPRIS2PlayerWidget MPRIS2PlayerControlsWidget
simplePlayerWidgetWithControls
  _
  _
  (Just p@MPRIS2PlayerControlsWidget {controlsPlayerWidget = widget})
  Nothing =
    lift $ Gtk.widgetHide widget >> return p
simplePlayerWidgetWithControls
  c
  addToParent
  Nothing
  np@(Just nowPlaying@NowPlaying {npBusName = busName}) = do
    ctx <- ask
    client <- asks sessionDBusClient
    lift $ do
      mprisLog DEBUG "Building widget for %s" busName
      image <- autoSizeImageNew (loadIconAtSize client busName) Gtk.OrientationHorizontal
      playerBox <- Gtk.boxNew Gtk.OrientationHorizontal 0
      clickArea <- Gtk.boxNew Gtk.OrientationHorizontal 0
      controlsBox <- Gtk.boxNew Gtk.OrientationHorizontal 0
      label <- Gtk.labelNew Nothing
      nowPlayingVar <- MV.newMVar nowPlaying
      (previousButton, _) <- newControlButton backIconText
      (playPauseButton, playPauseButtonLabel) <- newControlButton toggleIconText
      (nextButton, _) <- newControlButton nextIconText
      _ <- widgetSetClassGI controlsBox "mpris-controls"
      _ <- widgetSetClassGI previousButton "mpris-control"
      _ <- widgetSetClassGI previousButton "mpris-control-previous"
      _ <- widgetSetClassGI playPauseButton "mpris-control"
      _ <- widgetSetClassGI playPauseButton "mpris-control-play-pause"
      _ <- widgetSetClassGI nextButton "mpris-control"
      _ <- widgetSetClassGI nextButton "mpris-control-next"
      _ <- Gtk.onButtonClicked previousButton $ do
        currentState <- MV.readMVar nowPlayingVar
        when
          (npCanGoPrevious currentState)
          (void $ MPRIS2DBus.previous client (npBusName currentState))
      _ <- Gtk.onButtonClicked playPauseButton $ do
        currentState <- MV.readMVar nowPlayingVar
        runPlayPauseAction client currentState
      _ <- Gtk.onButtonClicked nextButton $ do
        currentState <- MV.readMVar nowPlayingVar
        when
          (npCanGoNext currentState)
          (void $ MPRIS2DBus.next client (npBusName currentState))
      ebox <- Gtk.eventBoxNew
      _ <-
        Gtk.onWidgetButtonPressEvent ebox $
          const $ do
            currentState <- MV.readMVar nowPlayingVar
            runPlayPauseAction client currentState
            return True
      Gtk.boxPackStart clickArea image False False 0
      Gtk.boxPackStart clickArea label True True 0
      Gtk.containerAdd ebox clickArea
      Gtk.boxPackStart controlsBox previousButton False False 0
      Gtk.boxPackStart controlsBox playPauseButton False False 0
      Gtk.boxPackStart controlsBox nextButton False False 0
      Gtk.boxPackStart playerBox ebox True True 0
      Gtk.boxPackStart playerBox controlsBox False False 0
      vFillCenter playerBox
      addToParent playerBox
      Gtk.widgetSetVexpand playerBox True
      Gtk.widgetSetName playerBox $ T.pack $ formatBusName busName
      Gtk.widgetShowAll playerBox
      Gtk.widgetHide playerBox
      widget <- Gtk.toWidget playerBox
      let widgetData =
            MPRIS2PlayerControlsWidget
              { controlsPlayerLabel = label,
                controlsPlayerWidget = widget,
                controlsNowPlayingVar = nowPlayingVar,
                controlsPreviousButton = previousButton,
                controlsPlayPauseButton = playPauseButton,
                controlsPlayPauseButtonLabel = playPauseButtonLabel,
                controlsNextButton = nextButton
              }
      flip runReaderT ctx $
        simplePlayerWidgetWithControls c addToParent (Just widgetData) np
simplePlayerWidgetWithControls
  config
  _
  ( Just
      w@MPRIS2PlayerControlsWidget
        { controlsPlayerLabel = label,
          controlsPlayerWidget = widget,
          controlsNowPlayingVar = nowPlayingVar
        }
    )
  (Just nowPlaying) = lift $ do
    mprisLog DEBUG "Setting state %s" nowPlaying
    void $ MV.swapMVar nowPlayingVar nowPlaying
    Gtk.labelSetMarkup label =<< setNowPlayingLabel config nowPlaying
    shouldShow <- showPlayerWidgetFn config nowPlaying
    if shouldShow
      then Gtk.widgetShowAll widget >> updateControlButtons w nowPlaying
      else Gtk.widgetHide widget
    return w
simplePlayerWidgetWithControls _ _ _ _ =
  mprisLog
    WARNING
    "widget update called with no widget or %s"
    ("nowplaying" :: String)
    >> return undefined

-- | Construct a new MPRIS2 widget using the `simplePlayerWidget` constructor.
mpris2New :: TaffyIO Gtk.Widget
mpris2New = mpris2NewWithConfig defaultMPRIS2Config

-- | Construct a new MPRIS2 widget with transport control buttons
-- (previous/play-pause/next) when the player advertises support for them.
mpris2NewWithControls :: TaffyIO Gtk.Widget
mpris2NewWithControls = mpris2NewWithConfig defaultMPRIS2ControlsConfig

-- | Construct a new MPRIS2 widget with the provided configuration.
mpris2NewWithConfig :: MPRIS2Config a -> TaffyIO Gtk.Widget
mpris2NewWithConfig config =
  ask >>= \ctx ->
    asks sessionDBusClient >>= \client -> lift $ do
      grid <- Gtk.gridNew
      outerWidget <- Gtk.toWidget grid >>= mprisWidgetWrapper config
      vFillCenter grid
      playerWidgetsVar <- MV.newMVar M.empty
      let updateWidget = updatePlayerWidget config
          updatePlayerWidgets nowPlayings playerWidgets = do
            let updateWidgetFromNP np@NowPlaying {npBusName = busName} =
                  (busName,)
                    <$> updateWidget
                      (Gtk.containerAdd grid)
                      (M.lookup busName playerWidgets)
                      (Just np)
                activeBusNames = map npBusName nowPlayings
                existingBusNames = M.keys playerWidgets
                inactiveBusNames = existingBusNames \\ activeBusNames
                callForNoPlayingAvailable busName =
                  updateWidget
                    (Gtk.containerAdd grid)
                    (M.lookup busName playerWidgets)
                    Nothing

            -- Invoke the widgets with no NowPlaying so they can hide etc.
            mapM_ callForNoPlayingAvailable inactiveBusNames
            -- Update all the other widgets
            updatedWidgets <- M.fromList <$> mapM updateWidgetFromNP nowPlayings
            return $ M.union updatedWidgets playerWidgets

          updatePlayerWidgetsVar nowPlayings =
            postGUISync $
              MV.modifyMVar_ playerWidgetsVar $
                flip runReaderT ctx
                  . updatePlayerWidgets nowPlayings

          setPlayingClass = do
            anyVisible <- anyM Gtk.widgetIsVisible =<< Gtk.containerGetChildren grid
            if anyVisible
              then do
                addClassIfMissing "visible-children" outerWidget
                removeClassIfPresent "no-visible-children" outerWidget
              else do
                addClassIfMissing "no-visible-children" outerWidget
                removeClassIfPresent "visible-children" outerWidget

          doUpdate = do
            nowPlayings <- getNowPlayingInfo client
            updatePlayerWidgetsVar nowPlayings
            setPlayingClass

          signalCallback _ _ _ _ = doUpdate

          propMatcher = matchAny {matchPath = Just "/org/mpris/MediaPlayer2"}

          handleNameOwnerChanged _ name _ _ = do
            playerWidgets <- MV.readMVar playerWidgetsVar
            busName <- parseBusName name
            when (busName `M.member` playerWidgets) doUpdate

      _ <- Gtk.onWidgetRealize grid $ do
        updateHandler <-
          DBus.registerForPropertiesChanged client propMatcher signalCallback
        nameHandler <-
          DBus.registerForNameOwnerChanged client matchAny handleNameOwnerChanged
        doUpdate
        void $
          Gtk.onWidgetUnrealize grid $
            removeMatch client updateHandler >> removeMatch client nameHandler

      Gtk.widgetShow grid
      setPlayingClass
      return outerWidget

-- | Generate now playing text with the artist truncated to a maximum given by
-- the first provided int, and the song title truncated to a maximum given by
-- the second provided int.
playingText :: (MonadIO m) => Int -> Int -> NowPlaying -> m T.Text
playingText artistMax songMax NowPlaying {npArtists = artists, npTitle = title} =
  G.markupEscapeText formattedText (-1)
  where
    truncatedTitle = truncateString songMax title
    formattedText =
      T.pack $
        if null artists
          then truncatedTitle
          else
            printf
              "%s - %s"
              (truncateString artistMax $ intercalate "," artists)
              truncatedTitle
