{-# LANGUAGE OverloadedStrings #-}
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
-----------------------------------------------------------------------------
module System.Taffybar.Widget.MPRIS2 ( mpris2New ) where

import           Control.Arrow
import qualified Control.Concurrent.MVar as MV
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Reader
import           DBus
import           DBus.Client
import           DBus.Internal.Types
import qualified DBus.TH as DBus
import           Data.Coerce
import           Data.List
import qualified Data.Text as T
import qualified GI.Gtk as Gtk
import qualified GI.GLib as G
import           System.Log.Logger
import           System.Taffybar.Context
import           System.Taffybar.DBus.Client.MPRIS2
import           System.Taffybar.Information.MPRIS2
import           System.Taffybar.Information.XDG.DesktopEntry
import           System.Taffybar.Util
import           System.Taffybar.Widget.Generic.AutoSizeImage
import           System.Taffybar.Widget.Util
import           Text.Printf

mprisLog :: (MonadIO m, Show t) => Priority -> String -> t -> m ()
mprisLog = logPrintF "System.Taffybar.Widget.MPRIS2"

data MPRIS2PlayerWidget = MPRIS2PlayerWidget
  { playerLabel :: Gtk.Label
  , playerGrid :: Gtk.Grid
  }

mpris2New :: TaffyIO Gtk.Widget
mpris2New = asks sessionDBusClient >>= \client -> lift $ do
  grid <- Gtk.gridNew
  vFillCenter grid
  playerWidgetsVar <- MV.newMVar []
  let
    newPlayerWidget :: BusName -> IO MPRIS2PlayerWidget
    newPlayerWidget busName =
      do
        let logErrorAndLoadDefault size err =
              mprisLog WARNING "Failed to get MPRIS icon: %s" err >>
              mprisLog WARNING "MPRIS failure for: %s" busName >>
                       loadIcon size "play.svg"
            makeExcept ::
              String -> (a -> IO (Maybe b)) -> a -> ExceptT String IO b
            makeExcept errorString actionBuilder =
              ExceptT . fmap (maybeToEither errorString) . actionBuilder
            loadIconAtSize size =
              either (logErrorAndLoadDefault size) return =<< runExceptT
              (   ExceptT (left show <$> getDesktopEntry client busName)
              >>= makeExcept "Failed to get desktop entry"
                  getDirectoryEntryDefault
              >>= makeExcept "Failed to get image"
                    (getImageForDesktopEntry size)
              )

        image <- autoSizeImageNew loadIconAtSize Gtk.OrientationHorizontal
        playerBox <- Gtk.gridNew
        label <- Gtk.labelNew Nothing

        Gtk.containerAdd playerBox image
        Gtk.containerAdd playerBox label
        vFillCenter playerBox

        Gtk.containerAdd grid playerBox
        Gtk.widgetSetVexpand playerBox True
        Gtk.widgetHide playerBox
        return MPRIS2PlayerWidget {playerLabel = label, playerGrid = playerBox}

    updatePlayerWidget
      children
      nowPlaying@NowPlaying
                  { npBusName = busName
                  , npStatus = status
                  } =
      case lookup busName children of
        Nothing -> do
          playerWidget <- newPlayerWidget busName
          setNowPlaying playerWidget
          return $ (busName, playerWidget):children
        Just playerWidget -> setNowPlaying playerWidget >> return children
      where setNowPlaying
              MPRIS2PlayerWidget
              { playerLabel = label
              , playerGrid = playerBox
              } = do
                logPrintF "System.Taffybar.Widget.MPRIS2"
                          DEBUG "Setting state %s" nowPlaying
                Gtk.labelSetMarkup label =<< playingText 20 30 nowPlaying
                if status == "Playing"
                then Gtk.widgetShowAll playerBox
                else Gtk.widgetHide playerBox

    updatePlayerWidgets nowPlayings playerWidgets = do
      newWidgets <- foldM updatePlayerWidget playerWidgets nowPlayings
      let existingBusNames = map npBusName nowPlayings
          noInfoPlayerWidgets =
            filter ((`notElem` existingBusNames) . fst) newWidgets
      mapM_ (Gtk.widgetHide . playerGrid . snd) noInfoPlayerWidgets
      return newWidgets

    updatePlayerWidgetsVar nowPlayings = postGUIASync $
      MV.modifyMVar_ playerWidgetsVar $ updatePlayerWidgets nowPlayings

    doUpdate = getNowPlayingInfo client >>= updatePlayerWidgetsVar
    signalCallback _ _ _ _ = doUpdate
    propMatcher = matchAny { matchPath = Just "/org/mpris/MediaPlayer2" }

    handleNameOwnerChanged _ name _ _ = do
      busNames <- map (coerce . fst) <$> MV.readMVar playerWidgetsVar
      when (name `elem` busNames) doUpdate

  _ <- Gtk.onWidgetRealize grid $ do
    updateHandler <-
      DBus.registerForPropertiesChanged client propMatcher signalCallback
    nameHandler <-
      DBus.registerForNameOwnerChanged client matchAny handleNameOwnerChanged
    doUpdate
    void $ Gtk.onWidgetUnrealize grid $
         removeMatch client updateHandler >> removeMatch client nameHandler
  Gtk.widgetShow grid
  Gtk.toWidget grid

playingText :: MonadIO m => Int -> Int -> NowPlaying -> m T.Text
playingText artistMax songMax NowPlaying {npArtists = artists, npTitle = title} =
  G.markupEscapeText formattedText (-1)
  where formattedText = T.pack $ printf
           "%s - %s"
           (truncateString artistMax $ intercalate "," artists)
           (truncateString songMax title)
