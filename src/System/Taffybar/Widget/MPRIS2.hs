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
-- This is a "Now Playing" widget that listens for MPRIS events on DBus. Various
-- media players implement this. This widget works with version 2 of the MPRIS
-- protocol (https://specifications.freedesktop.org/mpris-spec/latest/).
-----------------------------------------------------------------------------
module System.Taffybar.Widget.MPRIS2 ( mpris2New ) where

import qualified Control.Concurrent.MVar as MV
import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Reader
import           DBus
import           DBus.Client
import           DBus.Internal.Types
import qualified DBus.TH as DBus
import           Data.Coerce
import           Data.Either.Combinators
import           Data.List
import qualified Data.Text as T
import qualified GI.Gtk as Gtk
import qualified Graphics.UI.Gtk as Gtk2hs
import           System.Taffybar.Compat.GtkLibs
import           System.Taffybar.Context
import           System.Taffybar.DBus.Client.MPRIS2
import           System.Taffybar.Information.MPRIS2
import           System.Taffybar.Information.XDG.DesktopEntry
import           System.Taffybar.Util
import           System.Taffybar.Widget.Util
import           System.Log.Logger
import           Text.Printf

data MPRIS2PlayerWidget = MPRIS2PlayerWidget
  { playerLabel :: Gtk.Label
  , playerGrid :: Gtk.Grid
  }

mpris2New :: TaffyIO Gtk2hs.Widget
mpris2New = asks sessionDBusClient >>= \client -> lift $ fromGIWidget =<< do
  grid <- Gtk.gridNew
  alignCenter grid
  playerWidgetsVar <- MV.newMVar []
  let
    newPlayerWidget :: BusName -> IO MPRIS2PlayerWidget
    newPlayerWidget busName =
      do
        -- TODO: Size the image dynamically
        pixbuf <-
          maybe (loadIcon 20 "play.svg") return =<< runMaybeT
          (   MaybeT (rightToMaybe <$> getDesktopEntry client busName)
          >>= MaybeT . getDirectoryEntryDefault
          >>= MaybeT . getImageForDesktopEntry 30
          )

        image <- Gtk.imageNewFromPixbuf $ Just pixbuf
        playerBox <- Gtk.gridNew
        label <- Gtk.labelNew Nothing

        Gtk.containerAdd playerBox image
        Gtk.containerAdd playerBox label
        alignCenter playerBox

        Gtk.containerAdd grid playerBox
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
      where setNowPlaying MPRIS2PlayerWidget {playerLabel = label , playerGrid = playerBox} =
              do
                logPrintF "System.Taffybar.Widget.MPRIS2" DEBUG "Setting state %s" nowPlaying
                Gtk.labelSetMarkup label $ playingText 20 30 nowPlaying
                if status == "Playing"
                then Gtk.widgetShowAll playerBox
                else Gtk.widgetHide playerBox

    updatePlayerWidgets nowPlayings playerWidgets = do
      newWidgets <- foldM updatePlayerWidget playerWidgets nowPlayings
      let existingBusNames = map npBusName nowPlayings
          noInfoPlayerWidgets = filter ((`notElem` existingBusNames) . fst) newWidgets
      mapM_ (Gtk.widgetHide . playerGrid . snd) noInfoPlayerWidgets
      return newWidgets

    updatePlayerWidgetsVar nowPlayings =
      MV.modifyMVar_ playerWidgetsVar (updatePlayerWidgets nowPlayings)

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

playingText :: Int -> Int -> NowPlaying -> T.Text
playingText artistMax songMax NowPlaying { npArtists = artists, npTitle = title } = T.pack $
  Gtk2hs.escapeMarkup $ printf "%s - %s"
       (truncateString artistMax $ intercalate "," artists)
       (truncateString songMax title)
