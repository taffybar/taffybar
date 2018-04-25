{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      : System.Taffybar.Widgets.MPRIS2
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
module System.Taffybar.Widgets.MPRIS2 ( mpris2New ) where

import           Control.Monad.Trans
import           Control.Monad.Trans.Reader
import           DBus.Client hiding ( getProperty )
import qualified DBus.TH as DBus
import           Data.List
import           Graphics.UI.Gtk hiding ( Signal, Variant )
import           System.Taffybar.Information.MPRIS2
import           System.Taffybar.Context
import           System.Taffybar.Util
import           Text.Printf

mpris2New :: TaffyIO Widget
mpris2New = ask >>= \context -> lift $ do
  ebox <- eventBoxNew
  label <- labelNew (Nothing :: Maybe String)
  containerAdd ebox label
  _ <- on label realize $ runReaderT (initLabel label) context
  widgetShowAll ebox
  return (toWidget ebox)

initLabel :: Label -> TaffyIO ()
initLabel label = do
  client <- asks dbusClient
  let doUpdate =
        getNowPlayingInfo client >>= setLabelText label
      signalCallback _ _ _ _ = doUpdate
      propMatcher =
        matchAny
        { matchPath = Just "/org/mpris/MediaPlayer2"
        }
  lift $ do
    handler <- DBus.registerForPropertiesChanged client propMatcher signalCallback
    _ <- on label unrealize (removeMatch client handler)
    doUpdate

setLabelText :: Label -> [NowPlaying] -> IO ()
setLabelText label playingInfos =
  let mfirstPlaying = find ((== "Playing") . npStatus) playingInfos
      playingText :: NowPlaying -> String
      playingText NowPlaying { npArtists = artists, npTitle = title } =
        let textPortion :: String
            textPortion = escapeMarkup $ printf "%s - %s"
                          (truncateString 15 $ intercalate "," artists)
                          (truncateString 30 title)
        in printf "<span fgcolor='yellow'>▶</span> %s" textPortion
      setText np =
        postGUIAsync $ labelSetMarkup label (playingText np) >> widgetShow label
  in maybe (widgetHide label) setText mfirstPlaying
