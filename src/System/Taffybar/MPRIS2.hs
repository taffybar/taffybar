{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      : System.Taffybar.MPRIS2
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
module System.Taffybar.MPRIS2 ( mpris2New ) where

import Control.Monad ( void )
import Control.Monad.Trans
import DBus
import DBus.Client hiding ( getProperty )
import Data.List ( isPrefixOf )
import Data.Maybe
import Graphics.UI.Gtk hiding ( Signal, Variant )
import Text.Printf

mpris2New :: MonadIO m => m Widget
mpris2New = liftIO $ do
  label <- labelNew (Nothing :: Maybe String)
  widgetHide label
  _ <- on label realize $ initLabel label
  return (toWidget label)

unpack :: IsVariant a => Variant -> a
unpack var = fromMaybe theError $ fromVariant var
  where theError = error ("Could not unpack variant: " ++ show var)

initLabel :: Label -> IO ()
initLabel w = do
  client <- connectSession
  -- Set initial song state/info
  reqSongInfo w client
  void $ addMatch client propMatcher (callBack w)
  return ()
  where
    callBack label s = do
      let items = dictionaryItems $ unpack (signalBody s !! 1)
      updatePlaybackStatus label items
      updateMetadata label items
      return ()
    propMatcher =
      matchAny
      { matchSender = Nothing
      , matchDestination = Nothing
      , matchPath = Just "/org/mpris/MediaPlayer2"
      , matchInterface = Just "org.freedesktop.DBus.Properties"
      , matchMember = Just "PropertiesChanged"
      }

reqSongInfo :: Label -> Client -> IO ()
reqSongInfo w client = do
  rep <-
    call_
      client
      (methodCall "/org/freedesktop/DBus" "org.freedesktop.DBus" "ListNames")
      {methodCallDestination = Just "org.freedesktop.DBus"}
  let plist = unpack $ head $ methodReturnBody rep
  let players = filter (isPrefixOf "org.mpris.MediaPlayer2.") plist
  case length players of
    0 -> return ()
    _ -> do
      reply <- getProperty client (head players) "Metadata"
      updateSongInfo w $
        dictionaryItems $ (unpack . unpack) (head $ methodReturnBody reply)
      reply' <- getProperty client (head players) "PlaybackStatus"
      let status = (unpack . unpack) (head $ methodReturnBody reply') :: String
      case status of
        "Playing" -> postGUIAsync $ widgetShow w
        "Paused" -> postGUIAsync $ widgetHide w
        "Stopped" -> postGUIAsync $ widgetHide w
        _ -> return ()

getProperty :: Client -> String -> String -> IO MethodReturn
getProperty client name property =
  call_
    client
    (methodCall
       "/org/mpris/MediaPlayer2"
       "org.freedesktop.DBus.Properties"
       "Get")
    { methodCallDestination = Just (busName_ name)
    , methodCallBody =
        [ toVariant ("org.mpris.MediaPlayer2.Player" :: String)
        , toVariant property
        ]
    }

setSongInfo :: Label -> String -> String -> IO ()
setSongInfo w artist title = do
  let msg :: String
      msg = case artist of
        "" -> escapeMarkup $ printf "%s" (truncateString 30 title)
        _ ->  escapeMarkup $ printf "%s - %s" (truncateString 15 artist) (truncateString 30 title)
      txt = "<span fgcolor='yellow'>▶</span> " ++ msg
  postGUIAsync $
    labelSetMarkup w txt

truncateString :: Int -> String -> String
truncateString n xs | length xs <= n = xs
              | otherwise      = take n xs ++ "…"

updatePlaybackStatus :: Label -> [(Variant, Variant)] -> IO ()
updatePlaybackStatus w items =
  case lookup (toVariant ("PlaybackStatus" :: String)) items of
    Just a ->
      case (unpack . unpack) a :: String of
        "Playing" -> postGUIAsync $ widgetShow w
        "Paused"  -> postGUIAsync $ widgetHide w
        "Stopped" -> postGUIAsync $ widgetHide w
        _         -> return ()
    Nothing -> return ()

updateSongInfo :: Label -> [(Variant, Variant)] -> IO ()
updateSongInfo w items = do
  let artist = fromMaybe "" readArtist
  maybe (return ()) (setSongInfo w artist) readTitle
  where
    readArtist :: Maybe String
    readArtist = do
      artist <- lookup (toVariant ("xesam:artist" :: String)) items
      listToMaybe ((unpack . unpack) artist :: [String])
    readTitle :: Maybe String
    readTitle = do
      title <- lookup (toVariant ("xesam:title" :: String)) items
      Just $ (unpack . unpack) title

updateMetadata :: Label -> [(Variant, Variant)] -> IO ()
updateMetadata w items =
  case lookup (toVariant ("Metadata" :: String)) items of
    Just meta -> do
      let metaItems = dictionaryItems $ (unpack . unpack) meta
      updateSongInfo w metaItems
    Nothing -> return ()
