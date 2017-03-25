{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | This is a "Now Playing"-style widget that listens for MPRIS
-- events on DBus.  Various media players implement this.  This widget
-- works with version 2 of the MPRIS protocol
-- (http://www.mpris.org/2.0/spec.html).
--
module System.Taffybar.MPRIS2 ( mpris2New ) where

import Control.Monad ( void )
import Data.Maybe ( listToMaybe )
import DBus
import DBus.Client
import Data.List (isPrefixOf)
import Graphics.UI.Gtk hiding ( Signal, Variant )
import Text.Printf

mpris2New :: IO Widget
mpris2New = do
  label <- labelNew (Nothing :: Maybe String)
  widgetShowAll label
  _ <- on label realize $ initLabel label
  return (toWidget label)

unpack :: IsVariant a => Variant -> a
unpack var = case fromVariant var of
  Just x -> x
  Nothing -> error("Could not unpack variant: " ++ show var)

initLabel :: Label -> IO ()
initLabel w = do
  client <- connectSession
  -- Set initial song state/info
  reqSongInfo w client
  void $ addMatch client propMatcher (callBack w)
  return ()
    where callBack label s = do
            let items = dictionaryItems $ unpack (signalBody s !! 1)
            updatePlaybackStatus label items
            updateMetadata label items
            return ()
          propMatcher = matchAny { matchSender = Nothing
                                 , matchDestination = Nothing
                                 , matchPath = Just "/org/mpris/MediaPlayer2"
                                 , matchInterface = Just "org.freedesktop.DBus.Properties"
                                 , matchMember = Just "PropertiesChanged"
                                 }

reqSongInfo :: Label -> Client -> IO ()
reqSongInfo w client = do
  rep <- call_ client (methodCall "/org/freedesktop/DBus" "org.freedesktop.DBus" "ListNames")
                         { methodCallDestination = Just "org.freedesktop.DBus" }
  let plist = unpack $ methodReturnBody rep !! 0
  let players = filter (isPrefixOf "org.mpris.MediaPlayer2.") plist
  case length players of
    0 -> return ()
    _ -> do
      reply <- getProperty client (players !! 0) "Metadata"
      updateSongInfo w $ dictionaryItems $ (unpack . unpack) (methodReturnBody reply !! 0)
      reply' <- getProperty client (players !! 0) "PlaybackStatus"
      let status = (unpack . unpack) (methodReturnBody reply' !! 0) :: String
      case status of
        "Playing" -> postGUIAsync $ widgetShowAll w
        "Paused"  -> postGUIAsync $ widgetHideAll w
        "Stopped" -> postGUIAsync $ widgetHideAll w
        _         -> return ()

getProperty :: Client -> String -> String -> IO MethodReturn
getProperty client name property = do
  call_ client (methodCall "/org/mpris/MediaPlayer2" "org.freedesktop.DBus.Properties" "Get")
    { methodCallDestination = Just (busName_ name)
    , methodCallBody = [ toVariant ("org.mpris.MediaPlayer2.Player" :: String),
                         toVariant property ]
    }

setSongInfo :: Label -> String -> String -> IO ()
setSongInfo w artist title = do
  let msg :: String
      msg = case artist of
        "" -> escapeMarkup $ printf "%s" (truncateString 30 title)
        _ ->  escapeMarkup $ printf "%s - %s" (truncateString 15 artist) (truncateString 30 title)
      txt = "<span fgcolor='yellow'>▶</span> " ++ msg
  postGUIAsync $ do
    labelSetMarkup w txt

truncateString :: Int -> String -> String
truncateString n xs | length xs <= n = xs
              | otherwise      = take n xs ++ "…"

updatePlaybackStatus :: Label -> [(Variant, Variant)] -> IO ()
updatePlaybackStatus w items = do
  case lookup (toVariant ("PlaybackStatus" :: String)) items of
    Just a -> do
      case (unpack . unpack) a :: String of
        "Playing" -> postGUIAsync $ widgetShowAll w
        "Paused"  -> postGUIAsync $ widgetHideAll w
        "Stopped" -> postGUIAsync $ widgetHideAll w
        _         -> return ()
    Nothing -> do
      return ()

updateSongInfo :: Label -> [(Variant, Variant)] -> IO ()
updateSongInfo w items = do
  let artist = case readArtist of
        Just x -> x
        Nothing -> ""
  case readTitle of
    Just title -> do
      setSongInfo w artist title
    Nothing -> return ()
  where
    readArtist :: Maybe String
    readArtist = do
      artist <- lookup (toVariant ("xesam:artist" :: String)) items
      listToMaybe $ ((unpack . unpack) artist :: [String])
    readTitle :: Maybe String
    readTitle = do
      title <- lookup (toVariant ("xesam:title" :: String)) items
      Just $ (unpack . unpack) title

updateMetadata :: Label -> [(Variant, Variant)] -> IO ()
updateMetadata w items = do
  case lookup (toVariant ("Metadata" :: String)) items of
    Just meta -> do
      let metaItems = dictionaryItems $ (unpack . unpack) meta
      updateSongInfo w metaItems
    Nothing -> return ()
