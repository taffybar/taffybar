{-# LANGUAGE OverloadedStrings #-}
-- | This is a "Now Playing"-style widget that listens for MPRIS
-- events on DBus.  Various media players implement this.  This widget
-- only works with version 1 of the MPRIS protocol
-- (http://www.mpris.org/1.0/spec.html).  Support for version 2 will
-- be in a separate widget.
--
-- This widget isn't as configurable as the others yet - that will be
-- fixed.
module System.Taffybar.MPRIS ( mprisNew ) where

import Data.Int ( Int32 )
import qualified Data.Map as M
import Data.Text ( Text )
import qualified Data.Text as T
import DBus.Client.Simple ( connectSession )
import DBus.Client
import DBus.Types
import DBus.Message
import Graphics.UI.Gtk hiding ( Signal, Variant )
import Web.Encodings ( encodeHtml, decodeHtml )
import Text.Printf

setupDBus :: Label -> IO ()
setupDBus w = do
  let trackMatcher = MatchRule { matchSender = Nothing
                               , matchDestination = Nothing
                               , matchPath = Just "/Player"
                               , matchInterface = Just "org.freedesktop.MediaPlayer"
                               , matchMember = Just "TrackChange"
                               }
      stateMatcher = MatchRule { matchSender = Nothing
                               , matchDestination = Nothing
                               , matchPath = Just "/Player"
                               , matchInterface = Just "org.freedesktop.MediaPlayer"
                               , matchMember = Just "StatusChange"
                               }
  client <- connectSession
  listen client trackMatcher (trackCallback w)
  listen client stateMatcher (stateCallback w)

variantDictLookup :: (IsVariant b, Ord k) => k -> M.Map k Variant -> Maybe b
variantDictLookup k m = do
  val <- M.lookup k m
  fromVariant val


trackCallback :: Label -> BusName -> Signal -> IO ()
trackCallback w _ Signal { signalBody = [variant] } = do
  let v :: Maybe (M.Map Text Variant)
      v = fromVariant variant
  case v of
    Just m -> do
      let artist = maybe "[unknown]" id (variantDictLookup "artist" m)
          track = maybe "[unknown]" id (variantDictLookup "title" m)
          msg = encodeHtml $ decodeHtml $ printf "%s - %s" (T.unpack artist) (T.unpack track)
          txt = "<span fgcolor='yellow'>Now Playing:</span> " ++ msg
      postGUIAsync $ do
        -- In case the widget was hidden due to a stop/pause, forcibly
        -- show it again when the track changes.
        labelSetMarkup w txt
        widgetShowAll w
    _ -> return ()
trackCallback _ _ _ = return ()

stateCallback :: Label -> BusName -> Signal -> IO ()
stateCallback w _ Signal { signalBody = [bdy] } =
  case fromVariant bdy of
    Just st -> case structureItems st of
      (pstate:_) -> case (fromVariant pstate) :: Maybe Int32 of
        Just 2 -> postGUIAsync $ widgetHideAll w
        Just 1 -> postGUIAsync $ widgetHideAll w
        Just 0 -> postGUIAsync $ widgetShowAll w
        _ -> return ()
      _ -> return ()
    _ -> return ()
stateCallback _ _ _ = return ()

mprisNew :: IO Widget
mprisNew = do
  l <- labelNew Nothing

  _ <- on l realize $ setupDBus l
  widgetShowAll l
  return (toWidget l)
