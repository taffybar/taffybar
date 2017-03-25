{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | This is a "Now Playing"-style widget that listens for MPRIS
-- events on DBus.  Various media players implement this.  This widget
-- only works with version 1 of the MPRIS protocol
-- (http://www.mpris.org/1.0/spec.html).  Support for version 2 will
-- be in a separate widget.
module System.Taffybar.MPRIS
  ( TrackInfo (..)
  , MPRISConfig (..)
  , defaultMPRISConfig
  , mprisNew
  ) where

import Control.Monad ( void )
import Data.Int ( Int32 )
import qualified Data.Map as M
import Data.Text ( Text )
import qualified Data.Text as T
import DBus
import DBus.Client
import Graphics.UI.Gtk hiding ( Signal, Variant )
import Text.Printf

data TrackInfo = TrackInfo
  { trackArtist :: Maybe String -- ^ Artist name, if available.
  , trackTitle  :: Maybe String -- ^ Track name, if available.
  , trackAlbum  :: Maybe String -- ^ Album name, if available.
  }

data MPRISConfig = MPRISConfig
  { trackLabel :: TrackInfo -> String -- ^ Calculate a label to display.
  }

setupDBus :: MPRISConfig -> Label -> IO ()
setupDBus cfg w = do
  let trackMatcher = matchAny { matchSender = Nothing
                               , matchDestination = Nothing
                               , matchPath = Just "/Player"
                               , matchInterface = Just "org.freedesktop.MediaPlayer"
                               , matchMember = Just "TrackChange"
                               }
      stateMatcher = matchAny { matchSender = Nothing
                               , matchDestination = Nothing
                               , matchPath = Just "/Player"
                               , matchInterface = Just "org.freedesktop.MediaPlayer"
                               , matchMember = Just "StatusChange"
                               }
  client <- connectSession
  void $ addMatch client trackMatcher (trackCallback cfg w)
  void $ addMatch client stateMatcher (stateCallback w)

variantDictLookup :: (IsVariant b, Ord k) => k -> M.Map k Variant -> Maybe b
variantDictLookup k m = do
  val <- M.lookup k m
  fromVariant val


trackCallback :: MPRISConfig -> Label -> Signal -> IO ()
trackCallback cfg w s = do
  let v :: Maybe (M.Map Text Variant)
      v = fromVariant variant
      [variant] = signalBody s
  case v of
    Just m -> do
      let getInfo key = fmap (escapeMarkup . T.unpack) $ variantDictLookup key m
          txt = trackLabel cfg info
          info = TrackInfo { trackArtist = getInfo "artist"
                           , trackTitle  = getInfo "title"
                           , trackAlbum  = getInfo "album"
                           }
      postGUIAsync $ do
        -- In case the widget was hidden due to a stop/pause, forcibly
        -- show it again when the track changes.
        labelSetMarkup w txt
        widgetShowAll w
    _ -> return ()

stateCallback :: Label -> Signal -> IO ()
stateCallback w s =
  case fromVariant (signalBody s !! 0) of
    Just st -> case structureItems st of
      (pstate:_) -> case (fromVariant pstate) :: Maybe Int32 of
        Just 2 -> postGUIAsync $ widgetHideAll w
        Just 1 -> postGUIAsync $ widgetHideAll w
        Just 0 -> postGUIAsync $ widgetShowAll w
        _ -> return ()
      _ -> return ()
    _ -> return ()

defaultMPRISConfig :: MPRISConfig
defaultMPRISConfig = MPRISConfig
  { trackLabel = display
  }
  where artist track  = maybe "[unknown]" id (trackArtist track)
        title  track  = maybe "[unknown]" id (trackTitle  track)
        display :: TrackInfo -> String
        display track = "<span fgcolor='yellow'>▶</span> " ++
                        printf "%s - %s" (artist track) (title track)

mprisNew :: MPRISConfig -> IO Widget
mprisNew cfg = do
  l <- labelNew (Nothing :: Maybe String)

  _ <- on l realize $ setupDBus cfg l
  widgetShowAll l
  return (toWidget l)
