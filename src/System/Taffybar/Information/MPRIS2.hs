{-# LANGUAGE NoMonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      : System.Taffybar.Information.MPRIS2
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan A. Malison
-- Stability   : unstable
-- Portability : unportable
--
-----------------------------------------------------------------------------

module System.Taffybar.Information.MPRIS2 where

import           Control.Applicative
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Maybe
import qualified DBus
import qualified DBus.Client as DBus
import qualified DBus.Internal.Types as DBus
import qualified DBus.TH as DBus
import           Data.Coerce
import           Data.List
import qualified Data.Map as M
import           Data.Maybe
import           System.Log.Logger
import           System.Taffybar.DBus.Client.MPRIS2
import           Text.Printf

data NowPlaying = NowPlaying
  { npTitle :: String
  , npArtists :: [String]
  , npStatus :: String
  , npBusName :: DBus.BusName
  } deriving (Show, Eq)

eitherToMaybeWithLog :: (MonadIO m, Show a1) => Either a1 a2 -> m (Maybe a2)
eitherToMaybeWithLog (Right v) = return $ Just v
eitherToMaybeWithLog (Left e) = liftIO $ do
  logM "System.Taffybar.Information.MPRIS2" WARNING $
       printf "Got error: %s" $ show e
  return Nothing

getNowPlayingInfo :: MonadIO m => DBus.Client -> m [NowPlaying]
getNowPlayingInfo client =
  fmap (fromMaybe []) $ eitherToMaybeWithLog =<< liftIO (runExceptT $ do
    allBusNames <- ExceptT $ DBus.listNames client
    let mediaPlayerBusNames =
          filter (isPrefixOf "org.mpris.MediaPlayer2.") allBusNames
        getSongData _busName = runMaybeT $
          do
            let busName = coerce _busName
            metadataMap <-
              MaybeT $ getMetadata client busName >>= eitherToMaybeWithLog
            (title, artists) <- MaybeT $ return $ getSongInfo metadataMap
            status <- MaybeT $ getPlaybackStatus client busName >>=
                               eitherToMaybeWithLog
            return NowPlaying { npTitle = title
                              , npArtists = artists
                              , npStatus = status
                              , npBusName = busName
                              }
    lift $ catMaybes <$> mapM getSongData mediaPlayerBusNames)

getSongInfo :: M.Map String DBus.Variant -> Maybe (String, [String])
getSongInfo songData = do
  let lookupVariant k = M.lookup k songData >>= DBus.fromVariant
  artists <- lookupVariant "xesam:artist" <|> pure []
  title <- lookupVariant "xesam:title"
  return (title, artists)
