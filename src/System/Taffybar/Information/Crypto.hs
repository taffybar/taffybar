{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      : System.Taffybar.Information.Crypto
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan A. Malison
-- Stability   : unstable
-- Portability : unportable
--
-- This module provides utility functions for retrieving data about crypto
-- assets.
-----------------------------------------------------------------------------
module System.Taffybar.Information.Crypto where

import           BroadcastChan
import           Control.Concurrent
import           Control.Exception.Enclosed (catchAny)
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Aeson.Types (parseMaybe)
import qualified Data.Aeson.Key as Key
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.UTF8 as BS
import qualified Data.Map as M
import           Data.Maybe
import           Data.Proxy
import qualified Data.Text as T
import           GHC.TypeLits
import           Network.HTTP.Simple hiding (Proxy)
import           System.Log.Logger
import           System.Taffybar.Context
import           System.Taffybar.Util
import           Text.Printf

getSymbolToCoinGeckoId :: MonadIO m => m (M.Map T.Text T.Text)
getSymbolToCoinGeckoId = do
    let uri = "https://api.coingecko.com/api/v3/coins/list?include_platform=false"
        request = parseRequest_ uri
    bodyText <- liftIO $ catchAny (getResponseBody <$> httpLBS request) $ \e -> do
                           liftIO $ logM "System.Taffybar.Information.Crypto" WARNING $
                                  printf "Error fetching coins list from coin gecko %s" $ show e
                           return ""
    let coinInfos :: [CoinGeckoInfo]
        coinInfos = fromMaybe [] $ decode bodyText

    return $ M.fromList $ map (\CoinGeckoInfo { identifier = theId, symbol = theSymbol } ->
                        (theSymbol, theId)) coinInfos

newtype SymbolToCoinGeckoId = SymbolToCoinGeckoId (M.Map T.Text T.Text)

newtype CryptoPriceInfo = CryptoPriceInfo { lastPrice :: Double }

newtype CryptoPriceChannel (a :: Symbol) =
  CryptoPriceChannel (BroadcastChan In CryptoPriceInfo, MVar CryptoPriceInfo)

getCryptoPriceChannel :: KnownSymbol a => TaffyIO (CryptoPriceChannel a)
getCryptoPriceChannel = do
  -- XXX: This is a gross hack that is needed to avoid deadlock
  symbolToId <- getStateDefault $ SymbolToCoinGeckoId <$> getSymbolToCoinGeckoId
  getStateDefault $ buildCryptoPriceChannel (60.0 :: Double) symbolToId

data CoinGeckoInfo =
  CoinGeckoInfo { identifier :: T.Text, symbol :: T.Text }
  deriving (Show)

instance FromJSON CoinGeckoInfo where
  parseJSON = withObject "CoinGeckoInfo" (\v -> CoinGeckoInfo <$> v .: "id" <*> v .: "symbol")

buildCryptoPriceChannel ::
  forall a. KnownSymbol a => Double -> SymbolToCoinGeckoId ->  TaffyIO (CryptoPriceChannel a)
buildCryptoPriceChannel delay (SymbolToCoinGeckoId symbolToId) = do
  let initialBackoff = delay
  chan <- newBroadcastChan
  var <- liftIO $ newMVar $ CryptoPriceInfo 0.0
  backoffVar <- liftIO $ newMVar initialBackoff

  let doWrites info = do
        _ <- swapMVar var info
        _ <- writeBChan chan info
        _ <- swapMVar backoffVar initialBackoff
        return ()

  let symbolPair = T.pack $ symbolVal (Proxy :: Proxy a)
      (symbolName:inCurrency:_) = T.splitOn "-" symbolPair

  case M.lookup (T.toLower symbolName) symbolToId of
    Nothing -> liftIO $ logM "System.Taffybar.Information.Crypto"
               WARNING $ printf "Symbol %s not found in coin gecko list" symbolName
    Just cgIdentifier ->
      void $ foreverWithVariableDelay $
           catchAny (liftIO $ getLatestPrice cgIdentifier (T.toLower inCurrency) >>=
                            maybe (return ()) (doWrites . CryptoPriceInfo) >> return delay) $ \e -> do
                                     logPrintF "System.Taffybar.Information.Crypto"
                                               WARNING "Error when fetching crypto price: %s" e
                                     modifyMVar backoffVar $ \current ->
                                       return (min (current * 2) delay, current)

  return $ CryptoPriceChannel (chan, var)

getLatestPrice :: MonadIO m => T.Text -> T.Text -> m (Maybe Double)
getLatestPrice tokenId inCurrency = do
  let uri = printf "https://api.coingecko.com/api/v3/simple/price?ids=%s&vs_currencies=%s"
            tokenId inCurrency
      request = parseRequest_ uri
  bodyText <- getResponseBody <$> httpLBS request
  return $ decode bodyText >>= parseMaybe ((.: Key.fromText tokenId) >=> (.: Key.fromText inCurrency))

getCryptoMeta :: MonadIO m => String -> String -> m LBS.ByteString
getCryptoMeta cmcAPIKey symbolName = do
  let headers = [("X-CMC_PRO_API_KEY", BS.fromString cmcAPIKey)] :: RequestHeaders
      uri = printf "https://pro-api.coinmarketcap.com/v1/cryptocurrency/info?symbol=%s"
            symbolName
      request = setRequestHeaders headers $ parseRequest_ uri
  getResponseBody <$> httpLBS request
