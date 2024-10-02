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
import           Data.Text (Text)
import qualified Data.Text as T
import           GHC.TypeLits
import           Network.HTTP.Simple hiding (Proxy)
import           System.Log.Logger
import           System.Taffybar.Context
import           System.Taffybar.Util
import           Text.Printf
import qualified UnliftIO.MVar as MV
import           UnliftIO.Exception (catchAny)

getSymbolToCoinGeckoId :: MonadIO m => m (M.Map Text Text)
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

newtype SymbolToCoinGeckoId = SymbolToCoinGeckoId (M.Map Text Text)

newtype CryptoPriceInfo = CryptoPriceInfo { lastPrice :: Double }

newtype CryptoPriceChannel (a :: Symbol) =
  CryptoPriceChannel (BroadcastChan In CryptoPriceInfo, MV.MVar CryptoPriceInfo)

getCryptoPriceChannel :: KnownSymbol a => TaffyIO (CryptoPriceChannel a)
getCryptoPriceChannel = do
  -- XXX: This is a gross hack that is needed to avoid deadlock
  symbolToId <- getStateDefault $ SymbolToCoinGeckoId <$> getSymbolToCoinGeckoId
  getStateDefault $ buildCryptoPriceChannel (60.0 :: Double) symbolToId

data CoinGeckoInfo =
  CoinGeckoInfo { identifier :: Text, symbol :: Text }
  deriving (Show)

instance FromJSON CoinGeckoInfo where
  parseJSON = withObject "CoinGeckoInfo" (\v -> CoinGeckoInfo <$> v .: "id" <*> v .: "symbol")

logCrypto :: MonadIO m => Priority -> String -> m ()
logCrypto p = liftIO . logM "System.Taffybar.Information.Crypto" p

resolveSymbolPair :: KnownSymbol a => Proxy a -> SymbolToCoinGeckoId -> Either String (Text, Text)
resolveSymbolPair sym symbolToId = do
  (symbolName, inCurrency) <- parseSymbolPair (symbolVal sym)
  cgIdentifier <- lookupSymbolCoinGeckoId symbolToId symbolName
  pure (cgIdentifier, inCurrency)

  where
    parseSymbolPair :: String -> Either String (Text, Text)
    parseSymbolPair symbolPair = case T.splitOn "-" (T.toLower $ T.pack symbolPair) of
      [symbolName, inCurrency] | not (T.null inCurrency) -> Right (symbolName, inCurrency)
      _ -> Left $ printf "Type parameter \"%s\" does not match the form \"ASSET-CURRENCY\"" symbolPair

    lookupSymbolCoinGeckoId :: SymbolToCoinGeckoId -> Text -> Either String Text
    lookupSymbolCoinGeckoId (SymbolToCoinGeckoId m) symbolName = maybeToEither
      (printf "Symbol \"%s\" not found in coin gecko list" (T.unpack symbolName))
      (M.lookup symbolName m)

buildCryptoPriceChannel ::
  forall a. KnownSymbol a => Double -> SymbolToCoinGeckoId -> TaffyIO (CryptoPriceChannel a)
buildCryptoPriceChannel delay symbolToId = do
  let initialBackoff = delay
  chan <- newBroadcastChan
  var <- liftIO $ MV.newMVar $ CryptoPriceInfo 0.0
  backoffVar <- liftIO $ MV.newMVar initialBackoff

  let doWrites info = do
        _ <- MV.swapMVar var info
        _ <- writeBChan chan info
        _ <- MV.swapMVar backoffVar initialBackoff
        return ()

  case resolveSymbolPair (Proxy :: Proxy a) symbolToId of
    Left err -> logCrypto WARNING err
    Right (cgIdentifier, inCurrency) ->
      void $ foreverWithVariableDelay $
           catchAny (liftIO $ getLatestPrice cgIdentifier inCurrency >>=
                            maybe (return ()) (doWrites . CryptoPriceInfo) >> return delay) $ \e -> do
                                     logCrypto WARNING $ printf "Error when fetching crypto price: %s" (show e)
                                     MV.modifyMVar backoffVar $ \current ->
                                       return (min (current * 2) delay, current)

  return $ CryptoPriceChannel (chan, var)

getLatestPrice :: MonadIO m => Text -> Text -> m (Maybe Double)
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
