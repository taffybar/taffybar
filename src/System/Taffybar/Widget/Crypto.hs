{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      : System.Taffybar.Widget.Crypto
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan A. Malison
-- Stability   : unstable
-- Portability : unportable
--
-- This module provides widgets for tracking the price of crypto currency
-- assets.
module System.Taffybar.Widget.Crypto where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader (ask)
import Data.Aeson
import qualified Data.Aeson.Key as Key
import Data.Aeson.Types
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe
import Data.Proxy
import qualified Data.Text
import GHC.TypeLits
import qualified GI.GdkPixbuf.Objects.Pixbuf as Gdk
import qualified GI.Gtk as Gtk
import Network.HTTP.Simple hiding (Proxy)
import System.FilePath.Posix
import System.Taffybar.Context
import System.Taffybar.Information.Crypto hiding (symbol)
import System.Taffybar.Util
import System.Taffybar.Widget.Generic.ChannelWidget
import System.Taffybar.Widget.Generic.ScalingImage (scalingImage)
import System.Taffybar.Widget.Util (buildIconLabelBox, widgetSetClassGI)
import System.Taffybar.WindowIcon
import Text.Printf

-- | Extends 'cryptoPriceLabel' with an icon corresponding to the symbol of the
-- purchase crypto that will appear to the left of the price label. See the
-- docstring for 'getCryptoPixbuf' for details about how this icon is retrieved.
-- Note that automatic icon retrieval requires a coinmarketcap api key to be set
-- at taffybar startup. As with 'cryptoPriceLabel', this function must be
-- invoked with a type application with the type string that expresses the
-- symbol of the relevant token and the underlying currency in which its price
-- should be expressed. See the docstring of 'cryptoPriceLabel' for details
-- about the exact format that this string should take.
cryptoPriceLabelWithIcon :: forall a. (KnownSymbol a) => TaffyIO Gtk.Widget
cryptoPriceLabelWithIcon = do
  label <- cryptoPriceLabel @a
  let symbolPair = symbolVal (Proxy :: Proxy a)
      symbol = takeWhile (/= '-') symbolPair

  ctx <- ask
  let refresh size =
        Just
          <$> runTaffy
            ctx
            (fromMaybe <$> pixBufFromColor size 0 <*> getCryptoPixbuf symbol)
  (image, _) <- scalingImage refresh Gtk.OrientationHorizontal
  _ <- widgetSetClassGI image "crypto-price-icon"
  _ <- widgetSetClassGI label "crypto-price-label"
  buildIconLabelBox image label
    >>= (`widgetSetClassGI` "crypto-price")

-- | Stored CoinMarketCap API key used for on-demand icon fetches.
newtype CMCAPIKey = CMCAPIKey String

-- | Set the coinmarketcap.com api key that will be used for retrieving crypto
-- icons that are not cached. This should occur before any attempts to retrieve
-- crypto icons happen. The easiest way to call this appropriately is to set it
-- as a 'startupHook'.
setCMCAPIKey :: String -> TaffyIO CMCAPIKey
setCMCAPIKey key =
  getStateDefault $ return $ CMCAPIKey key

-- | Build a label that will reflect the price of some token in some currency in
-- the coingecko API. This function accepts these valuesas a type parameter with
-- kind 'String' of the form `(symbol for asset being purchased)-(currency the
-- price should be expressed in)`. For example, the product string for the price
-- of bitcoin quoted in U.S. dollars is "BTC-USD". You can invoke this function
-- by enabling the TypeApplications language extension and passing the string
-- associated with the asset that you want to track as follows:
--
-- > cryptoPriceLabel @"BTC-USD"
cryptoPriceLabel :: forall a. (KnownSymbol a) => TaffyIO Gtk.Widget
cryptoPriceLabel = getCryptoPriceChannel @a >>= cryptoPriceLabel'

-- | Build a crypto price label from an existing price channel.
cryptoPriceLabel' :: CryptoPriceChannel a -> TaffyIO Gtk.Widget
cryptoPriceLabel' (CryptoPriceChannel (chan, var)) = do
  label <- Gtk.labelNew Nothing
  _ <- widgetSetClassGI label "crypto-price-label"
  let updateWidget CryptoPriceInfo {lastPrice = cryptoPrice} =
        postGUIASync $
          Gtk.labelSetMarkup label $
            Data.Text.pack $
              show cryptoPrice
  void $
    Gtk.onWidgetRealize label $
      readMVar var >>= updateWidget
  (Gtk.toWidget =<< channelWidgetNew label chan updateWidget)
    >>= (`widgetSetClassGI` "crypto-price-label")

-- | Directory used to cache downloaded crypto icons.
cryptoIconsDir :: IO FilePath
cryptoIconsDir = (</> "crypto_icons") <$> taffyStateDir

-- | Cache path for a specific crypto symbol icon.
pathForCryptoSymbol :: String -> IO FilePath
pathForCryptoSymbol symbol =
  (</> printf "%s.png" symbol) <$> cryptoIconsDir

-- | Retrieve a pixbuf image corresponding to the provided crypto symbol. The
-- image used will be retrieved from the file with the name `(pricesymbol).png`
-- from the directory defined by 'cryptoIconsDir'. If a file is not found there
-- and an an api key for coinmarketcap.com has been set using 'setCMCAPIKey', an
-- icon will be automatically be retrieved from coinmarketcap.com.
getCryptoPixbuf :: String -> TaffyIO (Maybe Gdk.Pixbuf)
getCryptoPixbuf = getCryptoIconFromCache <||> getCryptoIconFromCMC

-- | Try to load a cached icon for a symbol from disk.
getCryptoIconFromCache :: (MonadIO m) => String -> m (Maybe Gdk.Pixbuf)
getCryptoIconFromCache symbol =
  liftIO $
    pathForCryptoSymbol symbol >>= safePixbufNewFromFile

-- | Try to fetch an icon via CoinMarketCap using the configured API key.
getCryptoIconFromCMC :: String -> TaffyIO (Maybe Gdk.Pixbuf)
getCryptoIconFromCMC symbol =
  runMaybeT $ do
    CMCAPIKey cmcAPIKey <- MaybeT getState
    MaybeT $ lift $ getCryptoIconFromCMC' cmcAPIKey symbol

-- | Fetch a symbol icon from CoinMarketCap metadata and cache it to disk.
getCryptoIconFromCMC' :: String -> String -> IO (Maybe Gdk.Pixbuf)
getCryptoIconFromCMC' cmcAPIKey symbol = do
  jsonText <- getCryptoMeta cmcAPIKey symbol
  let uri = getIconURIFromJSON symbol jsonText >>= parseRequest . Data.Text.unpack
  path <- pathForCryptoSymbol symbol
  maybe (return ()) (`downloadURIToPath` path) uri
  safePixbufNewFromFile path

-- | Extract the logo URI for a symbol from CoinMarketCap metadata JSON.
getIconURIFromJSON :: String -> LBS.ByteString -> Maybe Data.Text.Text
getIconURIFromJSON symbol jsonText =
  decode jsonText
    >>= parseMaybe
      ((.: "data") >=> (.: Key.fromString symbol) >=> (.: "logo"))
