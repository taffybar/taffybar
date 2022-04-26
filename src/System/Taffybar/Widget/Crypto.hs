{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
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
-----------------------------------------------------------------------------
module System.Taffybar.Widget.Crypto where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Reader
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy as LBS
import           Data.List.Split
import           Data.Maybe
import           Data.Proxy
import           Data.String (fromString)
import qualified Data.Text
import           GHC.TypeLits
import qualified GI.GdkPixbuf.Objects.Pixbuf as Gdk
import qualified GI.Gtk as Gtk
import           Network.HTTP.Simple hiding (Proxy)
import           System.FilePath.Posix
import           System.Taffybar.Context
import           System.Taffybar.Information.Crypto
import           System.Taffybar.Util
import           System.Taffybar.Widget.Generic.AutoSizeImage
import           System.Taffybar.Widget.Generic.ChannelWidget
import           System.Taffybar.WindowIcon
import           Text.Printf

-- | Extends 'cryptoPriceLabel' with an icon corresponding to the symbol of the
-- purchase crypto that will appear to the left of the price label. See the
-- docstring for 'getCryptoPixbuf' for details about how this icon is retrieved.
-- As with 'cryptoPriceLabel', this function must be invoked with a type
-- application with the type string for the coinbase product that should be
-- tracked. See the docstring of 'cryptoPriceLabel' for details about the exact
-- format that this string should take.
cryptoPriceLabelWithIcon :: forall a. KnownSymbol a => TaffyIO Gtk.Widget
cryptoPriceLabelWithIcon = do
  label <- cryptoPriceLabel @a
  let symbolPair = symbolVal (Proxy :: Proxy a)
      symbol = head $ splitOn "-" symbolPair
  hbox <- Gtk.boxNew Gtk.OrientationHorizontal 0

  ctx <- ask
  let refresh =
        const $ flip runReaderT ctx $
        fromMaybe <$> pixBufFromColor 10 0 <*> getCryptoPixbuf symbol
  image <- autoSizeImageNew refresh Gtk.OrientationHorizontal

  Gtk.containerAdd hbox image
  Gtk.containerAdd hbox label

  Gtk.toWidget hbox

newtype CMCAPIKey = CMCAPIKey String

-- | Set the coinmarketcap.com api key that will be used for retrieving crypto
-- icons that are not cached. This should occur before any attempts to retrieve
-- crypto icons happen. The easiest way to call this appropriately is to set it
-- as a 'startupHook'.
setCMCAPIKey :: String -> TaffyIO CMCAPIKey
setCMCAPIKey key =
  getStateDefault $ return $ CMCAPIKey key

-- | Build a label that will reflect the price of some crypto product in the
-- coinbase API. A coinbase product consists of a pair of assets, the asset that
-- is being purchased, and the asset used to do the purchasing. This function
-- accepts the product to be tracked as a type parameter with kind 'String' of
-- the form `(symbol for asset being purchased)-(symbol asset used for
-- purchase)`. For example, the product string for the price of bitcoin quoted
-- in U.S. dollars is "BTC-USD". You can invoke this function by enabling the
-- TypeApplications language extension and passing the string associated with
-- the asset that you want to track as follows:
--
-- > cryptoPriceLabel @"BTC-USD"
cryptoPriceLabel :: forall a. KnownSymbol a => TaffyIO Gtk.Widget
cryptoPriceLabel = getCryptoPriceChannel @a >>= cryptoPriceLabel'

cryptoPriceLabel' :: CryptoPriceChannel a -> TaffyIO Gtk.Widget
cryptoPriceLabel' (CryptoPriceChannel (chan, var)) = do
  label <- Gtk.labelNew Nothing
  let updateWidget CryptoPriceInfo { lastPrice = cryptoPrice } =
        postGUIASync $ Gtk.labelSetMarkup label $
                     Data.Text.pack $ show cryptoPrice
  void $ Gtk.onWidgetRealize label $
       readMVar var >>= updateWidget
  Gtk.toWidget =<< channelWidgetNew label chan updateWidget

cryptoIconsDir :: IO FilePath
cryptoIconsDir = (</> "crypto_icons") <$> taffyStateDir

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

getCryptoIconFromCache :: MonadIO m => String -> m (Maybe Gdk.Pixbuf)
getCryptoIconFromCache symbol = liftIO $
  pathForCryptoSymbol symbol >>= safePixbufNewFromFile

getCryptoIconFromCMC :: String -> TaffyIO (Maybe Gdk.Pixbuf)
getCryptoIconFromCMC symbol =
  runMaybeT $ do
    CMCAPIKey cmcAPIKey <- MaybeT getState
    MaybeT $ lift $ getCryptoIconFromCMC' cmcAPIKey symbol

getCryptoIconFromCMC' :: String -> String -> IO (Maybe Gdk.Pixbuf)
getCryptoIconFromCMC' cmcAPIKey symbol = do
  jsonText <- getCryptoMeta cmcAPIKey symbol
  let uri = getIconURIFromJSON symbol jsonText >>= parseRequest . Data.Text.unpack
  path <- pathForCryptoSymbol symbol
  maybe (return ()) (`downloadURIToPath` path) uri
  safePixbufNewFromFile path

getIconURIFromJSON :: String -> LBS.ByteString -> Maybe Data.Text.Text
getIconURIFromJSON symbol jsonText =
  decode jsonText >>= parseMaybe
           ((.: "data") >=> (.: fromString symbol) >=> (.: "logo"))


