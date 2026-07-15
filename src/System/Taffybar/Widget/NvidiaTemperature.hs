{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : System.Taffybar.Widget.NvidiaTemperature
-- Copyright   : (c) Ivan Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan Malison <IvanMalison@gmail.com>
-- Stability   : unstable
-- Portability : unportable
--
-- A widget for displaying NVIDIA GPU temperatures from @nvidia-smi@.
module System.Taffybar.Widget.NvidiaTemperature
  ( -- * Combined icon+label widget
    nvidiaTemperatureNew,
    nvidiaTemperatureNewWith,

    -- * Icon-only widget
    nvidiaTemperatureIconNew,
    nvidiaTemperatureIconNewWith,

    -- * Label-only widget
    nvidiaTemperatureLabelNew,
    nvidiaTemperatureLabelNewWith,

    -- * Configuration
    NvidiaTemperatureConfig (..),
    defaultNvidiaTemperatureConfig,
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Default (Default (..))
import Data.List (find, intercalate, maximumBy)
import Data.Ord (comparing)
import qualified Data.Text as T
import qualified GI.Gtk as Gtk
import System.Taffybar.Information.Nvidia
import System.Taffybar.Widget.Generic.PollingLabel (pollingLabelNewWithTooltip)
import System.Taffybar.Widget.Util (buildIconLabelBox, widgetSetClassGI)
import qualified Text.StringTemplate as ST

-- | Configuration for the NVIDIA temperature widget.
data NvidiaTemperatureConfig = NvidiaTemperatureConfig
  { -- | Path or command name for @nvidia-smi@.
    nvidiaTemperatureCommand :: FilePath,
    -- | GPU index to display. 'Nothing' displays the hottest available GPU.
    nvidiaTemperatureGpuIndex :: Maybe Int,
    -- | Label template. Available variables are @gpu@ and @tempC@.
    nvidiaTemperatureFormat :: String,
    -- | Text displayed when no temperature is available.
    nvidiaTemperatureFallback :: T.Text,
    -- | Polling period in seconds.
    nvidiaTemperaturePollInterval :: Double,
    -- | Icon used by the combined icon and label widget.
    nvidiaTemperatureIcon :: T.Text
  }

instance Default NvidiaTemperatureConfig where
  def = defaultNvidiaTemperatureConfig

-- | Default to GPU 0, refreshed every ten seconds.
defaultNvidiaTemperatureConfig :: NvidiaTemperatureConfig
defaultNvidiaTemperatureConfig =
  NvidiaTemperatureConfig
    { nvidiaTemperatureCommand = "nvidia-smi",
      nvidiaTemperatureGpuIndex = Just 0,
      nvidiaTemperatureFormat = "GPU $tempC$\176C",
      nvidiaTemperatureFallback = "GPU N/A",
      nvidiaTemperaturePollInterval = 10,
      nvidiaTemperatureIcon = "\xF2C9"
    }

-- | Create a combined icon and label widget with the default configuration.
nvidiaTemperatureNew :: (MonadIO m) => m Gtk.Widget
nvidiaTemperatureNew = nvidiaTemperatureNewWith defaultNvidiaTemperatureConfig

-- | Create a combined icon and label widget.
nvidiaTemperatureNewWith :: (MonadIO m) => NvidiaTemperatureConfig -> m Gtk.Widget
nvidiaTemperatureNewWith config = liftIO $ do
  iconWidget <- nvidiaTemperatureIconNewWith config
  labelWidget <- nvidiaTemperatureLabelNewWith config
  buildIconLabelBox iconWidget labelWidget
    >>= (`widgetSetClassGI` "nvidia-temperature")

-- | Create an icon widget with the default configuration.
nvidiaTemperatureIconNew :: (MonadIO m) => m Gtk.Widget
nvidiaTemperatureIconNew = nvidiaTemperatureIconNewWith defaultNvidiaTemperatureConfig

-- | Create an icon widget with the provided configuration.
nvidiaTemperatureIconNewWith :: (MonadIO m) => NvidiaTemperatureConfig -> m Gtk.Widget
nvidiaTemperatureIconNewWith config = liftIO $ do
  label <- Gtk.labelNew $ Just $ nvidiaTemperatureIcon config
  _ <- widgetSetClassGI label "nvidia-temperature-icon"
  Gtk.widgetShowAll label
  Gtk.toWidget label

-- | Create a label widget with the default configuration.
nvidiaTemperatureLabelNew :: (MonadIO m) => m Gtk.Widget
nvidiaTemperatureLabelNew = nvidiaTemperatureLabelNewWith defaultNvidiaTemperatureConfig

-- | Create a label widget that polls @nvidia-smi@.
nvidiaTemperatureLabelNewWith :: (MonadIO m) => NvidiaTemperatureConfig -> m Gtk.Widget
nvidiaTemperatureLabelNewWith config = do
  widget <- pollingLabelNewWithTooltip (nvidiaTemperaturePollInterval config) $ do
    temperatures <- readNvidiaGpuTemperaturesWith $ nvidiaTemperatureCommand config
    pure $ case selectTemperature config temperatures of
      Nothing -> (nvidiaTemperatureFallback config, formatTooltip temperatures)
      Just temperature ->
        (formatLabel config temperature, formatTooltip temperatures)
  widgetSetClassGI widget "nvidia-temperature-label"

selectTemperature :: NvidiaTemperatureConfig -> [NvidiaGpuTemperature] -> Maybe NvidiaGpuTemperature
selectTemperature _ [] = Nothing
selectTemperature config temperatures =
  case nvidiaTemperatureGpuIndex config of
    Just index -> find ((== index) . nvidiaGpuIndex) temperatures
    Nothing -> Just $ maximumBy (comparing nvidiaGpuTemperatureCelsius) temperatures

formatLabel :: NvidiaTemperatureConfig -> NvidiaGpuTemperature -> T.Text
formatLabel config temperature =
  T.pack $ ST.render template
  where
    template =
      ST.setManyAttrib
        [ ("gpu", show $ nvidiaGpuIndex temperature),
          ("tempC", show (round (nvidiaGpuTemperatureCelsius temperature) :: Int))
        ]
        $ ST.newSTMP
        $ nvidiaTemperatureFormat config

formatTooltip :: [NvidiaGpuTemperature] -> Maybe T.Text
formatTooltip [] = Nothing
formatTooltip temperatures =
  Just $ T.pack $ intercalate "\n" $ map formatOne temperatures
  where
    formatOne temperature =
      "NVIDIA GPU "
        ++ show (nvidiaGpuIndex temperature)
        ++ ": "
        ++ show (round (nvidiaGpuTemperatureCelsius temperature) :: Int)
        ++ "\176C"
