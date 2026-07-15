{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Module      : System.Taffybar.Widget.NvidiaTemperature
-- Copyright   : (c) Ivan Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan Malison <IvanMalison@gmail.com>
-- Stability   : unstable
-- Portability : unportable
--
-- NVIDIA GPU temperature widgets backed by @nvidia-smi@.
module System.Taffybar.Widget.NvidiaTemperature
  ( -- * Combined icon+label widget
    nvidiaTemperatureNew,
    nvidiaTemperatureNewWith,
    nvidiaTemperatureNewChan,
    nvidiaTemperatureNewChanWith,

    -- * Icon-only widget
    nvidiaTemperatureIconNew,
    nvidiaTemperatureIconNewWith,

    -- * Label-only widget
    nvidiaTemperatureLabelNew,
    nvidiaTemperatureLabelNewWith,
    nvidiaTemperatureLabelNewChan,
    nvidiaTemperatureLabelNewChanWith,

    -- * Configuration
    NvidiaTemperatureConfig (..),
    defaultNvidiaTemperatureConfig,
  )
where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Default (Default (..))
import Data.List (find, intercalate, maximumBy)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Ord (comparing)
import qualified Data.Text as T
import qualified GI.Gtk as Gtk
import System.Taffybar.Context (TaffyIO)
import System.Taffybar.Information.Nvidia
import System.Taffybar.Util (postGUIASync)
import System.Taffybar.Widget.Generic.ChannelWidget (channelWidgetNew)
import System.Taffybar.Widget.Generic.PollingLabel (pollingLabelNewWithTooltip)
import System.Taffybar.Widget.Util (buildIconLabelBox, widgetSetClassGI)
import Text.Printf (printf)
import qualified Text.StringTemplate as ST

-- | Configuration for the NVIDIA temperature widget.
data NvidiaTemperatureConfig = NvidiaTemperatureConfig
  { -- | Path or command name for @nvidia-smi@.
    nvidiaTemperatureCommand :: FilePath,
    -- | GPU index to display. 'Nothing' displays the hottest available GPU.
    nvidiaTemperatureGpuIndex :: Maybe Int,
    -- | Label template. Available variables are @gpu@, @name@, and @tempC@.
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

-- | Create a combined icon and label widget backed by the shared channel.
nvidiaTemperatureNewChan :: TaffyIO Gtk.Widget
nvidiaTemperatureNewChan = nvidiaTemperatureNewChanWith defaultNvidiaTemperatureConfig

-- | Create a combined icon and label widget backed by the shared channel.
nvidiaTemperatureNewChanWith :: NvidiaTemperatureConfig -> TaffyIO Gtk.Widget
nvidiaTemperatureNewChanWith config = do
  iconWidget <- liftIO $ nvidiaTemperatureIconNewWith config
  labelWidget <- nvidiaTemperatureLabelNewChanWith config
  liftIO $
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

-- | Create a label widget that polls for rich NVIDIA information.
nvidiaTemperatureLabelNewWith :: (MonadIO m) => NvidiaTemperatureConfig -> m Gtk.Widget
nvidiaTemperatureLabelNewWith config = liftIO $ do
  widget <-
    pollingLabelNewWithTooltip (nvidiaTemperaturePollInterval config) $
      formatWidget config <$> readNvidiaGpuInfoWith (nvidiaTemperatureCommand config)
  widgetSetClassGI widget "nvidia-temperature-label"

-- | Create a label driven by the shared NVIDIA information channel.
nvidiaTemperatureLabelNewChan :: TaffyIO Gtk.Widget
nvidiaTemperatureLabelNewChan = nvidiaTemperatureLabelNewChanWith defaultNvidiaTemperatureConfig

-- | Create a label driven by the shared NVIDIA information channel.
nvidiaTemperatureLabelNewChanWith :: NvidiaTemperatureConfig -> TaffyIO Gtk.Widget
nvidiaTemperatureLabelNewChanWith config = do
  let command = nvidiaTemperatureCommand config
      interval = nvidiaTemperaturePollInterval config
  chan <- getNvidiaGpuInfoChanWith command interval
  initialInfo <- getNvidiaGpuInfoStateWith command interval

  liftIO $ do
    label <- Gtk.labelNew Nothing
    _ <- widgetSetClassGI label "nvidia-temperature-label"

    let updateLabel info = postGUIASync $ do
          let (labelText, tooltipText) = formatWidget config info
          Gtk.labelSetText label labelText
          Gtk.widgetSetTooltipText label tooltipText

    void $ Gtk.onWidgetRealize label $ updateLabel initialInfo
    Gtk.widgetShowAll label
    Gtk.toWidget =<< channelWidgetNew label chan updateLabel

formatWidget :: NvidiaTemperatureConfig -> [NvidiaGpuInfo] -> (T.Text, Maybe T.Text)
formatWidget config info =
  ( maybe (nvidiaTemperatureFallback config) (uncurry $ formatLabel config) $
      selectTemperature config info,
    formatTooltip info
  )

selectTemperature :: NvidiaTemperatureConfig -> [NvidiaGpuInfo] -> Maybe (NvidiaGpuInfo, Double)
selectTemperature config info =
  case nvidiaTemperatureGpuIndex config of
    Just index -> do
      gpu <- find ((== index) . nvidiaInfoIndex) info
      temperature <- nvidiaInfoTemperatureCelsius gpu
      pure (gpu, temperature)
    Nothing ->
      case mapMaybe withTemperature info of
        [] -> Nothing
        temperatures -> Just $ maximumBy (comparing snd) temperatures
  where
    withTemperature gpu = (gpu,) <$> nvidiaInfoTemperatureCelsius gpu

formatLabel :: NvidiaTemperatureConfig -> NvidiaGpuInfo -> Double -> T.Text
formatLabel config info temperature =
  T.pack $ ST.render template
  where
    template =
      ST.setManyAttrib
        [ ("gpu", show $ nvidiaInfoIndex info),
          ("name", T.unpack $ nvidiaInfoName info),
          ("tempC", show (round temperature :: Int))
        ]
        $ ST.newSTMP
        $ nvidiaTemperatureFormat config

formatTooltip :: [NvidiaGpuInfo] -> Maybe T.Text
formatTooltip [] = Nothing
formatTooltip info =
  Just $ T.pack $ intercalate "\n\n" $ map formatGpu info

formatGpu :: NvidiaGpuInfo -> String
formatGpu info = intercalate "\n" $ header : catMaybes detailLines
  where
    header =
      T.unpack (nvidiaInfoName info)
        ++ " (GPU "
        ++ show (nvidiaInfoIndex info)
        ++ ")"
    detailLines =
      [ temperatureLine info,
        measurementLine "Memory temperature" "\176C" $ nvidiaInfoMemoryTemperatureCelsius info,
        utilizationLine info,
        memoryLine info,
        powerLine info,
        measurementLine "Fan" "%" $ nvidiaInfoFanSpeedPercent info,
        ("Performance state: " ++) . T.unpack <$> nvidiaInfoPerformanceState info
      ]

temperatureLine :: NvidiaGpuInfo -> Maybe String
temperatureLine info = do
  temperature <- nvidiaInfoTemperatureCelsius info
  let details =
        catMaybes
          [ ("target " ++) . formatMeasurement "\176C" <$> nvidiaInfoTargetTemperatureCelsius info,
            ("headroom " ++) . formatMeasurement "\176C" <$> nvidiaInfoThermalHeadroomCelsius info
          ]
      suffix = if null details then "" else " (" ++ intercalate ", " details ++ ")"
  pure $ "Temperature: " ++ formatMeasurement "\176C" temperature ++ suffix

utilizationLine :: NvidiaGpuInfo -> Maybe String
utilizationLine info =
  prefixedValues
    "Utilization: "
    [ ("GPU " ++) . formatMeasurement "%" <$> nvidiaInfoGpuUtilizationPercent info,
      ("memory " ++) . formatMeasurement "%" <$> nvidiaInfoMemoryUtilizationPercent info
    ]

memoryLine :: NvidiaGpuInfo -> Maybe String
memoryLine info =
  case (nvidiaInfoMemoryUsedMiB info, nvidiaInfoMemoryTotalMiB info) of
    (Just used, Just total) ->
      Just $ "VRAM: " ++ formatMeasurement " MiB" used ++ " / " ++ formatMeasurement " MiB" total
    (Just used, Nothing) -> measurementLine "VRAM" " MiB" $ Just used
    _ -> Nothing

powerLine :: NvidiaGpuInfo -> Maybe String
powerLine info =
  case (nvidiaInfoPowerDrawWatts info, nvidiaInfoPowerLimitWatts info) of
    (Just draw, Just limit) ->
      Just $ "Power: " ++ formatMeasurement " W" draw ++ " / " ++ formatMeasurement " W" limit
    (Just draw, Nothing) -> measurementLine "Power" " W" $ Just draw
    _ -> Nothing

measurementLine :: String -> String -> Maybe Double -> Maybe String
measurementLine name unit = fmap $ ((name ++ ": ") ++) . formatMeasurement unit

prefixedValues :: String -> [Maybe String] -> Maybe String
prefixedValues prefix values =
  case catMaybes values of
    [] -> Nothing
    present -> Just $ prefix ++ intercalate ", " present

formatMeasurement :: String -> Double -> String
formatMeasurement unit value = formatNumber value ++ unit

formatNumber :: Double -> String
formatNumber value
  | abs (value - fromIntegral (round value :: Int)) < 0.05 = show (round value :: Int)
  | otherwise = printf "%.1f" value
