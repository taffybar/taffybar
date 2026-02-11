--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : System.Taffybar.Widget.Temperature
-- Copyright   : (c) Ivan Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan Malison <IvanMalison@gmail.com>
-- Stability   : unstable
-- Portability : unportable
--
-- A widget for displaying system temperature. Monitors thermal zones and/or
-- hwmon devices and displays the current temperature with configurable
-- thresholds for warning and critical states.
module System.Taffybar.Widget.Temperature
  ( -- * Combined icon+label widget
    temperatureNew,
    temperatureNewWith,

    -- * Icon-only widget
    temperatureIconNew,
    temperatureIconNewWith,

    -- * Label-only widget
    temperatureLabelNew,
    temperatureLabelNewWith,

    -- * Configuration
    TemperatureConfig (..),
    defaultTemperatureConfig,

    -- * Re-exports
    TemperatureUnit (..),
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Default (Default (..))
import Data.List (intercalate)
import qualified Data.Text as T
import qualified GI.Gtk as Gtk
import System.Taffybar.Information.Temperature
import System.Taffybar.Widget.Generic.PollingLabel (pollingLabelNewWithTooltip)
import System.Taffybar.Widget.Util (buildIconLabelBox)
import qualified Text.StringTemplate as ST

-- | Configuration for the temperature widget
data TemperatureConfig = TemperatureConfig
  { -- | Format string for the label. Available variables:
    -- temp (in configured unit), tempC (Celsius), tempF (Fahrenheit), tempK (Kelvin)
    tempFormat :: String,
    -- | Unit to use for the {temp} variable (default: Celsius)
    tempUnit :: TemperatureUnit,
    -- | Temperature (in Celsius) at which to show warning style (default: 70)
    tempWarningThreshold :: Double,
    -- | Temperature (in Celsius) at which to show critical style (default: 85)
    tempCriticalThreshold :: Double,
    -- | How often to poll for temperature updates, in seconds (default: 10)
    tempPollInterval :: Double,
    -- | Filter function to select which sensors to monitor (default: all)
    tempSensorFilter :: ThermalSensor -> Bool,
    -- | How to aggregate multiple sensor readings (default: maximum)
    tempAggregation :: [TemperatureInfo] -> Maybe Double,
    -- | Nerd font icon character (default U+F2C9, nf-fa-thermometer).
    temperatureIcon :: T.Text
  }

instance Default TemperatureConfig where
  def = defaultTemperatureConfig

-- | Default configuration for the temperature widget
defaultTemperatureConfig :: TemperatureConfig
defaultTemperatureConfig =
  TemperatureConfig
    { tempFormat = "$temp$\176C", -- degree symbol
      tempUnit = Celsius,
      tempWarningThreshold = 70,
      tempCriticalThreshold = 85,
      tempPollInterval = 10,
      tempSensorFilter = const True,
      tempAggregation = \temps ->
        if null temps
          then Nothing
          else Just $ maximum $ map tempCelsius temps,
      temperatureIcon = T.pack "\xF2C9"
    }

-- | Create a combined icon+label temperature widget with default configuration.
temperatureNew :: (MonadIO m) => m Gtk.Widget
temperatureNew = temperatureNewWith defaultTemperatureConfig

-- | Create a combined icon+label temperature widget.
temperatureNewWith :: (MonadIO m) => TemperatureConfig -> m Gtk.Widget
temperatureNewWith config = liftIO $ do
  iconWidget <- temperatureIconNewWith config
  labelWidget <- temperatureLabelNewWith config
  buildIconLabelBox iconWidget labelWidget

-- | Create a temperature icon widget with default configuration.
temperatureIconNew :: (MonadIO m) => m Gtk.Widget
temperatureIconNew = temperatureIconNewWith defaultTemperatureConfig

-- | Create a temperature icon widget with the provided configuration.
temperatureIconNewWith :: (MonadIO m) => TemperatureConfig -> m Gtk.Widget
temperatureIconNewWith config = liftIO $ do
  label <- Gtk.labelNew (Just (temperatureIcon config))
  Gtk.widgetShowAll label
  Gtk.toWidget label

-- | Create a temperature label widget with default configuration.
temperatureLabelNew :: (MonadIO m) => m Gtk.Widget
temperatureLabelNew = temperatureLabelNewWith defaultTemperatureConfig

-- | Create a temperature label widget with custom configuration.
temperatureLabelNewWith :: (MonadIO m) => TemperatureConfig -> m Gtk.Widget
temperatureLabelNewWith config = liftIO $ do
  -- Discover sensors once at startup, filtered by config
  allSensors <- discoverSensors
  let sensors = filter (tempSensorFilter config) allSensors

  widget <- pollingLabelNewWithTooltip (tempPollInterval config) $ do
    temps <- readTemperaturesFiltered sensors
    case tempAggregation config temps of
      Nothing -> return (T.pack "N/A", Nothing)
      Just tempC -> do
        let tempF = convertTemperature Fahrenheit tempC
            tempK = convertTemperature Kelvin tempC
            tempDisplay = convertTemperature (tempUnit config) tempC
            labelText = formatTemperature config tempDisplay tempC tempF tempK
            tooltipText = formatTooltip temps
        return (labelText, Just tooltipText)

  -- We need to update CSS classes dynamically, but pollingLabel doesn't
  -- support that directly. Instead, we set up the widget and update classes
  -- in the polling callback through a custom approach.
  -- For now, we'll use a simpler approach with the base widget.
  Gtk.toWidget widget
  where
    readTemperaturesFiltered :: [ThermalSensor] -> IO [TemperatureInfo]
    readTemperaturesFiltered sensors =
      filter (\t -> tempSensor t `elem` sensors) <$> readAllTemperatures

-- | Format the temperature label using the template
formatTemperature :: TemperatureConfig -> Double -> Double -> Double -> Double -> T.Text
formatTemperature config tempDisplay tempC tempF tempK =
  let template = ST.newSTMP (tempFormat config)
      template' =
        ST.setManyAttrib
          [ ("temp", formatDouble tempDisplay),
            ("tempC", formatDouble tempC),
            ("tempF", formatDouble tempF),
            ("tempK", formatDouble tempK)
          ]
          template
   in T.pack $ ST.render template'
  where
    formatDouble :: Double -> String
    formatDouble d = show (round d :: Int)

-- | Format tooltip showing all sensor readings
formatTooltip :: [TemperatureInfo] -> T.Text
formatTooltip temps =
  T.pack $ intercalate "\n" $ map formatSensor temps
  where
    formatSensor info =
      sensorName (tempSensor info)
        ++ ": "
        ++ show (round (tempCelsius info) :: Int)
        ++ "\176C"
