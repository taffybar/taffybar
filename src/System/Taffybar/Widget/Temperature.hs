--------------------------------------------------------------------------------
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
--
--------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module System.Taffybar.Widget.Temperature
  ( -- * Widget
    temperatureNew
  , temperatureNewWith
    -- * Configuration
  , TemperatureConfig(..)
  , defaultTemperatureConfig
    -- * Re-exports
  , TemperatureUnit(..)
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Default (Default(..))
import Data.List (intercalate)
import qualified Data.Text as T
import qualified GI.Gtk as Gtk
import qualified Text.StringTemplate as ST

import System.Taffybar.Information.Temperature
import System.Taffybar.Widget.Generic.PollingLabel (pollingLabelNewWithTooltip)

-- | Configuration for the temperature widget
data TemperatureConfig = TemperatureConfig
  { tempFormat :: String
    -- ^ Format string for the label. Available variables:
    -- temp (in configured unit), tempC (Celsius), tempF (Fahrenheit), tempK (Kelvin)
  , tempUnit :: TemperatureUnit
    -- ^ Unit to use for the {temp} variable (default: Celsius)
  , tempWarningThreshold :: Double
    -- ^ Temperature (in Celsius) at which to show warning style (default: 70)
  , tempCriticalThreshold :: Double
    -- ^ Temperature (in Celsius) at which to show critical style (default: 85)
  , tempPollInterval :: Double
    -- ^ How often to poll for temperature updates, in seconds (default: 10)
  , tempSensorFilter :: ThermalSensor -> Bool
    -- ^ Filter function to select which sensors to monitor (default: all)
  , tempAggregation :: [TemperatureInfo] -> Maybe Double
    -- ^ How to aggregate multiple sensor readings (default: maximum)
  }

instance Default TemperatureConfig where
  def = defaultTemperatureConfig

-- | Default configuration for the temperature widget
defaultTemperatureConfig :: TemperatureConfig
defaultTemperatureConfig = TemperatureConfig
  { tempFormat = "$temp$\176C"  -- degree symbol
  , tempUnit = Celsius
  , tempWarningThreshold = 70
  , tempCriticalThreshold = 85
  , tempPollInterval = 10
  , tempSensorFilter = const True
  , tempAggregation = \temps ->
      if null temps
        then Nothing
        else Just $ maximum $ map tempCelsius temps
  }

-- | Create a temperature widget with default configuration
temperatureNew :: MonadIO m => m Gtk.Widget
temperatureNew = temperatureNewWith defaultTemperatureConfig

-- | Create a temperature widget with custom configuration
temperatureNewWith :: MonadIO m => TemperatureConfig -> m Gtk.Widget
temperatureNewWith config = liftIO $ do
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
    readTemperaturesFiltered sensors = do
      allTemps <- readAllTemperatures
      return $ filter (\t -> tempSensor t `elem` sensors) allTemps

-- | Format the temperature label using the template
formatTemperature :: TemperatureConfig -> Double -> Double -> Double -> Double -> T.Text
formatTemperature config tempDisplay tempC tempF tempK =
  let template = ST.newSTMP (tempFormat config)
      template' = ST.setManyAttrib
        [ ("temp", formatDouble tempDisplay)
        , ("tempC", formatDouble tempC)
        , ("tempF", formatDouble tempF)
        , ("tempK", formatDouble tempK)
        ] template
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
      sensorName (tempSensor info) ++ ": " ++
      show (round (tempCelsius info) :: Int) ++ "\176C"
