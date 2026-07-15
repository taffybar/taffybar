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
    temperatureNewChan,
    temperatureNewChanWith,

    -- * Icon-only widget
    temperatureIconNew,
    temperatureIconNewWith,

    -- * Label-only widget
    temperatureLabelNew,
    temperatureLabelNewWith,
    temperatureLabelNewChan,
    temperatureLabelNewChanWith,

    -- * Configuration
    TemperatureConfig (..),
    defaultTemperatureConfig,

    -- * Re-exports
    TemperatureUnit (..),
  )
where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Default (Default (..))
import Data.List (intercalate)
import qualified Data.Text as T
import qualified GI.Gtk as Gtk
import System.Taffybar.Context (TaffyIO)
import System.Taffybar.Information.Temperature
import System.Taffybar.Util (postGUIASync)
import System.Taffybar.Widget.Generic.ChannelWidget (channelWidgetNew)
import System.Taffybar.Widget.Generic.PollingLabel (pollingLabelNewWithTooltip)
import System.Taffybar.Widget.Util (buildIconLabelBox, widgetSetClassGI)
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
    -- | Select additional sensors to include only in the tooltip. Sensors
    -- selected by 'tempSensorFilter' are always included (default: none).
    tempTooltipSensorFilter :: ThermalSensor -> Bool,
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
      tempTooltipSensorFilter = const False,
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
    >>= (`widgetSetClassGI` "temperature")

-- | Create a combined icon+label widget backed by the shared channel.
temperatureNewChan :: TaffyIO Gtk.Widget
temperatureNewChan = temperatureNewChanWith defaultTemperatureConfig

-- | Create a combined icon+label widget backed by the shared channel.
temperatureNewChanWith :: TemperatureConfig -> TaffyIO Gtk.Widget
temperatureNewChanWith config = do
  iconWidget <- liftIO $ temperatureIconNewWith config
  labelWidget <- temperatureLabelNewChanWith config
  liftIO $
    buildIconLabelBox iconWidget labelWidget
      >>= (`widgetSetClassGI` "temperature")

-- | Create a temperature icon widget with default configuration.
temperatureIconNew :: (MonadIO m) => m Gtk.Widget
temperatureIconNew = temperatureIconNewWith defaultTemperatureConfig

-- | Create a temperature icon widget with the provided configuration.
temperatureIconNewWith :: (MonadIO m) => TemperatureConfig -> m Gtk.Widget
temperatureIconNewWith config = liftIO $ do
  label <- Gtk.labelNew (Just (temperatureIcon config))
  _ <- widgetSetClassGI label "temperature-icon"
  Gtk.widgetShowAll label
  Gtk.toWidget label

-- | Create a temperature label widget with default configuration.
temperatureLabelNew :: (MonadIO m) => m Gtk.Widget
temperatureLabelNew = temperatureLabelNewWith defaultTemperatureConfig

-- | Create a polling temperature label with custom configuration.
temperatureLabelNewWith :: (MonadIO m) => TemperatureConfig -> m Gtk.Widget
temperatureLabelNewWith config = liftIO $ do
  widget <-
    pollingLabelNewWithTooltip (tempPollInterval config) $
      formatTemperatureInfo config <$> readAllTemperatures
  widgetSetClassGI widget "temperature-label"

-- | Create a channel-driven temperature label with the default configuration.
temperatureLabelNewChan :: TaffyIO Gtk.Widget
temperatureLabelNewChan = temperatureLabelNewChanWith defaultTemperatureConfig

-- | Create a channel-driven temperature label with custom configuration.
-- Sensor discovery and polling are shared by every channel-driven widget.
temperatureLabelNewChanWith :: TemperatureConfig -> TaffyIO Gtk.Widget
temperatureLabelNewChanWith config = do
  chan <- getTemperatureInfoChan $ tempPollInterval config
  initialInfo <- getTemperatureInfoState $ tempPollInterval config

  liftIO $ do
    label <- Gtk.labelNew Nothing
    _ <- widgetSetClassGI label "temperature-label"

    let updateLabel info = postGUIASync $ do
          let (labelText, tooltipText) = formatTemperatureInfo config info
          Gtk.labelSetText label labelText
          Gtk.widgetSetTooltipText label tooltipText

    void $ Gtk.onWidgetRealize label $ updateLabel initialInfo
    Gtk.widgetShowAll label
    Gtk.toWidget =<< channelWidgetNew label chan updateLabel

formatTemperatureInfo :: TemperatureConfig -> [TemperatureInfo] -> (T.Text, Maybe T.Text)
formatTemperatureInfo config allTemperatures =
  case tempAggregation config temperatures of
    Nothing -> ("N/A", tooltipText)
    Just tempC ->
      let tempF = convertTemperature Fahrenheit tempC
          tempK = convertTemperature Kelvin tempC
          tempDisplay = convertTemperature (tempUnit config) tempC
       in (formatTemperature config tempDisplay tempC tempF tempK, tooltipText)
  where
    includedInLabel = tempSensorFilter config . tempSensor
    includedInTooltip info =
      includedInLabel info || tempTooltipSensorFilter config (tempSensor info)
    temperatures = filter includedInLabel allTemperatures
    tooltipTemperatures = filter includedInTooltip allTemperatures
    tooltipText = formatTooltip tooltipTemperatures

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

-- | Format tooltip showing all selected sensor readings.
formatTooltip :: [TemperatureInfo] -> Maybe T.Text
formatTooltip [] = Nothing
formatTooltip temps =
  Just $ T.pack $ intercalate "\n" $ map formatSensor temps
  where
    formatSensor info =
      sensorName (tempSensor info)
        ++ ": "
        ++ show (round (tempCelsius info) :: Int)
        ++ "\176C"
