--------------------------------------------------------------------------------
-- |
-- Module      : System.Taffybar.Information.Temperature
-- Copyright   : (c) Ivan Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan Malison <IvanMalison@gmail.com>
-- Stability   : unstable
-- Portability : unportable
--
-- This module provides functions to read system temperature information from
-- Linux thermal zones and hwmon devices.
--
--------------------------------------------------------------------------------

module System.Taffybar.Information.Temperature
  ( TemperatureInfo(..)
  , ThermalSensor(..)
  , TemperatureUnit(..)
  , discoverSensors
  , readSensorTemperature
  , readAllTemperatures
  , readTemperaturesFrom
  , convertTemperature
  ) where

import Control.Exception (try, SomeException)
import Control.Monad (forM)
import Data.List (sortOn)
import Data.Maybe (catMaybes)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath ((</>))
import Text.Read (readMaybe)

-- | Temperature unit for display
data TemperatureUnit = Celsius | Fahrenheit | Kelvin
  deriving (Show, Eq)

-- | Information about a thermal sensor
data ThermalSensor = ThermalSensor
  { sensorName :: String      -- ^ Human-readable sensor name
  , sensorPath :: FilePath    -- ^ Path to the temperature file
  , sensorZone :: String      -- ^ Zone or hwmon identifier
  } deriving (Show, Eq)

-- | Temperature reading from a sensor
data TemperatureInfo = TemperatureInfo
  { tempCelsius :: Double     -- ^ Temperature in Celsius
  , tempSensor :: ThermalSensor  -- ^ The sensor this reading is from
  } deriving (Show, Eq)

-- | Convert temperature from Celsius to another unit
convertTemperature :: TemperatureUnit -> Double -> Double
convertTemperature Celsius t = t
convertTemperature Fahrenheit t = t * 1.8 + 32
convertTemperature Kelvin t = t + 273.15

-- | Discover all available thermal sensors on the system
-- Tries thermal_zone first, then hwmon devices
discoverSensors :: IO [ThermalSensor]
discoverSensors = do
  thermalSensors <- discoverThermalZones
  hwmonSensors <- discoverHwmon
  return $ thermalSensors ++ hwmonSensors

-- | Discover thermal zones from /sys/class/thermal/
discoverThermalZones :: IO [ThermalSensor]
discoverThermalZones = do
  let thermalDir = "/sys/class/thermal"
  exists <- doesDirectoryExist thermalDir
  if not exists
    then return []
    else do
      entries <- listDirectory thermalDir
      let zones = filter (isPrefixOf "thermal_zone") entries
      sensors <- forM zones $ \zone -> do
        let tempPath = thermalDir </> zone </> "temp"
            typePath = thermalDir </> zone </> "type"
        tempExists <- doesFileExist tempPath
        if tempExists
          then do
            sensorType <- readFileSafe typePath
            return $ Just ThermalSensor
              { sensorName = maybe zone id sensorType
              , sensorPath = tempPath
              , sensorZone = zone
              }
          else return Nothing
      return $ catMaybes sensors
  where
    isPrefixOf prefix str = take (length prefix) str == prefix

-- | Discover hwmon temperature sensors from /sys/class/hwmon/
discoverHwmon :: IO [ThermalSensor]
discoverHwmon = do
  let hwmonDir = "/sys/class/hwmon"
  exists <- doesDirectoryExist hwmonDir
  if not exists
    then return []
    else do
      entries <- listDirectory hwmonDir
      let hwmons = filter (isPrefixOf "hwmon") entries
      sensorLists <- forM hwmons $ \hwmon -> do
        let hwmonPath = hwmonDir </> hwmon
            namePath = hwmonPath </> "name"
        hwmonName <- readFileSafe namePath
        -- Find all temp*_input files
        allFiles <- listDirectory hwmonPath
        let tempInputs = filter isTempInput allFiles
        forM tempInputs $ \tempFile -> do
          let tempPath = hwmonPath </> tempFile
              sensorId = extractSensorId tempFile
              labelPath = hwmonPath </> ("temp" ++ sensorId ++ "_label")
          labelName <- readFileSafe labelPath
          let name = case (labelName, hwmonName) of
                (Just l, _) -> l
                (_, Just n) -> n ++ "_temp" ++ sensorId
                _ -> hwmon ++ "_temp" ++ sensorId
          return ThermalSensor
            { sensorName = name
            , sensorPath = tempPath
            , sensorZone = hwmon
            }
      return $ concat sensorLists
  where
    isPrefixOf prefix str = take (length prefix) str == prefix
    isTempInput file = take 4 file == "temp" && isSuffixOf "_input" file
    isSuffixOf suffix str =
      let suffixLen = length suffix
          strLen = length str
      in strLen >= suffixLen && drop (strLen - suffixLen) str == suffix
    -- Extract sensor number from "temp1_input" -> "1"
    extractSensorId file =
      let withoutPrefix = drop 4 file  -- remove "temp"
          withoutSuffix = take (length withoutPrefix - 6) withoutPrefix  -- remove "_input"
      in withoutSuffix

-- | Read temperature from a file, returns Nothing if file cannot be read
readFileSafe :: FilePath -> IO (Maybe String)
readFileSafe path = do
  exists <- doesFileExist path
  if not exists
    then return Nothing
    else do
      result <- try $ readFile path :: IO (Either SomeException String)
      case result of
        Left _ -> return Nothing
        Right content -> return $ Just $ strip content
  where
    strip = dropWhile (== ' ') . reverse . dropWhile (== '\n') . reverse . dropWhile (== ' ')

-- | Read temperature from a single sensor
-- Returns Nothing if the sensor cannot be read
readSensorTemperature :: ThermalSensor -> IO (Maybe TemperatureInfo)
readSensorTemperature sensor = do
  result <- try $ readFile (sensorPath sensor) :: IO (Either SomeException String)
  case result of
    Left _ -> return Nothing
    Right content ->
      case readMaybe (strip content) :: Maybe Integer of
        Nothing -> return Nothing
        Just milliDegrees -> return $ Just TemperatureInfo
          { tempCelsius = fromIntegral milliDegrees / 1000.0
          , tempSensor = sensor
          }
  where
    strip = dropWhile (== ' ') . reverse . dropWhile (== '\n') . reverse . dropWhile (== ' ')

-- | Read temperatures from all discovered sensors
-- Returns list sorted by temperature (highest first)
readAllTemperatures :: IO [TemperatureInfo]
readAllTemperatures = do
  sensors <- discoverSensors
  temps <- forM sensors readSensorTemperature
  return $ sortOn (negate . tempCelsius) $ catMaybes temps

-- | Read temperatures from specific sensors (by path)
readTemperaturesFrom :: [ThermalSensor] -> IO [TemperatureInfo]
readTemperaturesFrom sensors = do
  temps <- forM sensors readSensorTemperature
  return $ catMaybes temps
