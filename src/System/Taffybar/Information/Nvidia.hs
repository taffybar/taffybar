{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : System.Taffybar.Information.Nvidia
-- Copyright   : (c) Ivan Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan Malison <IvanMalison@gmail.com>
-- Stability   : unstable
-- Portability : unportable
--
-- NVIDIA GPU information obtained from @nvidia-smi@.
module System.Taffybar.Information.Nvidia
  ( NvidiaGpuInfo (..),
    parseNvidiaGpuInfo,
    readNvidiaGpuInfo,
    readNvidiaGpuInfoWith,
    shouldQueryNvidiaForRuntimeStatuses,
    getNvidiaGpuInfoChan,
    getNvidiaGpuInfoChanWith,
    getNvidiaGpuInfoState,
    getNvidiaGpuInfoStateWith,
    NvidiaGpuTemperature (..),
    parseNvidiaGpuTemperatures,
    readNvidiaGpuTemperatures,
    readNvidiaGpuTemperaturesWith,
  )
where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Concurrent.STM.TChan
import Control.Exception (IOException, try)
import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.STM (atomically)
import Data.Char (isHexDigit)
import Data.Foldable (for_)
import Data.List (sortOn)
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (listDirectory)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)
import System.Taffybar.Context (TaffyIO, getStateDefault)
import System.Taffybar.Information.Wakeup (getWakeupChannelForDelay)
import Text.Read (readMaybe)
import Text.XML.Light

-- | A complete snapshot for one NVIDIA GPU.
--
-- Fields that @nvidia-smi@ reports as unavailable are represented by
-- 'Nothing'. Temperatures are in Celsius, power values are in watts, and
-- utilization and fan readings are percentages.
data NvidiaGpuInfo = NvidiaGpuInfo
  { nvidiaInfoIndex :: !Int,
    nvidiaInfoName :: !T.Text,
    nvidiaInfoTemperatureCelsius :: !(Maybe Double),
    nvidiaInfoMemoryTemperatureCelsius :: !(Maybe Double),
    nvidiaInfoTargetTemperatureCelsius :: !(Maybe Double),
    -- | Remaining temperature headroom before the target temperature.
    nvidiaInfoThermalHeadroomCelsius :: !(Maybe Double),
    nvidiaInfoFanSpeedPercent :: !(Maybe Double),
    nvidiaInfoGpuUtilizationPercent :: !(Maybe Double),
    nvidiaInfoMemoryUtilizationPercent :: !(Maybe Double),
    nvidiaInfoMemoryUsedMiB :: !(Maybe Double),
    nvidiaInfoMemoryTotalMiB :: !(Maybe Double),
    nvidiaInfoPowerDrawWatts :: !(Maybe Double),
    nvidiaInfoPowerLimitWatts :: !(Maybe Double),
    nvidiaInfoPerformanceState :: !(Maybe T.Text)
  }
  deriving (Eq, Show)

-- | Parse the XML produced by @nvidia-smi -q -x@.
parseNvidiaGpuInfo :: T.Text -> [NvidiaGpuInfo]
parseNvidiaGpuInfo contents =
  maybe [] (sortOn nvidiaInfoIndex . mapMaybe parseGpu . findElements (unqual "gpu")) $
    parseXMLDoc $
      T.unpack contents
  where
    parseGpu gpu = do
      index <- readElement ["minor_number"] gpu
      name <- elementText ["product_name"] gpu
      pure
        NvidiaGpuInfo
          { nvidiaInfoIndex = index,
            nvidiaInfoName = name,
            nvidiaInfoTemperatureCelsius = readElement ["temperature", "gpu_temp"] gpu,
            nvidiaInfoMemoryTemperatureCelsius = readElement ["temperature", "memory_temp"] gpu,
            nvidiaInfoTargetTemperatureCelsius = readElement ["temperature", "gpu_target_temperature"] gpu,
            nvidiaInfoThermalHeadroomCelsius = readElement ["temperature", "gpu_temp_tlimit"] gpu,
            nvidiaInfoFanSpeedPercent = readElement ["fan_speed"] gpu,
            nvidiaInfoGpuUtilizationPercent = readElement ["utilization", "gpu_util"] gpu,
            nvidiaInfoMemoryUtilizationPercent = readElement ["utilization", "memory_util"] gpu,
            nvidiaInfoMemoryUsedMiB = readElement ["fb_memory_usage", "used"] gpu,
            nvidiaInfoMemoryTotalMiB = readElement ["fb_memory_usage", "total"] gpu,
            nvidiaInfoPowerDrawWatts =
              firstElement
                [ ["gpu_power_readings", "average_power_draw"],
                  ["gpu_power_readings", "instant_power_draw"],
                  ["power_readings", "power_draw"]
                ]
                gpu,
            nvidiaInfoPowerLimitWatts =
              firstElement
                [ ["gpu_power_readings", "current_power_limit"],
                  ["gpu_power_readings", "requested_power_limit"],
                  ["power_readings", "power_limit"]
                ]
                gpu,
            nvidiaInfoPerformanceState = availableElementText ["performance_state"] gpu
          }

-- | Read a rich snapshot using @nvidia-smi@ from @PATH@.
readNvidiaGpuInfo :: IO [NvidiaGpuInfo]
readNvidiaGpuInfo = readNvidiaGpuInfoWith "nvidia-smi"

-- | Read a rich snapshot using the supplied @nvidia-smi@ executable.
-- Returns an empty list when the command is missing, exits unsuccessfully, or
-- is skipped because every detected NVIDIA PCI device is runtime-suspended.
readNvidiaGpuInfoWith :: FilePath -> IO [NvidiaGpuInfo]
readNvidiaGpuInfoWith command =
  fromMaybe [] <$> readNvidiaGpuInfoUpdateWith command

-- | Decide whether querying NVIDIA is safe from the runtime power states of
-- the detected NVIDIA PCI devices. A query is skipped only when at least one
-- device was detected and every device explicitly reports that it is suspended
-- or suspending. Missing and unknown states preserve the historical behavior
-- of running @nvidia-smi@.
shouldQueryNvidiaForRuntimeStatuses :: [Maybe T.Text] -> Bool
shouldQueryNvidiaForRuntimeStatuses [] = True
shouldQueryNvidiaForRuntimeStatuses statuses =
  any (maybe True (`notElem` lowPowerStatuses) . fmap T.strip) statuses
  where
    lowPowerStatuses = ["suspended", "suspending"]

newtype NvidiaGpuInfoChanVar
  = NvidiaGpuInfoChanVar (TChan [NvidiaGpuInfo], MVar [NvidiaGpuInfo])

-- | Get a shared broadcast channel of rich NVIDIA snapshots.
--
-- The first call starts one polling producer for the process; subsequent calls
-- reuse it. Consequently, the command and interval from the first call win.
getNvidiaGpuInfoChan :: Double -> TaffyIO (TChan [NvidiaGpuInfo])
getNvidiaGpuInfoChan = getNvidiaGpuInfoChanWith "nvidia-smi"

-- | Like 'getNvidiaGpuInfoChan', using a supplied @nvidia-smi@ executable.
getNvidiaGpuInfoChanWith :: FilePath -> Double -> TaffyIO (TChan [NvidiaGpuInfo])
getNvidiaGpuInfoChanWith command interval = do
  NvidiaGpuInfoChanVar (chan, _) <- setupNvidiaGpuInfoChanVar command interval
  pure chan

-- | Read the latest snapshot cached by 'getNvidiaGpuInfoChan'.
getNvidiaGpuInfoState :: Double -> TaffyIO [NvidiaGpuInfo]
getNvidiaGpuInfoState = getNvidiaGpuInfoStateWith "nvidia-smi"

-- | Like 'getNvidiaGpuInfoState', using a supplied @nvidia-smi@ executable.
getNvidiaGpuInfoStateWith :: FilePath -> Double -> TaffyIO [NvidiaGpuInfo]
getNvidiaGpuInfoStateWith command interval = do
  NvidiaGpuInfoChanVar (_, var) <- setupNvidiaGpuInfoChanVar command interval
  liftIO $ readMVar var

setupNvidiaGpuInfoChanVar :: FilePath -> Double -> TaffyIO NvidiaGpuInfoChanVar
setupNvidiaGpuInfoChanVar command interval = do
  wakeupChan <- getWakeupChannelForDelay $ max 0.000001 interval
  ourWakeupChan <- liftIO $ atomically $ dupTChan wakeupChan
  getStateDefault $ liftIO $ do
    initialInfo <- fromMaybe [] <$> readNvidiaGpuInfoUpdateWith command
    chan <- newBroadcastTChanIO
    var <- newMVar initialInfo
    void $ forkIO $ forever $ do
      void $ atomically $ readTChan ourWakeupChan
      maybeInfo <- readNvidiaGpuInfoUpdateWith command
      for_ maybeInfo $ \info -> do
        void $ swapMVar var info
        atomically $ writeTChan chan info
    pure $ NvidiaGpuInfoChanVar (chan, var)

-- | A temperature reported for one NVIDIA GPU.
--
-- This small compatibility type is retained for callers that only need the
-- current core temperature. New code should prefer 'NvidiaGpuInfo'.
data NvidiaGpuTemperature = NvidiaGpuTemperature
  { nvidiaGpuIndex :: Int,
    nvidiaGpuTemperatureCelsius :: Double
  }
  deriving (Eq, Show)

-- | Parse @index, temperature.gpu@ rows emitted by @nvidia-smi@.
-- Invalid rows, including unavailable temperature values, are ignored.
parseNvidiaGpuTemperatures :: T.Text -> [NvidiaGpuTemperature]
parseNvidiaGpuTemperatures =
  sortOn nvidiaGpuIndex . mapMaybe parseLine . T.lines
  where
    parseLine line =
      case map T.strip $ T.splitOn "," line of
        [indexText, temperatureText] ->
          NvidiaGpuTemperature
            <$> readMaybe (T.unpack indexText)
            <*> readMaybe (T.unpack temperatureText)
        _ -> Nothing

-- | Read core temperatures using @nvidia-smi@ from @PATH@.
readNvidiaGpuTemperatures :: IO [NvidiaGpuTemperature]
readNvidiaGpuTemperatures = readNvidiaGpuTemperaturesWith "nvidia-smi"

-- | Read core temperatures using the supplied @nvidia-smi@ executable.
readNvidiaGpuTemperaturesWith :: FilePath -> IO [NvidiaGpuTemperature]
readNvidiaGpuTemperaturesWith command = do
  result <-
    runNvidiaSmi
      command
      [ "--query-gpu=index,temperature.gpu",
        "--format=csv,noheader,nounits"
      ]
  pure $ case result of
    NvidiaSmiOutput output -> parseNvidiaGpuTemperatures $ T.pack output
    NvidiaSmiSkipped -> []
    NvidiaSmiFailed -> []

data NvidiaSmiResult
  = NvidiaSmiSkipped
  | NvidiaSmiFailed
  | NvidiaSmiOutput String

readNvidiaGpuInfoUpdateWith :: FilePath -> IO (Maybe [NvidiaGpuInfo])
readNvidiaGpuInfoUpdateWith command = do
  result <- runNvidiaSmi command ["-q", "-x"]
  pure $ case result of
    NvidiaSmiSkipped -> Nothing
    NvidiaSmiFailed -> Just []
    NvidiaSmiOutput output -> Just $ parseNvidiaGpuInfo $ T.pack output

runNvidiaSmi :: FilePath -> [String] -> IO NvidiaSmiResult
runNvidiaSmi command arguments = do
  statuses <- nvidiaPciRuntimeStatuses
  if shouldQueryNvidiaForRuntimeStatuses statuses
    then do
      result <-
        try (readProcessWithExitCode command arguments "") ::
          IO (Either IOException (ExitCode, String, String))
      pure $ case result of
        Right (ExitSuccess, output, _) -> NvidiaSmiOutput output
        _ -> NvidiaSmiFailed
    else pure NvidiaSmiSkipped

nvidiaPciRuntimeStatuses :: IO [Maybe T.Text]
nvidiaPciRuntimeStatuses = do
  let driverPath = "/sys/bus/pci/drivers/nvidia"
  entriesResult <- try (listDirectory driverPath) :: IO (Either IOException [FilePath])
  case entriesResult of
    Left _ -> pure []
    Right entries ->
      traverse (readRuntimeStatus . (driverPath </>)) $
        filter isPciAddress entries

readRuntimeStatus :: FilePath -> IO (Maybe T.Text)
readRuntimeStatus devicePath = do
  result <-
    try (T.strip <$> TIO.readFile (devicePath </> "power/runtime_status")) ::
      IO (Either IOException T.Text)
  pure $ either (const Nothing) Just result

isPciAddress :: FilePath -> Bool
isPciAddress entry =
  case T.split (\character -> character == ':' || character == '.') $ T.pack entry of
    [domain, bus, device, function] ->
      and
        [ T.length domain == 4,
          T.length bus == 2,
          T.length device == 2,
          T.length function == 1,
          all (all isHexDigit . T.unpack) [domain, bus, device, function]
        ]
    _ -> False

elementAt :: [String] -> Element -> Maybe Element
elementAt [] element = Just element
elementAt (name : rest) element =
  findChild (unqual name) element >>= elementAt rest

elementText :: [String] -> Element -> Maybe T.Text
elementText path element = T.strip . T.pack . strContent <$> elementAt path element

availableElementText :: [String] -> Element -> Maybe T.Text
availableElementText path element = do
  value <- elementText path element
  if value `elem` ["", "N/A", "[N/A]"] then Nothing else Just value

readElement :: (Read a) => [String] -> Element -> Maybe a
readElement path element = do
  value <- availableElementText path element
  case reads $ T.unpack value of
    [(number, _)] -> Just number
    _ -> Nothing

firstElement :: (Read a) => [[String]] -> Element -> Maybe a
firstElement paths element =
  case mapMaybe (`readElement` element) paths of
    value : _ -> Just value
    [] -> Nothing
