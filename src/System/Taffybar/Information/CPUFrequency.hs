{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : System.Taffybar.Information.CPUFrequency
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Generic Linux CPU-frequency information backed by one shared poller per
-- Taffybar process. Widgets on separate monitor bars reuse the same cached
-- snapshot and broadcast channel through the shared 'Context'.
module System.Taffybar.Information.CPUFrequency
  ( CPUFrequencyInfo (..),
    cpuFrequencyAverageGHz,
    cpuFrequencyMinimumGHz,
    cpuFrequencyMaximumGHz,
    summarizeCPUFrequencies,
    readCPUFrequencyInfo,
    getCPUFrequencyInfoChan,
    getCPUFrequencyInfoState,
  )
where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newMVar, readMVar, swapMVar)
import Control.Concurrent.STM.TChan (TChan, dupTChan, newBroadcastTChanIO, readTChan, writeTChan)
import Control.Exception (SomeException, try)
import Control.Monad (filterM, forever, void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.STM (atomically)
import qualified Data.ByteString.Char8 as BS8
import Data.Either (fromRight)
import Data.List (isPrefixOf, sort)
import Data.Maybe (catMaybes, mapMaybe)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath ((</>))
import System.Taffybar.Context (TaffyIO, getStateDefault)
import System.Taffybar.Information.Wakeup (getWakeupChannelForDelay)
import Text.Read (readMaybe)

-- | A frequency snapshot in kHz, matching Linux's cpufreq sysfs units.
data CPUFrequencyInfo = CPUFrequencyInfo
  { cpuFrequencyAverageKHz :: Maybe Integer,
    cpuFrequencyMinimumKHz :: Maybe Integer,
    cpuFrequencyMaximumKHz :: Maybe Integer,
    cpuFrequencySampleCount :: Int
  }
  deriving (Eq, Show)

emptyCPUFrequencyInfo :: CPUFrequencyInfo
emptyCPUFrequencyInfo = CPUFrequencyInfo Nothing Nothing Nothing 0

cpuFrequencyAverageGHz :: CPUFrequencyInfo -> Maybe Double
cpuFrequencyAverageGHz = fmap kHzToGHz . cpuFrequencyAverageKHz

cpuFrequencyMinimumGHz :: CPUFrequencyInfo -> Maybe Double
cpuFrequencyMinimumGHz = fmap kHzToGHz . cpuFrequencyMinimumKHz

cpuFrequencyMaximumGHz :: CPUFrequencyInfo -> Maybe Double
cpuFrequencyMaximumGHz = fmap kHzToGHz . cpuFrequencyMaximumKHz

kHzToGHz :: Integer -> Double
kHzToGHz value = fromIntegral value / 1_000_000

-- | Summarize a set of per-policy or per-core frequency readings.
summarizeCPUFrequencies :: [Integer] -> CPUFrequencyInfo
summarizeCPUFrequencies [] = emptyCPUFrequencyInfo
summarizeCPUFrequencies values =
  CPUFrequencyInfo
    { cpuFrequencyAverageKHz = Just $ sum values `div` fromIntegral (length values),
      cpuFrequencyMinimumKHz = Just $ minimum values,
      cpuFrequencyMaximumKHz = Just $ maximum values,
      cpuFrequencySampleCount = length values
    }

-- | Read current frequencies from the generic Linux cpufreq interface.
-- Falls back to @/proc/cpuinfo@ on systems without cpufreq sysfs entries.
readCPUFrequencyInfo :: IO CPUFrequencyInfo
readCPUFrequencyInfo = do
  result <- try readCPUFrequencyInfoUnsafe
  pure $ fromRight emptyCPUFrequencyInfo (result :: Either SomeException CPUFrequencyInfo)

readCPUFrequencyInfoUnsafe :: IO CPUFrequencyInfo
readCPUFrequencyInfoUnsafe = do
  paths <- discoverCPUFrequencyPaths
  readCPUFrequencyInfoFromPaths paths

discoverCPUFrequencyPaths :: IO [[FilePath]]
discoverCPUFrequencyPaths = do
  policyPaths <- discoverPolicyFrequencyPaths
  if null policyPaths then discoverCoreFrequencyPaths else pure policyPaths

readCPUFrequencyInfoFromPaths :: [[FilePath]] -> IO CPUFrequencyInfo
readCPUFrequencyInfoFromPaths paths = do
  values <- catMaybes <$> mapM readFrequencyPath paths
  if null values
    then summarizeCPUFrequencies <$> readProcCPUInfoFrequencies
    else pure $ summarizeCPUFrequencies values

discoverPolicyFrequencyPaths :: IO [[FilePath]]
discoverPolicyFrequencyPaths =
  discoverFrequencyPaths
    "/sys/devices/system/cpu/cpufreq"
    ("policy" `isPrefixOf`)
    (</>)

discoverCoreFrequencyPaths :: IO [[FilePath]]
discoverCoreFrequencyPaths =
  discoverFrequencyPaths
    "/sys/devices/system/cpu"
    isCPUCoreDirectory
    (\base entry -> base </> entry </> "cpufreq")

discoverFrequencyPaths :: FilePath -> (FilePath -> Bool) -> (FilePath -> FilePath -> FilePath) -> IO [[FilePath]]
discoverFrequencyPaths base acceptEntry entryDirectory = do
  exists <- doesDirectoryExist base
  if not exists
    then pure []
    else do
      entries <- sort . filter acceptEntry <$> listDirectory base
      filterM (fmap or . mapM doesFileExist) $
        map
          ( \entry ->
              let directory = entryDirectory base entry
               in [directory </> "scaling_cur_freq", directory </> "cpuinfo_cur_freq"]
          )
          entries

isCPUCoreDirectory :: FilePath -> Bool
isCPUCoreDirectory name =
  "cpu" `isPrefixOf` name
    && not (null suffix)
    && all (`elem` (['0' .. '9'] :: String)) suffix
  where
    suffix = drop 3 name

readFrequencyPath :: [FilePath] -> IO (Maybe Integer)
readFrequencyPath [] = pure Nothing
readFrequencyPath (path : rest) = do
  result <- try $ BS8.readFile path
  case result :: Either SomeException BS8.ByteString of
    Right contents ->
      case readMaybe (BS8.unpack $ BS8.takeWhile (`notElem` ['\n', '\r', ' ', '\t']) contents) of
        Just value -> pure $ Just value
        Nothing -> readFrequencyPath rest
    Left _ -> readFrequencyPath rest

readProcCPUInfoFrequencies :: IO [Integer]
readProcCPUInfoFrequencies = do
  result <- try $ BS8.readFile "/proc/cpuinfo"
  pure $ case result :: Either SomeException BS8.ByteString of
    Left _ -> []
    Right contents -> mapMaybe parseMHzLine $ lines $ BS8.unpack contents
  where
    parseMHzLine line = case break (== ':') line of
      (key, ':' : value)
        | words key == ["cpu", "MHz"] ->
            (\mhz -> round (mhz * 1000)) <$> (readMaybe value :: Maybe Double)
      _ -> Nothing

newtype CPUFrequencyInfoChanVar
  = CPUFrequencyInfoChanVar (TChan CPUFrequencyInfo, MVar CPUFrequencyInfo)

-- | Return the process-wide frequency stream. The first caller's interval
-- wins; later widgets reuse the same sampler.
getCPUFrequencyInfoChan :: Double -> TaffyIO (TChan CPUFrequencyInfo)
getCPUFrequencyInfoChan interval = do
  CPUFrequencyInfoChanVar (chan, _) <- setupCPUFrequencyInfoChanVar interval
  pure chan

-- | Read the latest snapshot from the shared frequency sampler.
getCPUFrequencyInfoState :: Double -> TaffyIO CPUFrequencyInfo
getCPUFrequencyInfoState interval = do
  CPUFrequencyInfoChanVar (_, infoVar) <- setupCPUFrequencyInfoChanVar interval
  liftIO $ readMVar infoVar

setupCPUFrequencyInfoChanVar :: Double -> TaffyIO CPUFrequencyInfoChanVar
setupCPUFrequencyInfoChanVar interval = getStateDefault $ do
  wakeupChan <- getWakeupChannelForDelay $ max 0.000001 interval
  ourWakeupChan <- liftIO $ atomically $ dupTChan wakeupChan
  liftIO $ do
    paths <- discoverCPUFrequencyPaths
    initialInfo <- readCPUFrequencyInfoFromPaths paths
    chan <- newBroadcastTChanIO
    infoVar <- newMVar initialInfo
    pathsVar <- newMVar paths
    void $ forkIO $ forever $ do
      void $ atomically $ readTChan ourWakeupChan
      currentPaths <- readMVar pathsVar
      currentValues <- catMaybes <$> mapM readFrequencyPath currentPaths
      values <-
        if null currentValues
          then do
            refreshedPaths <- discoverCPUFrequencyPaths
            refreshedValues <- catMaybes <$> mapM readFrequencyPath refreshedPaths
            void $ swapMVar pathsVar refreshedPaths
            pure refreshedValues
          else pure currentValues
      info <-
        if null values
          then summarizeCPUFrequencies <$> readProcCPUInfoFrequencies
          else pure $ summarizeCPUFrequencies values
      old <- swapMVar infoVar info
      when (info /= old) $ atomically $ writeTChan chan info
    pure $ CPUFrequencyInfoChanVar (chan, infoVar)
