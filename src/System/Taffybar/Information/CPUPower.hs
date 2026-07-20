{-# LANGUAGE NumericUnderscores #-}

-- |
-- Module      : System.Taffybar.Information.CPUPower
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Linux CPU package-power information derived from the RAPL energy counter.
-- One process-wide sampler feeds all widgets through a broadcast channel.
module System.Taffybar.Information.CPUPower
  ( CPUPowerInfo (..),
    EnergySample (..),
    calculateCPUPowerWatts,
    readCPUPowerInfo,
    getCPUPowerInfoChan,
    getCPUPowerInfoState,
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
import Data.List (isPrefixOf, sort)
import Data.Maybe (catMaybes, listToMaybe)
import GHC.Clock (getMonotonicTimeNSec)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.FilePath ((</>))
import System.Taffybar.Context (TaffyIO, getStateDefault)
import System.Taffybar.Information.Wakeup (getWakeupChannelForDelay)
import Text.Read (readMaybe)

data CPUPowerInfo = CPUPowerInfo
  { cpuPackagePowerWatts :: Maybe Double,
    cpuPowerDomainName :: Maybe String
  }
  deriving (Eq, Show)

data EnergySample = EnergySample
  { energySampleMicrojoules :: Integer,
    energySampleMonotonicNanoseconds :: Integer
  }
  deriving (Eq, Show)

data CPUPowerDomain = CPUPowerDomain
  { cpuPowerDomainPath :: FilePath,
    cpuPowerDomainLabel :: String,
    cpuPowerMaximumEnergyMicrojoules :: Integer
  }
  deriving (Eq, Show)

emptyCPUPowerInfo :: CPUPowerInfo
emptyCPUPowerInfo = CPUPowerInfo Nothing Nothing

-- | Calculate average package power between two energy samples. The maximum
-- energy range is used to account for the kernel counter wrapping around.
calculateCPUPowerWatts :: Integer -> EnergySample -> EnergySample -> Maybe Double
calculateCPUPowerWatts maximumEnergy previous current
  | elapsedNanoseconds <= 0 = Nothing
  | energyDelta < 0 = Nothing
  | otherwise =
      Just $
        fromIntegral energyDelta
          * 1_000
          / fromIntegral elapsedNanoseconds
  where
    previousEnergy = energySampleMicrojoules previous
    currentEnergy = energySampleMicrojoules current
    energyDelta
      | currentEnergy >= previousEnergy = currentEnergy - previousEnergy
      | maximumEnergy > previousEnergy = maximumEnergy - previousEnergy + currentEnergy
      | otherwise = -1
    elapsedNanoseconds =
      energySampleMonotonicNanoseconds current
        - energySampleMonotonicNanoseconds previous

-- | Take one package-power reading. The first reading has no wattage because
-- RAPL exposes accumulated energy; a second sample is needed to derive power.
readCPUPowerInfo :: IO CPUPowerInfo
readCPUPowerInfo = do
  maybeDomain <- discoverCPUPowerDomain
  pure $
    case maybeDomain of
      Nothing -> emptyCPUPowerInfo
      Just domain -> CPUPowerInfo Nothing (Just $ cpuPowerDomainLabel domain)

discoverCPUPowerDomain :: IO (Maybe CPUPowerDomain)
discoverCPUPowerDomain = do
  domains <- discoverDomainsInBases powercapBases
  pure $ listToMaybe domains
  where
    -- Prefer the MSR-backed package counter. The MMIO interface commonly
    -- exposes the same package and would otherwise double-count it.
    powercapBases =
      [ "/sys/devices/virtual/powercap/intel-rapl",
        "/sys/class/powercap"
      ]

discoverDomainsInBases :: [FilePath] -> IO [CPUPowerDomain]
discoverDomainsInBases [] = pure []
discoverDomainsInBases (base : rest) = do
  domains <- discoverDomainsInBase base
  if null domains then discoverDomainsInBases rest else pure domains

discoverDomainsInBase :: FilePath -> IO [CPUPowerDomain]
discoverDomainsInBase base = do
  exists <- doesDirectoryExist base
  if not exists
    then pure []
    else do
      entriesResult <- try $ listDirectory base
      case entriesResult :: Either SomeException [FilePath] of
        Left _ -> pure []
        Right entries -> do
          candidates <- filterM (doesFileExist . (</> "energy_uj")) $ map (base </>) $ sort entries
          catMaybes <$> mapM readPowerDomain candidates

readPowerDomain :: FilePath -> IO (Maybe CPUPowerDomain)
readPowerDomain path = do
  maybeName <- readStringFile $ path </> "name"
  maybeMaximum <- readIntegerFile $ path </> "max_energy_range_uj"
  pure $ case (maybeName, maybeMaximum) of
    (Just name, Just maximumEnergy)
      | "package-" `isPrefixOf` name ->
          Just $
            CPUPowerDomain
              { cpuPowerDomainPath = path,
                cpuPowerDomainLabel = name,
                cpuPowerMaximumEnergyMicrojoules = maximumEnergy
              }
    _ -> Nothing

readEnergySample :: CPUPowerDomain -> IO (Maybe EnergySample)
readEnergySample domain = do
  maybeEnergy <- readIntegerFile $ cpuPowerDomainPath domain </> "energy_uj"
  case maybeEnergy of
    Nothing -> pure Nothing
    Just energy -> do
      timestamp <- fromIntegral <$> getMonotonicTimeNSec
      pure $ Just $ EnergySample energy timestamp

readStringFile :: FilePath -> IO (Maybe String)
readStringFile path = do
  result <- try $ BS8.readFile path
  pure $ case result :: Either SomeException BS8.ByteString of
    Left _ -> Nothing
    Right contents -> Just $ BS8.unpack $ BS8.takeWhile (`notElem` ['\n', '\r']) contents

readIntegerFile :: FilePath -> IO (Maybe Integer)
readIntegerFile path = do
  maybeContents <- readStringFile path
  pure $ maybeContents >>= readMaybe

data CPUPowerSamplerState = CPUPowerSamplerState
  { samplerDomain :: Maybe CPUPowerDomain,
    samplerPreviousSample :: Maybe EnergySample
  }

newtype CPUPowerInfoChanVar
  = CPUPowerInfoChanVar (TChan CPUPowerInfo, MVar CPUPowerInfo)

-- | Return the process-wide package-power stream. The first caller's interval
-- wins; later widgets reuse the same sampler.
getCPUPowerInfoChan :: Double -> TaffyIO (TChan CPUPowerInfo)
getCPUPowerInfoChan interval = do
  CPUPowerInfoChanVar (chan, _) <- setupCPUPowerInfoChanVar interval
  pure chan

-- | Read the latest snapshot from the shared package-power sampler.
getCPUPowerInfoState :: Double -> TaffyIO CPUPowerInfo
getCPUPowerInfoState interval = do
  CPUPowerInfoChanVar (_, infoVar) <- setupCPUPowerInfoChanVar interval
  liftIO $ readMVar infoVar

setupCPUPowerInfoChanVar :: Double -> TaffyIO CPUPowerInfoChanVar
setupCPUPowerInfoChanVar interval = getStateDefault $ do
  wakeupChan <- getWakeupChannelForDelay $ max 0.000001 interval
  ourWakeupChan <- liftIO $ atomically $ dupTChan wakeupChan
  liftIO $ do
    domain <- discoverCPUPowerDomain
    initialSample <- maybe (pure Nothing) readEnergySample domain
    let initialInfo = CPUPowerInfo Nothing (cpuPowerDomainLabel <$> domain)
        initialSamplerState = CPUPowerSamplerState domain initialSample
    chan <- newBroadcastTChanIO
    infoVar <- newMVar initialInfo
    samplerVar <- newMVar initialSamplerState
    void $ forkIO $ forever $ do
      void $ atomically $ readTChan ourWakeupChan
      oldSamplerState <- readMVar samplerVar
      (info, newSamplerState) <- sampleCPUPower oldSamplerState
      void $ swapMVar samplerVar newSamplerState
      oldInfo <- swapMVar infoVar info
      when (info /= oldInfo) $ atomically $ writeTChan chan info
    pure $ CPUPowerInfoChanVar (chan, infoVar)

sampleCPUPower :: CPUPowerSamplerState -> IO (CPUPowerInfo, CPUPowerSamplerState)
sampleCPUPower oldState = do
  domain <- maybe discoverCPUPowerDomain (pure . Just) $ samplerDomain oldState
  case domain of
    Nothing -> pure (emptyCPUPowerInfo, CPUPowerSamplerState Nothing Nothing)
    Just currentDomain -> do
      sample <- readEnergySample currentDomain
      case sample of
        Nothing ->
          pure
            ( CPUPowerInfo Nothing (Just $ cpuPowerDomainLabel currentDomain),
              CPUPowerSamplerState Nothing Nothing
            )
        Just currentSample -> do
          let watts =
                samplerPreviousSample oldState
                  >>= \previousSample ->
                    calculateCPUPowerWatts
                      (cpuPowerMaximumEnergyMicrojoules currentDomain)
                      previousSample
                      currentSample
          pure
            ( CPUPowerInfo watts (Just $ cpuPowerDomainLabel currentDomain),
              CPUPowerSamplerState (Just currentDomain) (Just currentSample)
            )
