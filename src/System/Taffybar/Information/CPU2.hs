{-# LANGUAGE TupleSections #-}

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      : System.Taffybar.Information.CPU2
-- Copyright   : (c) José A. Romero L.
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : José A. Romero L. <escherdragon@gmail.com>
-- Stability   : unstable
-- Portability : unportable
--
-- Provides information about used CPU times, obtained from parsing the
-- @\/proc\/stat@ file using some of the facilities included in the
-- "System.Taffybar.Information.StreamInfo" module.
-- And also provides information about the temperature of cores.
-- (Now supports only physical cpu).
module System.Taffybar.Information.CPU2 where

import Control.Concurrent (ThreadId, forkIO, killThread)
import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newMVar)
import Control.Concurrent.STM (STM, orElse)
import Control.Concurrent.STM.TChan
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.STM (atomically)
import Control.Monad.Trans.Reader (asks)
import Data.IORef
import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Word (Word64)
import Safe
import System.Directory
import System.FilePath
import System.Log.Logger (Priority (DEBUG), logM)
import System.Taffybar.Context (TaffyIO, getStateDefault, wakeupManager)
import System.Taffybar.Information.StreamInfo
import System.Taffybar.Information.Wakeup
  ( WakeupSubscription (..),
    intervalSecondsToNanoseconds,
  )
import System.Taffybar.Information.Wakeup.Manager
  ( WakeupManager,
    registerWakeupInterval,
    subscribeWakeupInterval,
  )
import Text.Read (readMaybe)

-- | Relative CPU load values, expressed as ratios in [0,1].
data CPULoad = CPULoad
  { cpuUserLoad :: Double,
    cpuSystemLoad :: Double,
    cpuTotalLoad :: Double
  }
  deriving (Eq, Show)

-- | Returns a list of 5 to 7 elements containing all the values available for
-- the given core (or all of them aggregated, if "cpu" is passed).
{-# DEPRECATED getCPUInfo "Legacy low-level API. Use getCPULoadChan (preferred) or sampleCPULoad." #-}
getCPUInfo :: String -> IO [Int]
getCPUInfo = getParsedInfo "/proc/stat" parse

-- | Parse @/proc/stat@ contents into CPU-name/sample tuples.
parse :: String -> [(String, [Int])]
parse = mapMaybe (tuplize . words) . filter (\x -> take 3 x == "cpu") . lines

-- | Convert tokenized @/proc/stat@ lines into a typed CPU sample entry.
tuplize :: [String] -> Maybe (String, [Int])
tuplize s = do
  cpu <- s `atMay` 0
  return (cpu, map (readDef (-1)) (tailSafe s))

-- | Returns a two-element list containing relative system and user times
-- calculated using two almost simultaneous samples of the @\/proc\/stat@ file
-- for the given core (or all of them aggregated, if \"cpu\" is passed).
{-# DEPRECATED getCPULoad "Legacy polling API. Use getCPULoadChan (preferred) or sampleCPULoad." #-}
getCPULoad :: String -> IO [Double]
getCPULoad cpu = do
  load <- getLoad 0.05 $ getCPUInfo cpu
  case load of
    l0 : l1 : l2 : _ -> return [l0 + l1, l2]
    _ -> return []

-- | Sample CPU usage for a given core over the provided interval (seconds).
sampleCPULoad :: Double -> String -> IO CPULoad
sampleCPULoad interval cpu = toCPULoad <$> getLoad interval (getCPUInfo cpu)

-- | A shared CPU information producer with an optional temporary fast cadence.
-- The fast cadence still comes from the coordinated wakeup scheduler and is
-- removed entirely when its final lease is released.
data CPULoadSource = CPULoadSource
  { cpuLoadSourceChannel :: TChan CPULoad,
    forceCPULoadRefresh :: IO (),
    acquireCPULoadFastRefresh :: IO (IO ())
  }

type CPULoadSourceKey = (String, Word64, Word64)

newtype CPULoadSources
  = CPULoadSources (MVar (M.Map CPULoadSourceKey CPULoadSource))

data FastRefreshState
  = FastRefreshInactive
  | FastRefreshActive !Int !ThreadId !WakeupSubscription

cpuLoadLogPath :: String
cpuLoadLogPath = "System.Taffybar.Information.CPU2"

defaultCPUFastRefreshInterval :: Double -> Double
defaultCPUFastRefreshInterval baseInterval = min baseInterval 0.5

-- | Return a process-wide CPU source keyed by CPU name and both cadences. The
-- normal cadence is permanently registered; the fast cadence is only active
-- while at least one caller holds a lease from 'acquireCPULoadFastRefresh'.
getCPULoadSource :: String -> Double -> Double -> TaffyIO CPULoadSource
getCPULoadSource cpu baseInterval fastInterval = do
  baseNs <- intervalNanosecondsOrFail baseInterval
  fastNs <- intervalNanosecondsOrFail fastInterval
  manager <- asks wakeupManager
  CPULoadSources sourcesVar <-
    getStateDefault $ liftIO $ CPULoadSources <$> newMVar M.empty
  liftIO $
    modifyMVar sourcesVar $ \sources ->
      case M.lookup (cpu, baseNs, fastNs) sources of
        Just source -> pure (sources, source)
        Nothing -> do
          source <- createCPULoadSource manager cpu baseNs fastNs
          pure (M.insert (cpu, baseNs, fastNs) source sources, source)

intervalNanosecondsOrFail :: Double -> TaffyIO Word64
intervalNanosecondsOrFail interval =
  case intervalSecondsToNanoseconds (max 0.000001 interval) of
    Left err -> fail err
    Right intervalNs -> pure intervalNs

createCPULoadSource :: WakeupManager -> String -> Word64 -> Word64 -> IO CPULoadSource
createCPULoadSource manager cpu baseIntervalNs fastIntervalNs = do
  baseChannel <- registerWakeupInterval manager baseIntervalNs
  ourBaseChannel <- atomically $ dupTChan baseChannel
  refreshChannel <- newTChanIO
  outputChannel <- newBroadcastTChanIO
  initial <- getCPUInfo cpu
  sampleRef <- newIORef initial
  fastStateVar <- newMVar FastRefreshInactive
  void $ forkIO $ forever $ do
    atomically $ do
      void (readTChan ourBaseChannel) `orElse` void (readTChan refreshChannel)
      drainTChan ourBaseChannel
      drainTChan refreshChannel
    load <- toCPULoad <$> getAccLoad sampleRef (getCPUInfo cpu)
    atomically $ writeTChan outputChannel load
  let forceRefresh = atomically $ writeTChan refreshChannel ()
  pure
    CPULoadSource
      { cpuLoadSourceChannel = outputChannel,
        forceCPULoadRefresh = forceRefresh,
        acquireCPULoadFastRefresh = do
          release <-
            if fastIntervalNs >= baseIntervalNs
              then pure $ pure ()
              else acquireFastRefresh manager fastIntervalNs refreshChannel fastStateVar
          forceRefresh
          pure release
      }

acquireFastRefresh :: WakeupManager -> Word64 -> TChan () -> MVar FastRefreshState -> IO (IO ())
acquireFastRefresh manager intervalNs refreshChannel stateVar = do
  modifyMVar_ stateVar $ \state ->
    case state of
      FastRefreshActive count threadId subscription ->
        pure $ FastRefreshActive (count + 1) threadId subscription
      FastRefreshInactive -> do
        logM cpuLoadLogPath DEBUG $ "Acquiring fast CPU refresh lease at " <> show intervalNs <> "ns"
        subscription <- subscribeWakeupInterval manager intervalNs
        ourChannel <- atomically $ dupTChan $ wakeupSubscriptionChannel subscription
        threadId <-
          forkIO $
            forever $
              atomically (readTChan ourChannel)
                >> atomically (writeTChan refreshChannel ())
        pure $ FastRefreshActive 1 threadId subscription
  releasedRef <- newIORef False
  pure $ do
    shouldRelease <- atomicModifyIORef' releasedRef $ \released -> (True, not released)
    when shouldRelease $
      modifyMVar_ stateVar $ \state ->
        case state of
          FastRefreshInactive -> pure FastRefreshInactive
          FastRefreshActive count threadId subscription
            | count > 1 -> pure $ FastRefreshActive (count - 1) threadId subscription
            | otherwise -> do
                logM cpuLoadLogPath DEBUG $ "Releasing fast CPU refresh lease at " <> show intervalNs <> "ns"
                killThread threadId
                releaseWakeupSubscription subscription
                pure FastRefreshInactive

drainTChan :: TChan a -> STM ()
drainTChan chan = do
  next <- tryReadTChan chan
  case next of
    Nothing -> pure ()
    Just _ -> drainTChan chan

-- | Build a broadcast channel that is fed by a shared polling producer.
--
-- The polling thread is paced by the coordinated wakeup scheduler so CPU
-- sampling aligns with other interval-driven widgets.
--
-- Repeated calls with the same CPU and interval reuse one producer.
getCPULoadChan :: String -> Double -> TaffyIO (TChan CPULoad)
getCPULoadChan cpu interval = do
  source <- getCPULoadSource cpu interval (defaultCPUFastRefreshInterval interval)
  pure $ cpuLoadSourceChannel source

toCPULoad :: [Double] -> CPULoad
toCPULoad load =
  case load of
    l0 : l1 : l2 : _ ->
      CPULoad
        { cpuUserLoad = l0 + l1,
          cpuSystemLoad = l2,
          cpuTotalLoad = l0 + l1 + l2
        }
    _ ->
      CPULoad
        { cpuUserLoad = 0,
          cpuSystemLoad = 0,
          cpuTotalLoad = 0
        }

-- | Get the directory in which core temperature files are kept.
getCPUTemperatureDirectory :: IO FilePath
getCPUTemperatureDirectory =
  (baseDir </>)
    . fromMaybe "hwmon0"
    . find (isPrefixOf "hwmon")
    <$> listDirectory baseDir
  where
    baseDir =
      "/"
        </> "sys"
        </> "bus"
        </> "platform"
        </> "devices"
        </> "coretemp.0"
        </> "hwmon"

-- | Read one core-temperature file from sysfs and convert milli-degrees to
-- degrees Celsius.
readCPUTempFile :: FilePath -> IO Double
readCPUTempFile cpuTempFilePath =
  maybe 0 (/ 1000) . readMaybe <$> readFile cpuTempFilePath

-- | List core-temperature input files in a hwmon directory.
getAllTemperatureFiles :: FilePath -> IO [FilePath]
getAllTemperatureFiles temperaturesDirectory =
  filter (liftM2 (&&) (isPrefixOf "temp") (isSuffixOf "input"))
    <$> listDirectory temperaturesDirectory

-- | Read all available CPU core temperatures from sysfs.
getCPUTemperatures :: IO [(String, Double)]
getCPUTemperatures = do
  dir <- getCPUTemperatureDirectory
  let mkPair filename = (filename,) <$> readCPUTempFile (dir </> filename)
  getAllTemperatureFiles dir >>= mapM mkPair
