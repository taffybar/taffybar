{-# LANGUAGE NamedFieldPuns #-}
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

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM.TChan
import Control.Monad
import Control.Monad.STM (atomically)
import Data.IORef
import Data.List
import Data.Maybe
import Safe
import System.Directory
import System.FilePath
import System.Taffybar.Information.StreamInfo

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

parse :: String -> [(String, [Int])]
parse = mapMaybe (tuplize . words) . filter (\x -> take 3 x == "cpu") . lines

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

-- | Build a broadcast channel that is fed by a polling thread.
--
-- Each channel has its own polling thread; if multiple widgets should share a
-- data source, create once and reuse the returned channel.
getCPULoadChan :: String -> Double -> IO (TChan CPULoad)
getCPULoadChan cpu interval = do
  chan <- newBroadcastTChanIO
  initial <- getCPUInfo cpu
  sample <- newIORef initial
  let delayMicroseconds = max 1 (floor $ interval * 1000000)
  _ <- forkIO $ forever $ do
    load <- toCPULoad <$> getAccLoad sample (getCPUInfo cpu)
    atomically $ writeTChan chan load
    threadDelay delayMicroseconds
  return chan

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

readCPUTempFile :: FilePath -> IO Double
readCPUTempFile cpuTempFilePath = (/ 1000) . read <$> readFile cpuTempFilePath

getAllTemperatureFiles :: FilePath -> IO [FilePath]
getAllTemperatureFiles temperaturesDirectory =
  filter (liftM2 (&&) (isPrefixOf "temp") (isSuffixOf "input"))
    <$> listDirectory temperaturesDirectory

getCPUTemperatures :: IO [(String, Double)]
getCPUTemperatures = do
  dir <- getCPUTemperatureDirectory
  let mkPair filename = (filename,) <$> readCPUTempFile (dir </> filename)
  getAllTemperatureFiles dir >>= mapM mkPair
