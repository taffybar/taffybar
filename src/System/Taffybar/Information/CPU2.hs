{-# LANGUAGE TupleSections #-}
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
--
-----------------------------------------------------------------------------

module System.Taffybar.Information.CPU2 where

import Control.Monad
import Control.Monad.Trans.Reader
import Data.List
import Data.Maybe
import Safe
import System.Directory
import System.FilePath
import System.Taffybar.Information.StreamInfo

-- | Returns a list of 5 to 7 elements containing all the values available for
-- the given core (or all of them aggregated, if "cpu" is passed).
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
getCPULoad :: String -> IO [Double]
getCPULoad cpu = do
  load <- getLoad 0.05 $ getCPUInfo cpu
  case load of
    l0:l1:l2:_ -> return [ l0 + l1, l2 ]
    _ -> return []

-- | Get the directory in which core temperature files are kept.
getCPUTemperatureDirectory :: IO FilePath
getCPUTemperatureDirectory =
  (baseDir </>) . fromMaybe "hwmon0" .
  find (isPrefixOf "hwmon")
  <$> listDirectory baseDir
  where baseDir =
          "/"  </> "sys" </> "bus" </> "platform" </>
          "devices" </> "coretemp.0" </> "hwmon"

readCPUTempFile :: FilePath -> IO Double
readCPUTempFile cpuTempFilePath = (/ 1000) . read <$> readFile cpuTempFilePath

getAllTemperatureFiles :: FilePath -> IO [FilePath]
getAllTemperatureFiles temperaturesDirectory =
  filter (liftM2 (&&) (isPrefixOf "temp") (isSuffixOf "input")) <$>
         listDirectory temperaturesDirectory

getCPUTemperatures :: IO [(String, Double)]
getCPUTemperatures = do
  dir <- getCPUTemperatureDirectory
  let mkPair filename = (filename,) <$> readCPUTempFile (dir </> filename)
  getAllTemperatureFiles dir >>= mapM mkPair
