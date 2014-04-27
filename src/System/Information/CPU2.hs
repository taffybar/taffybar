-----------------------------------------------------------------------------
-- |
-- Module      : System.Information.CPU2
-- Copyright   : (c) José A. Romero L.
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : José A. Romero L. <escherdragon@gmail.com>
-- Stability   : unstable
-- Portability : unportable
--
-- Provides information about used CPU times, obtained from parsing the
-- @\/proc\/stat@ file using some of the facilities included in the
-- "System.Information.StreamInfo" module.
--
-----------------------------------------------------------------------------

module System.Information.CPU2 ( getCPULoad, getCPUInfo ) where

import Data.Maybe ( mapMaybe )
import Safe ( atMay, readDef, tailSafe )
import System.Information.StreamInfo ( getLoad, getParsedInfo )

-- | Returns a two-element list containing relative system and user times
-- calculated using two almost simultaneous samples of the @\/proc\/stat@ file
-- for the given core (or all of them aggregated, if \"cpu\" is passed).
getCPULoad :: String -> IO [Double]
getCPULoad cpu = do
  load <- getLoad 0.05 $ getCPUInfo cpu
  case load of
    l0:l1:l2:_ -> return [ l0 + l1, l2 ]
    _ -> return []

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

