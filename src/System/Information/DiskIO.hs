-----------------------------------------------------------------------------
-- |
-- Module      : System.Information.DiskIO
-- Copyright   : (c) José A. Romero L.
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : José A. Romero L. <escherdragon@gmail.com>
-- Stability   : unstable
-- Portability : unportable
--
-- Provides information about read/write operations in a given disk or
-- partition, obtained from parsing the @\/proc\/diskstats@ file with some
-- of the facilities included in the "System.Information.StreamInfo" module.
-----------------------------------------------------------------------------

module System.Information.DiskIO ( getDiskTransfer ) where

import Data.Maybe ( mapMaybe )
import Safe ( atMay, headMay, readDef )
import System.Information.StreamInfo ( getParsedInfo, getTransfer )

-- | Returns a two-element list containing the speed of transfer for read and
-- write operations performed in the given disk\/partition (e.g. \"sda\",
-- \"sda1\").
getDiskTransfer :: String -> IO [Double]
getDiskTransfer disk = getTransfer 0.05 $ getDiskInfo disk

-- | Returns the list of all the values available in @\/proc\/diskstats@
-- for the given disk or partition.
getDiskInfo :: String -> IO [Int]
getDiskInfo = getParsedInfo "/proc/diskstats" parse

parse :: String -> [(String, [Int])]
parse = mapMaybe tuplize . map (drop 2 . words) . lines

tuplize :: [String] -> Maybe (String, [Int])
tuplize s = do
  device <- headMay s
  used <- s `atMay` 3
  capacity <- s `atMay` 7
  return (device, [readDef (-1) used, readDef (-1) capacity])

