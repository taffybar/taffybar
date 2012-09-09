-- | Provides information about read/write operations in a given disk or
-- partition, obtained from parsing the /proc/diskstats file.
module System.Information.DiskIO (getDiskTransfer) where

import System.Information.StreamInfo (getParsedInfo, getTransfer)

-- | Returns a two-element list containing the speed of transfer for read and
-- write operations performed in the given disk/partition (e.g. "sda", "sda1").
getDiskTransfer :: String -> IO [Double]
getDiskTransfer disk = getTransfer 0.05 $ getDiskInfo disk

-- | Returns the list of all the values available in /proc/diskstats for the
-- given disk or partition.
getDiskInfo :: String -> IO [Integer]
getDiskInfo = getParsedInfo "/proc/diskstats" parse

parse :: String -> [(String, [Integer])]
parse = map tuplize . map (drop 2 . words) . lines

tuplize :: [String] -> (String, [Integer])
tuplize s = (head s, map read [s!!3, s!!7])
