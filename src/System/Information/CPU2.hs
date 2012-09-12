-- | Provides information about used CPU times, obtained from parsing the
-- /proc/stat file.
module System.Information.CPU2 (getCPULoad, getCPUInfo) where

import System.Information.StreamInfo (getLoad, getParsedInfo)

-- | Returns a two-element list containing relative system and user times
-- calculated using two almost simultaneous samples of the /proc/stat file
-- for the given core (or all of them aggregated, if "cpu" is passed).
getCPULoad :: String -> IO [Double]
getCPULoad cpu = do
    load <- getLoad 0.05 $ getCPUInfo cpu
    return [load!!0 + load!!1, load!!2]

-- | Returns a list of 5 to 7 elements containing all the values available for
-- the given core (or all of them aggregated, if "cpu" is passed).
getCPUInfo :: String -> IO [Integer]
getCPUInfo = getParsedInfo "/proc/stat" parse

parse :: String -> [(String, [Integer])]
parse = map (tuplize . words) . filter (\x -> take 3 x == "cpu") . lines

tuplize :: [String] -> (String, [Integer])
tuplize s = (head s, map read $ tail s)
