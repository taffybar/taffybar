module System.Information.CPU2 (getCPULoad) where

import System.Information.StreamInfo (getLoad, getParsedInfo)

getCPULoad :: String -> IO [Double]
getCPULoad cpu = do
    load <- getLoad 0.05 $ getCPUInfo cpu
    return [load!!0 + load!!1, load!!2]

getCPUInfo :: String -> IO [Integer]
getCPUInfo = getParsedInfo "/proc/stat" parse

parse :: String -> [(String, [Integer])]
parse = map (tuplize . words) . filter (\x -> take 3 x == "cpu") . lines

tuplize :: [String] -> (String, [Integer])
tuplize s = (head s, map read $ tail s)
