module System.Information.Network (getNetInfo) where

import System.Information.StreamInfo (getParsedInfo)

getNetInfo :: String -> IO [Integer]
getNetInfo = getParsedInfo "/proc/net/dev" parse

parse :: String -> [(String, [Integer])]
parse = map tuplize . map words . drop 2 . lines

tuplize :: [String] -> (String, [Integer])
tuplize s = (init $ head s, map read [s!!1, s!!out])
    where out = (length s) - 7
