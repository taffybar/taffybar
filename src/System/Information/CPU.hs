module System.Information.CPU ( cpuLoad ) where

import Control.Concurrent ( threadDelay )
import System.IO ( IOMode(ReadMode), openFile, hGetLine, hClose )

procData :: IO [Double]
procData = do
  h <- openFile "/proc/stat" ReadMode
  firstLine <- hGetLine h
  (length firstLine) `seq` return ()
  hClose h
  return (procParser firstLine)

procParser :: String -> [Double]
procParser = map read . tail . words

truncVal :: Double -> Double
truncVal v
  | isNaN v || v < 0.0 = 0.0
  | otherwise = v

-- | Return a pair with (user time, system time, total time) (read
-- from /proc/stat).  The function waits for 50 ms between samples.
cpuLoad :: IO (Double, Double, Double)
cpuLoad = do
  a <- procData
  threadDelay 50000
  b <- procData
  let dif = zipWith (-) b a
      tot = foldr (+) 0 dif
      pct = map (/ tot) dif
      user = foldr (+) 0 $ take 2 pct
      system = pct !! 2
      t = user + system
  return (truncVal user, truncVal system, truncVal t)
