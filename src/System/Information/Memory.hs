module System.Information.Memory (
  MemoryInfo(..),
  parseMeminfo
  ) where

toMB :: [String] -> Double
toMB line = (read $ line !! 1 :: Double) / 1024

data MemoryInfo = MemoryInfo { memoryUsedRatio :: Double
                             , memoryTotal :: Double
                             , memoryFree :: Double
                             , memoryBuffer :: Double
                             , memoryCache :: Double
                             , memoryRest :: Double
                             , memoryUsed :: Double
                             }

parseMeminfo :: IO MemoryInfo
parseMeminfo = do
  s <- readFile "/proc/meminfo"
  let content = map words $ take 4 $ lines s
      [total, free, buffer, cache ] = map toMB content
      rest = free + buffer + cache
      used = total - rest
      usedRatio = used / total
  return MemoryInfo { memoryUsedRatio = usedRatio
                    , memoryTotal = total
                    , memoryFree = free
                    , memoryBuffer = buffer
                    , memoryCache = cache
                    , memoryRest = rest
                    , memoryUsed = used
                    }

