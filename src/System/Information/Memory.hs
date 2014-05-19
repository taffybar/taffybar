module System.Information.Memory (
  MemoryInfo(..),
  parseMeminfo
  ) where

toMB :: String -> Double
toMB size = (read size :: Double) / 1024

data MemoryInfo = MemoryInfo { memoryTotal :: Double
                             , memoryFree :: Double
                             , memoryBuffer :: Double
                             , memoryCache :: Double
                             , memoryRest :: Double      -- free + buffer + cache
                             , memoryUsed :: Double      -- total - rest
                             , memoryUsedRatio :: Double -- used / total
                             }

emptyMemoryInfo :: MemoryInfo
emptyMemoryInfo = MemoryInfo 0 0 0 0 0 0 0

parseLines :: [String] -> MemoryInfo -> MemoryInfo
parseLines (line:rest) memInfo = parseLines rest newMemInfo
  where (label:size:_) = words line
        newMemInfo = case label of
                       "MemTotal:" -> memInfo { memoryTotal = toMB size }
                       "MemFree:"  -> memInfo { memoryFree = toMB size }
                       "Buffers:"  -> memInfo { memoryBuffer = toMB size }
                       "Cached:"   -> memInfo { memoryCache = toMB size }
                       _           -> memInfo
parseLines _ memInfo = memInfo

parseMeminfo :: IO MemoryInfo
parseMeminfo = do
  s <- readFile "/proc/meminfo"
  let m = parseLines (lines s) emptyMemoryInfo
      rest = memoryFree m + memoryBuffer m + memoryCache m
      used = memoryTotal m - rest
      usedRatio = used / memoryTotal m
  return m { memoryRest = rest
           , memoryUsed = used
           , memoryUsedRatio = usedRatio
           }

