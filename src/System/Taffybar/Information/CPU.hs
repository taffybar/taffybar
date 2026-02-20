{-# LANGUAGE NamedFieldPuns #-}

-- | Read short-window CPU utilization samples from @/proc/stat@.
module System.Taffybar.Information.CPU
  {-# DEPRECATED "Legacy CPU module. Use System.Taffybar.Information.CPU2.getCPULoadChan (preferred) or sampleCPULoad/CPULoad." #-}
  (cpuLoad)
where

import System.Taffybar.Information.CPU2 (CPULoad (..), sampleCPULoad)

{-# DEPRECATED cpuLoad "Legacy API. Use System.Taffybar.Information.CPU2.getCPULoadChan (preferred) or sampleCPULoad." #-}

-- | Return a triple with (user time, system time, total time), sampled from
-- /proc/stat over 50ms.
cpuLoad :: IO (Double, Double, Double)
cpuLoad = do
  CPULoad {cpuUserLoad, cpuSystemLoad, cpuTotalLoad} <- sampleCPULoad 0.05 "cpu"
  return (cpuUserLoad, cpuSystemLoad, cpuTotalLoad)
