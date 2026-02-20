{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      : System.Taffybar.Example
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan A. Malison
-- Stability   : unstable
-- Portability : unportable
--
-- Example taffybar configurations used as reference defaults.
module System.Taffybar.Example where

-- XXX: in an actual taffybar.hs configuration file, you will need the module
-- name to be Main, and you would need to have a main function defined at the
-- top level, e.g.
--
-- > main = dyreTaffybar exampleTaffybarConfig

import Data.Default (def)
import System.Taffybar.Context (TaffybarConfig (..))
import System.Taffybar.Hooks
import System.Taffybar.Information.Memory
import System.Taffybar.SimpleConfig
import System.Taffybar.Widget
import System.Taffybar.Widget.Generic.PollingGraph
import qualified System.Taffybar.Widget.Workspaces.Config as WorkspaceConfig
import qualified System.Taffybar.Widget.Workspaces.EWMH as Workspaces

-- | Fully transparent RGBA color.
transparent :: (Double, Double, Double, Double)
transparent = (0.0, 0.0, 0.0, 0.0)

-- | Accent yellow color (dark).
yellow1 :: (Double, Double, Double, Double)
yellow1 = (0.9453125, 0.63671875, 0.2109375, 1.0)

-- | Accent yellow color (light).
yellow2 :: (Double, Double, Double, Double)
yellow2 = (0.9921875, 0.796875, 0.32421875, 1.0)

-- | Accent green color.
green1 :: (Double, Double, Double, Double)
green1 = (0, 1, 0, 1)

-- | Accent translucent magenta color.
green2 :: (Double, Double, Double, Double)
green2 = (1, 0, 1, 0.5)

-- | Primary blue accent.
taffyBlue :: (Double, Double, Double, Double)
taffyBlue = (0.129, 0.588, 0.953, 1)

-- | Base graph configuration used by example graphs.
myGraphConfig :: GraphConfig
myGraphConfig =
  def
    { graphPadding = 0,
      graphBorderWidth = 0,
      graphWidth = 75,
      graphBackgroundColor = transparent
    }

-- | Network graph configuration for the example bar.
netCfg :: GraphConfig
netCfg =
  myGraphConfig
    { graphDataColors = [yellow1, yellow2],
      graphLabel = Just "net"
    }

-- | Memory graph configuration for the example bar.
memCfg :: GraphConfig
memCfg =
  myGraphConfig
    { graphDataColors = [taffyBlue],
      graphLabel = Just "mem"
    }

-- | CPU graph configuration for the example bar.
cpuCfg :: GraphConfig
cpuCfg =
  myGraphConfig
    { graphDataColors = [green1, green2],
      graphLabel = Just "cpu"
    }

-- | Callback used by the example memory graph.
memCallback :: IO [Double]
memCallback = do
  mi <- parseMeminfo
  return [memoryUsedRatio mi]

-- | Callback used by the example CPU graph.
cpuCallback :: IO [Double]
cpuCallback = do
  (_, systemLoad, totalLoad) <- cpuLoad
  return [totalLoad, systemLoad]

-- | Example X11-oriented taffybar configuration.
exampleTaffybarConfig :: TaffybarConfig
exampleTaffybarConfig =
  let myWorkspacesConfig :: Workspaces.WorkspacesConfig
      myWorkspacesConfig =
        let cfg = def
         in cfg
              { Workspaces.workspacesConfig =
                  (Workspaces.workspacesConfig cfg)
                    { WorkspaceConfig.minIcons = 1,
                      WorkspaceConfig.widgetGap = 0,
                      WorkspaceConfig.showWorkspaceFn = hideEmpty
                    }
              }
      workspaces = Workspaces.workspacesNew myWorkspacesConfig
      cpu = cpuMonitorNew cpuCfg 0.5 "cpu"
      mem = pollingGraphNew memCfg 1 memCallback
      net = networkGraphNew netCfg Nothing
      clock = textClockNewWith def
      layout = layoutNew def
      windowsW = windowsNew def
      -- See https://github.com/taffybar/gtk-sni-tray#statusnotifierwatcher
      -- for a better way to set up the sni tray
      tray = sniTrayThatStartsWatcherEvenThoughThisIsABadWayToDoIt
      myConfig =
        def
          { startWidgets =
              workspaces : map (>>= buildContentsBox) [layout, windowsW],
            endWidgets =
              map
                (>>= buildContentsBox)
                [ batteryIconNew,
                  clock,
                  tray,
                  cpu,
                  mem,
                  net,
                  mpris2New
                ],
            barPosition = Top,
            barPadding = 10,
            barHeight = ExactSize 50,
            widgetSpacing = 0
          }
   in withLogServer $ withToggleServer $ toTaffybarConfig myConfig

-- | Example minimal Wayland-oriented taffybar configuration.
exampleWaylandTaffybarConfig :: TaffybarConfig
exampleWaylandTaffybarConfig =
  let clock = textClockNewWith def
      cpu = cpuMonitorNew cpuCfg 1 "cpu"
      myConfig =
        def
          { startWidgets = [],
            endWidgets = [clock, cpu],
            barPosition = Top,
            barHeight = ExactSize 28,
            barPadding = 0,
            widgetSpacing = 8,
            monitorsAction = usePrimaryMonitor
          }
   in withLogServer $ withToggleServer $ toTaffybarConfig myConfig
