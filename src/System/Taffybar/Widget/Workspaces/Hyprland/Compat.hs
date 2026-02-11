{-# LANGUAGE StrictData #-}

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      : System.Taffybar.Widget.Workspaces.Hyprland.Compat
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan A. Malison
-- Stability   : unstable
-- Portability : unportable
--
-- Flat legacy compatibility config for Hyprland workspaces.
--
-- This module is intentionally not re-exported by the umbrella widget
-- modules so that consumers opt in explicitly.
module System.Taffybar.Widget.Workspaces.Hyprland.Compat
  ( FlatHyprlandWorkspacesConfig (..),
    defaultFlatHyprlandWorkspacesConfig,
    fromFlatHyprlandWorkspacesConfig,
    toFlatHyprlandWorkspacesConfig,
  )
where

import Data.Default (Default (..))
import Data.Int (Int32)
import System.Taffybar.Context (TaffyIO)
import System.Taffybar.Widget.Workspaces.Config
  ( WorkspaceWidgetCommonConfig (WorkspaceWidgetCommonConfig),
  )
import qualified System.Taffybar.Widget.Workspaces.Config as WorkspaceConfig
import qualified System.Taffybar.Widget.Workspaces.Hyprland as Hyprland

data FlatHyprlandWorkspacesConfig
  = FlatHyprlandWorkspacesConfig
  { getWorkspaces :: TaffyIO [Hyprland.HyprlandWorkspace],
    switchToWorkspace :: Hyprland.HyprlandWorkspace -> TaffyIO (),
    updateIntervalSeconds :: Double,
    widgetBuilder :: Hyprland.HyprlandControllerConstructor,
    widgetGap :: Int,
    maxIcons :: Maybe Int,
    minIcons :: Int,
    iconSize :: Int32,
    getWindowIconPixbuf :: Hyprland.HyprlandWindowIconPixbufGetter,
    labelSetter :: Hyprland.HyprlandWorkspace -> TaffyIO String,
    showWorkspaceFn :: Hyprland.HyprlandWorkspace -> Bool,
    iconSort :: [Hyprland.HyprlandWindow] -> TaffyIO [Hyprland.HyprlandWindow],
    urgentWorkspaceState :: Bool
  }

fromFlatHyprlandWorkspacesConfig ::
  FlatHyprlandWorkspacesConfig -> Hyprland.HyprlandWorkspacesConfig
fromFlatHyprlandWorkspacesConfig flat =
  Hyprland.HyprlandWorkspacesConfig
    { Hyprland.getWorkspaces = getWorkspaces flat,
      Hyprland.switchToWorkspace = switchToWorkspace flat,
      Hyprland.updateIntervalSeconds = updateIntervalSeconds flat,
      Hyprland.iconSize = iconSize flat,
      Hyprland.workspacesConfig =
        WorkspaceWidgetCommonConfig
          { WorkspaceConfig.widgetBuilder = widgetBuilder flat,
            WorkspaceConfig.widgetGap = widgetGap flat,
            WorkspaceConfig.maxIcons = maxIcons flat,
            WorkspaceConfig.minIcons = minIcons flat,
            WorkspaceConfig.getWindowIconPixbuf = getWindowIconPixbuf flat,
            WorkspaceConfig.labelSetter = labelSetter flat,
            WorkspaceConfig.showWorkspaceFn = showWorkspaceFn flat,
            WorkspaceConfig.iconSort = iconSort flat,
            WorkspaceConfig.urgentWorkspaceState = urgentWorkspaceState flat
          }
    }

toFlatHyprlandWorkspacesConfig ::
  Hyprland.HyprlandWorkspacesConfig -> FlatHyprlandWorkspacesConfig
toFlatHyprlandWorkspacesConfig cfg =
  let common = Hyprland.workspacesConfig cfg
   in FlatHyprlandWorkspacesConfig
        { getWorkspaces = Hyprland.getWorkspaces cfg,
          switchToWorkspace = Hyprland.switchToWorkspace cfg,
          updateIntervalSeconds = Hyprland.updateIntervalSeconds cfg,
          widgetBuilder = WorkspaceConfig.widgetBuilder common,
          widgetGap = WorkspaceConfig.widgetGap common,
          maxIcons = WorkspaceConfig.maxIcons common,
          minIcons = WorkspaceConfig.minIcons common,
          iconSize = Hyprland.iconSize cfg,
          getWindowIconPixbuf = WorkspaceConfig.getWindowIconPixbuf common,
          labelSetter = WorkspaceConfig.labelSetter common,
          showWorkspaceFn = WorkspaceConfig.showWorkspaceFn common,
          iconSort = WorkspaceConfig.iconSort common,
          urgentWorkspaceState = WorkspaceConfig.urgentWorkspaceState common
        }

defaultFlatHyprlandWorkspacesConfig :: FlatHyprlandWorkspacesConfig
defaultFlatHyprlandWorkspacesConfig =
  toFlatHyprlandWorkspacesConfig Hyprland.defaultHyprlandWorkspacesConfig

instance Default FlatHyprlandWorkspacesConfig where
  def = defaultFlatHyprlandWorkspacesConfig
