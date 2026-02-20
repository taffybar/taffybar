-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      : System.Taffybar.Widget.HyprlandWorkspaces
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan A. Malison
-- Stability   : unstable
-- Portability : unportable
module System.Taffybar.Widget.HyprlandWorkspaces
  {-# DEPRECATED "Legacy Hyprland workspaces widget API. Use System.Taffybar.Widget.Workspaces.Channel (or System.Taffybar.Widget.ChannelWorkspaces) instead." #-}
  ( module System.Taffybar.Widget.Workspaces.Hyprland,
    HyprlandWorkspacesConfig (..),
    defaultHyprlandWorkspacesConfig,
    hyprlandWorkspacesNew,
    hyprlandWorkspacesCommonConfig,
    applyCommonHyprlandWorkspacesConfig,
    refreshWorkspaces,
    applyUrgentState,
    hyprlandBuildLabelController,
    hyprlandBuildIconController,
    hyprlandBuildContentsController,
    hyprlandBuildLabelOverlayController,
    hyprlandBuildCustomOverlayController,
    hyprlandBuildButtonController,
    defaultHyprlandWidgetBuilder,
    buildIconWidget,
  )
where

import System.Taffybar.Widget.Workspaces.Hyprland hiding
  ( HyprlandWorkspacesConfig (..),
    applyCommonHyprlandWorkspacesConfig,
    applyUrgentState,
    buildIconWidget,
    defaultHyprlandWidgetBuilder,
    defaultHyprlandWorkspacesConfig,
    hyprlandBuildButtonController,
    hyprlandBuildContentsController,
    hyprlandBuildCustomOverlayController,
    hyprlandBuildIconController,
    hyprlandBuildLabelController,
    hyprlandBuildLabelOverlayController,
    hyprlandWorkspacesCommonConfig,
    hyprlandWorkspacesNew,
    refreshWorkspaces,
  )
import System.Taffybar.Widget.Workspaces.Hyprland.Compat
  ( HyprlandWorkspacesConfig (..),
    applyCommonHyprlandWorkspacesConfig,
    applyUrgentState,
    buildIconWidget,
    defaultHyprlandWidgetBuilder,
    defaultHyprlandWorkspacesConfig,
    hyprlandBuildButtonController,
    hyprlandBuildContentsController,
    hyprlandBuildCustomOverlayController,
    hyprlandBuildIconController,
    hyprlandBuildLabelController,
    hyprlandBuildLabelOverlayController,
    hyprlandWorkspacesCommonConfig,
    hyprlandWorkspacesNew,
    refreshWorkspaces,
  )
