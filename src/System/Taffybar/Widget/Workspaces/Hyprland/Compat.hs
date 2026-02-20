{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

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
  ( HyprlandWorkspacesConfig (..),
    defaultHyprlandWorkspacesConfig,
    hyprlandWorkspacesNew,
    hyprlandWorkspacesCommonConfig,
    modifyCommonHyprlandWorkspacesConfig,
    applyCommonHyprlandWorkspacesConfig,
    toHyprlandWorkspacesConfig,
    fromHyprlandWorkspacesConfig,
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
    FlatHyprlandWorkspacesConfig,
    defaultFlatHyprlandWorkspacesConfig,
    fromFlatHyprlandWorkspacesConfig,
    toFlatHyprlandWorkspacesConfig,
  )
where

import Control.Monad.Trans.Reader (ReaderT)
import Data.Default (Default (..))
import Data.Int (Int32)
import qualified GI.Gtk as Gtk
import System.Taffybar.Context (Context, TaffyIO)
import System.Taffybar.Widget.Workspaces.Config
  ( WorkspaceWidgetCommonConfig (WorkspaceWidgetCommonConfig),
  )
import qualified System.Taffybar.Widget.Workspaces.Config as WorkspaceConfig
import qualified System.Taffybar.Widget.Workspaces.Hyprland as Hyprland

data HyprlandWorkspacesConfig
  = HyprlandWorkspacesConfig
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

{-# DEPRECATED getWorkspaces "Legacy flat config field. Use `System.Taffybar.Widget.Workspaces.Hyprland.getWorkspaces` on the canonical nested config type instead." #-}

{-# DEPRECATED switchToWorkspace "Legacy flat config field. Use `System.Taffybar.Widget.Workspaces.Hyprland.switchToWorkspace` on the canonical nested config type instead." #-}

{-# DEPRECATED updateIntervalSeconds "Legacy flat config field. Use `System.Taffybar.Widget.Workspaces.Hyprland.updateIntervalSeconds` on the canonical nested config type instead." #-}

{-# DEPRECATED widgetBuilder "Legacy flat config field. Use the nested `workspacesConfig` field in `System.Taffybar.Widget.Workspaces.Hyprland.HyprlandWorkspacesConfig` instead." #-}

{-# DEPRECATED widgetGap "Legacy flat config field. Use the nested `workspacesConfig` field in `System.Taffybar.Widget.Workspaces.Hyprland.HyprlandWorkspacesConfig` instead." #-}

{-# DEPRECATED maxIcons "Legacy flat config field. Use the nested `workspacesConfig` field in `System.Taffybar.Widget.Workspaces.Hyprland.HyprlandWorkspacesConfig` instead." #-}

{-# DEPRECATED minIcons "Legacy flat config field. Use the nested `workspacesConfig` field in `System.Taffybar.Widget.Workspaces.Hyprland.HyprlandWorkspacesConfig` instead." #-}

{-# DEPRECATED iconSize "Legacy flat config field. Use `System.Taffybar.Widget.Workspaces.Hyprland.iconSize` on the canonical nested config type instead." #-}

{-# DEPRECATED getWindowIconPixbuf "Legacy flat config field. Use the nested `workspacesConfig` field in `System.Taffybar.Widget.Workspaces.Hyprland.HyprlandWorkspacesConfig` instead." #-}

{-# DEPRECATED labelSetter "Legacy flat config field. Use the nested `workspacesConfig` field in `System.Taffybar.Widget.Workspaces.Hyprland.HyprlandWorkspacesConfig` instead." #-}

{-# DEPRECATED showWorkspaceFn "Legacy flat config field. Use the nested `workspacesConfig` field in `System.Taffybar.Widget.Workspaces.Hyprland.HyprlandWorkspacesConfig` instead." #-}

{-# DEPRECATED iconSort "Legacy flat config field. Use the nested `workspacesConfig` field in `System.Taffybar.Widget.Workspaces.Hyprland.HyprlandWorkspacesConfig` instead." #-}

{-# DEPRECATED urgentWorkspaceState "Legacy flat config field. Use the nested `workspacesConfig` field in `System.Taffybar.Widget.Workspaces.Hyprland.HyprlandWorkspacesConfig` instead." #-}

toHyprlandWorkspacesConfig ::
  HyprlandWorkspacesConfig -> Hyprland.HyprlandWorkspacesConfig
toHyprlandWorkspacesConfig flat =
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

fromHyprlandWorkspacesConfig ::
  Hyprland.HyprlandWorkspacesConfig -> HyprlandWorkspacesConfig
fromHyprlandWorkspacesConfig cfg =
  let common = Hyprland.workspacesConfig cfg
   in HyprlandWorkspacesConfig
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

defaultHyprlandWorkspacesConfig :: HyprlandWorkspacesConfig
defaultHyprlandWorkspacesConfig =
  fromHyprlandWorkspacesConfig Hyprland.defaultHyprlandWorkspacesConfig

hyprlandWorkspacesNew :: HyprlandWorkspacesConfig -> TaffyIO Gtk.Widget
hyprlandWorkspacesNew =
  Hyprland.hyprlandWorkspacesNew . toHyprlandWorkspacesConfig

hyprlandWorkspacesCommonConfig ::
  HyprlandWorkspacesConfig ->
  WorkspaceWidgetCommonConfig (ReaderT Context IO) Hyprland.HyprlandWorkspace Hyprland.HyprlandWindow Hyprland.HyprlandWWC
hyprlandWorkspacesCommonConfig =
  Hyprland.workspacesConfig . toHyprlandWorkspacesConfig

-- | Modify the nested common config inside a legacy flat Hyprland workspaces
-- config.
--
-- Prefer this over defining @common@ in terms of a recursively-defined @cfg@,
-- which can accidentally create a black-hole and hang at runtime.
modifyCommonHyprlandWorkspacesConfig ::
  ( WorkspaceWidgetCommonConfig (ReaderT Context IO) Hyprland.HyprlandWorkspace Hyprland.HyprlandWindow Hyprland.HyprlandWWC ->
    WorkspaceWidgetCommonConfig (ReaderT Context IO) Hyprland.HyprlandWorkspace Hyprland.HyprlandWindow Hyprland.HyprlandWWC
  ) ->
  HyprlandWorkspacesConfig ->
  HyprlandWorkspacesConfig
modifyCommonHyprlandWorkspacesConfig f cfg =
  applyCommonHyprlandWorkspacesConfig (f (hyprlandWorkspacesCommonConfig cfg)) cfg

applyCommonHyprlandWorkspacesConfig ::
  WorkspaceWidgetCommonConfig (ReaderT Context IO) Hyprland.HyprlandWorkspace Hyprland.HyprlandWindow Hyprland.HyprlandWWC ->
  HyprlandWorkspacesConfig ->
  HyprlandWorkspacesConfig
applyCommonHyprlandWorkspacesConfig common cfg =
  fromHyprlandWorkspacesConfig $
    Hyprland.applyCommonHyprlandWorkspacesConfig common (toHyprlandWorkspacesConfig cfg)

refreshWorkspaces ::
  HyprlandWorkspacesConfig -> Gtk.Box -> ReaderT Context IO ()
refreshWorkspaces cfg =
  Hyprland.refreshWorkspaces (toHyprlandWorkspacesConfig cfg)

applyUrgentState ::
  HyprlandWorkspacesConfig ->
  Hyprland.HyprlandWorkspace ->
  Hyprland.HyprlandWorkspace
applyUrgentState cfg =
  Hyprland.applyUrgentState (toHyprlandWorkspacesConfig cfg)

hyprlandBuildLabelController ::
  HyprlandWorkspacesConfig -> Hyprland.HyprlandControllerConstructor
hyprlandBuildLabelController =
  Hyprland.hyprlandBuildLabelController . toHyprlandWorkspacesConfig

hyprlandBuildIconController ::
  HyprlandWorkspacesConfig -> Hyprland.HyprlandControllerConstructor
hyprlandBuildIconController =
  Hyprland.hyprlandBuildIconController . toHyprlandWorkspacesConfig

hyprlandBuildContentsController ::
  [Hyprland.HyprlandControllerConstructor] -> Hyprland.HyprlandControllerConstructor
hyprlandBuildContentsController =
  Hyprland.hyprlandBuildContentsController

hyprlandBuildLabelOverlayController ::
  HyprlandWorkspacesConfig ->
  Hyprland.HyprlandControllerConstructor
hyprlandBuildLabelOverlayController =
  Hyprland.hyprlandBuildLabelOverlayController . toHyprlandWorkspacesConfig

hyprlandBuildCustomOverlayController ::
  (Gtk.Widget -> Gtk.Widget -> TaffyIO Gtk.Widget) ->
  HyprlandWorkspacesConfig ->
  Hyprland.HyprlandControllerConstructor
hyprlandBuildCustomOverlayController overlay cfg =
  Hyprland.hyprlandBuildCustomOverlayController
    overlay
    (toHyprlandWorkspacesConfig cfg)

hyprlandBuildButtonController ::
  HyprlandWorkspacesConfig ->
  Hyprland.HyprlandParentControllerConstructor
hyprlandBuildButtonController cfg =
  Hyprland.hyprlandBuildButtonController (toHyprlandWorkspacesConfig cfg)

defaultHyprlandWidgetBuilder ::
  HyprlandWorkspacesConfig -> Hyprland.HyprlandControllerConstructor
defaultHyprlandWidgetBuilder =
  Hyprland.defaultHyprlandWidgetBuilder . toHyprlandWorkspacesConfig

buildIconWidget ::
  Bool ->
  HyprlandWorkspacesConfig ->
  ReaderT Context IO Hyprland.HyprlandIconWidget
buildIconWidget transparentOnNone cfg =
  Hyprland.buildIconWidget transparentOnNone (toHyprlandWorkspacesConfig cfg)

type FlatHyprlandWorkspacesConfig = HyprlandWorkspacesConfig

defaultFlatHyprlandWorkspacesConfig :: FlatHyprlandWorkspacesConfig
defaultFlatHyprlandWorkspacesConfig = defaultHyprlandWorkspacesConfig

fromFlatHyprlandWorkspacesConfig ::
  FlatHyprlandWorkspacesConfig -> Hyprland.HyprlandWorkspacesConfig
fromFlatHyprlandWorkspacesConfig = toHyprlandWorkspacesConfig

toFlatHyprlandWorkspacesConfig ::
  Hyprland.HyprlandWorkspacesConfig -> FlatHyprlandWorkspacesConfig
toFlatHyprlandWorkspacesConfig = fromHyprlandWorkspacesConfig

instance Default HyprlandWorkspacesConfig where
  def = defaultHyprlandWorkspacesConfig
