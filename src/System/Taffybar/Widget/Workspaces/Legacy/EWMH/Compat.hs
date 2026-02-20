{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      : System.Taffybar.Widget.Workspaces.Legacy.EWMH.Compat
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan A. Malison
-- Stability   : unstable
-- Portability : unportable
--
-- Flat legacy compatibility config for EWMH workspaces.
--
-- This module is intentionally not re-exported by the umbrella widget
-- modules so that consumers opt in explicitly.
module System.Taffybar.Widget.Workspaces.Legacy.EWMH.Compat
  ( WorkspacesConfig (..),
    defaultWorkspacesConfig,
    workspacesNew,
    workspacesCommonConfig,
    modifyCommonWorkspacesConfig,
    applyCommonWorkspacesConfig,
    toEWMHWorkspacesConfig,
    fromEWMHWorkspacesConfig,
    FlatWorkspacesConfig,
    defaultFlatWorkspacesConfig,
    fromFlatWorkspacesConfig,
    toFlatWorkspacesConfig,
  )
where

import Control.Monad.Trans.Reader (ReaderT)
import Data.Default (Default (..))
import qualified GI.Gtk as Gtk
import System.Taffybar.Context (TaffyIO)
import System.Taffybar.Widget.Workspaces.Config
  ( WorkspaceWidgetCommonConfig (WorkspaceWidgetCommonConfig),
  )
import qualified System.Taffybar.Widget.Workspaces.Config as WorkspaceConfig
import qualified System.Taffybar.Widget.Workspaces.Legacy.EWMH as EWMH

data WorkspacesConfig
  = WorkspacesConfig
  { widgetBuilder :: EWMH.ControllerConstructor,
    widgetGap :: Int,
    maxIcons :: Maybe Int,
    minIcons :: Int,
    getWindowIconPixbuf :: EWMH.WindowIconPixbufGetter,
    labelSetter :: EWMH.Workspace -> EWMH.WorkspacesIO String,
    showWorkspaceFn :: EWMH.Workspace -> Bool,
    borderWidth :: Int,
    updateEvents :: [String],
    updateRateLimitMicroseconds :: Integer,
    iconSort :: [EWMH.WindowData] -> EWMH.WorkspacesIO [EWMH.WindowData],
    urgentWorkspaceState :: Bool
  }

{-# DEPRECATED widgetBuilder "Legacy flat config field. Use the nested `workspacesConfig` field in `System.Taffybar.Widget.Workspaces.Legacy.EWMH.WorkspacesConfig` instead." #-}

{-# DEPRECATED widgetGap "Legacy flat config field. Use the nested `workspacesConfig` field in `System.Taffybar.Widget.Workspaces.Legacy.EWMH.WorkspacesConfig` instead." #-}

{-# DEPRECATED maxIcons "Legacy flat config field. Use the nested `workspacesConfig` field in `System.Taffybar.Widget.Workspaces.Legacy.EWMH.WorkspacesConfig` instead." #-}

{-# DEPRECATED minIcons "Legacy flat config field. Use the nested `workspacesConfig` field in `System.Taffybar.Widget.Workspaces.Legacy.EWMH.WorkspacesConfig` instead." #-}

{-# DEPRECATED getWindowIconPixbuf "Legacy flat config field. Use the nested `workspacesConfig` field in `System.Taffybar.Widget.Workspaces.Legacy.EWMH.WorkspacesConfig` instead." #-}

{-# DEPRECATED labelSetter "Legacy flat config field. Use the nested `workspacesConfig` field in `System.Taffybar.Widget.Workspaces.Legacy.EWMH.WorkspacesConfig` instead." #-}

{-# DEPRECATED showWorkspaceFn "Legacy flat config field. Use the nested `workspacesConfig` field in `System.Taffybar.Widget.Workspaces.Legacy.EWMH.WorkspacesConfig` instead." #-}

{-# DEPRECATED borderWidth "Legacy flat config field. Use `System.Taffybar.Widget.Workspaces.Legacy.EWMH.borderWidth` on the canonical EWMH config type instead." #-}

{-# DEPRECATED updateEvents "Legacy flat config field. Use `System.Taffybar.Widget.Workspaces.Legacy.EWMH.updateEvents` on the canonical EWMH config type instead." #-}

{-# DEPRECATED updateRateLimitMicroseconds "Legacy flat config field. Use `System.Taffybar.Widget.Workspaces.Legacy.EWMH.updateRateLimitMicroseconds` on the canonical EWMH config type instead." #-}

{-# DEPRECATED iconSort "Legacy flat config field. Use the nested `workspacesConfig` field in `System.Taffybar.Widget.Workspaces.Legacy.EWMH.WorkspacesConfig` instead." #-}

{-# DEPRECATED urgentWorkspaceState "Legacy flat config field. Use the nested `workspacesConfig` field in `System.Taffybar.Widget.Workspaces.Legacy.EWMH.WorkspacesConfig` instead." #-}

toEWMHWorkspacesConfig :: WorkspacesConfig -> EWMH.WorkspacesConfig
toEWMHWorkspacesConfig flat =
  EWMH.WorkspacesConfig
    { EWMH.workspacesConfig =
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
          },
      EWMH.borderWidth = borderWidth flat,
      EWMH.updateEvents = updateEvents flat,
      EWMH.updateRateLimitMicroseconds = updateRateLimitMicroseconds flat
    }

fromEWMHWorkspacesConfig :: EWMH.WorkspacesConfig -> WorkspacesConfig
fromEWMHWorkspacesConfig cfg =
  let common = EWMH.workspacesConfig cfg
   in WorkspacesConfig
        { widgetBuilder = WorkspaceConfig.widgetBuilder common,
          widgetGap = WorkspaceConfig.widgetGap common,
          maxIcons = WorkspaceConfig.maxIcons common,
          minIcons = WorkspaceConfig.minIcons common,
          getWindowIconPixbuf = WorkspaceConfig.getWindowIconPixbuf common,
          labelSetter = WorkspaceConfig.labelSetter common,
          showWorkspaceFn = WorkspaceConfig.showWorkspaceFn common,
          borderWidth = EWMH.borderWidth cfg,
          updateEvents = EWMH.updateEvents cfg,
          updateRateLimitMicroseconds = EWMH.updateRateLimitMicroseconds cfg,
          iconSort = WorkspaceConfig.iconSort common,
          urgentWorkspaceState = WorkspaceConfig.urgentWorkspaceState common
        }

defaultWorkspacesConfig :: WorkspacesConfig
defaultWorkspacesConfig =
  fromEWMHWorkspacesConfig EWMH.defaultWorkspacesConfig

workspacesNew :: WorkspacesConfig -> TaffyIO Gtk.Widget
workspacesNew = EWMH.workspacesNew . toEWMHWorkspacesConfig

workspacesCommonConfig ::
  WorkspacesConfig ->
  WorkspaceWidgetCommonConfig (ReaderT EWMH.WorkspacesContext IO) EWMH.Workspace EWMH.WindowData EWMH.WWC
workspacesCommonConfig =
  EWMH.workspacesConfig . toEWMHWorkspacesConfig

-- | Modify the nested common config inside a legacy flat workspaces config.
--
-- Prefer this over defining @common@ in terms of a recursively-defined @cfg@,
-- which can accidentally create a black-hole and hang at runtime.
modifyCommonWorkspacesConfig ::
  ( WorkspaceWidgetCommonConfig (ReaderT EWMH.WorkspacesContext IO) EWMH.Workspace EWMH.WindowData EWMH.WWC ->
    WorkspaceWidgetCommonConfig (ReaderT EWMH.WorkspacesContext IO) EWMH.Workspace EWMH.WindowData EWMH.WWC
  ) ->
  WorkspacesConfig ->
  WorkspacesConfig
modifyCommonWorkspacesConfig f cfg =
  applyCommonWorkspacesConfig (f (workspacesCommonConfig cfg)) cfg

applyCommonWorkspacesConfig ::
  WorkspaceWidgetCommonConfig (ReaderT EWMH.WorkspacesContext IO) EWMH.Workspace EWMH.WindowData EWMH.WWC ->
  WorkspacesConfig ->
  WorkspacesConfig
applyCommonWorkspacesConfig common cfg =
  fromEWMHWorkspacesConfig $
    EWMH.applyCommonWorkspacesConfig common (toEWMHWorkspacesConfig cfg)

type FlatWorkspacesConfig = WorkspacesConfig

defaultFlatWorkspacesConfig :: FlatWorkspacesConfig
defaultFlatWorkspacesConfig = defaultWorkspacesConfig

fromFlatWorkspacesConfig :: FlatWorkspacesConfig -> EWMH.WorkspacesConfig
fromFlatWorkspacesConfig = toEWMHWorkspacesConfig

toFlatWorkspacesConfig :: EWMH.WorkspacesConfig -> FlatWorkspacesConfig
toFlatWorkspacesConfig = fromEWMHWorkspacesConfig

instance Default WorkspacesConfig where
  def = defaultWorkspacesConfig
