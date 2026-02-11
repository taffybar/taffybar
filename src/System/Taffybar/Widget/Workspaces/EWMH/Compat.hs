{-# LANGUAGE StrictData #-}

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      : System.Taffybar.Widget.Workspaces.EWMH.Compat
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
module System.Taffybar.Widget.Workspaces.EWMH.Compat
  ( FlatWorkspacesConfig (..),
    defaultFlatWorkspacesConfig,
    fromFlatWorkspacesConfig,
    toFlatWorkspacesConfig,
  )
where

import Data.Default (Default (..))
import System.Taffybar.Widget.Workspaces.Config
  ( WorkspaceWidgetCommonConfig (WorkspaceWidgetCommonConfig),
  )
import qualified System.Taffybar.Widget.Workspaces.Config as WorkspaceConfig
import qualified System.Taffybar.Widget.Workspaces.EWMH as EWMH

data FlatWorkspacesConfig
  = FlatWorkspacesConfig
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

fromFlatWorkspacesConfig :: FlatWorkspacesConfig -> EWMH.WorkspacesConfig
fromFlatWorkspacesConfig flat =
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

toFlatWorkspacesConfig :: EWMH.WorkspacesConfig -> FlatWorkspacesConfig
toFlatWorkspacesConfig cfg =
  let common = EWMH.workspacesConfig cfg
   in FlatWorkspacesConfig
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

defaultFlatWorkspacesConfig :: FlatWorkspacesConfig
defaultFlatWorkspacesConfig =
  toFlatWorkspacesConfig EWMH.defaultWorkspacesConfig

instance Default FlatWorkspacesConfig where
  def = defaultFlatWorkspacesConfig
