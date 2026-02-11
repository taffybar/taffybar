-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      : System.Taffybar.Widget.Workspaces
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan A. Malison
-- Stability   : unstable
-- Portability : unportable
module System.Taffybar.Widget.Workspaces
  ( module System.Taffybar.Widget.Workspaces.EWMH,
    module System.Taffybar.Widget.Workspaces.Shared,
    WorkspacesConfig (..),
    defaultWorkspacesConfig,
    workspacesNew,
    workspacesCommonConfig,
    applyCommonWorkspacesConfig,
  )
where

import System.Taffybar.Widget.Workspaces.EWMH hiding
  ( WorkspacesConfig (..),
    applyCommonWorkspacesConfig,
    defaultWorkspacesConfig,
    workspacesCommonConfig,
    workspacesNew,
  )
import System.Taffybar.Widget.Workspaces.EWMH.Compat
  ( WorkspacesConfig (..),
    applyCommonWorkspacesConfig,
    defaultWorkspacesConfig,
    workspacesCommonConfig,
    workspacesNew,
  )
import System.Taffybar.Widget.Workspaces.Shared
