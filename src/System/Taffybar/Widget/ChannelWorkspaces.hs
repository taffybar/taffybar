-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      : System.Taffybar.Widget.ChannelWorkspaces
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan A. Malison
-- Stability   : unstable
-- Portability : unportable
--
-- Transitional, prefixed exports for the channel-driven Workspaces widget.
--
-- The canonical API lives in "System.Taffybar.Widget.Workspaces.Channel"
-- (with names like 'workspacesNew'). This module provides prefixed aliases so
-- "System.Taffybar.Widget" can expose the new widget without colliding with
-- legacy workspace exports.
module System.Taffybar.Widget.ChannelWorkspaces
  ( ChannelWorkspacesConfig,
    defaultChannelWorkspacesConfig,
    defaultChannelEWMHWorkspacesConfig,
    channelWorkspacesNew,
  )
where

import qualified GI.Gtk as Gtk
import System.Taffybar.Context (TaffyIO)
import qualified System.Taffybar.Widget.Workspaces.Channel as Channel

type ChannelWorkspacesConfig = Channel.WorkspacesConfig

defaultChannelWorkspacesConfig :: ChannelWorkspacesConfig
defaultChannelWorkspacesConfig = Channel.defaultWorkspacesConfig

defaultChannelEWMHWorkspacesConfig :: ChannelWorkspacesConfig
defaultChannelEWMHWorkspacesConfig = Channel.defaultEWMHWorkspacesConfig

channelWorkspacesNew :: ChannelWorkspacesConfig -> TaffyIO Gtk.Widget
channelWorkspacesNew = Channel.workspacesNew
