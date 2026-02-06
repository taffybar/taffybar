{-# LANGUAGE CPP #-}
{-# LANGUAGE DuplicateRecordFields #-}

module System.Taffybar.Widget
  ( module System.Taffybar.Widget.Util
  -- * "System.Taffybar.Widget.PulseAudio"
  , module System.Taffybar.Widget.PulseAudio

  -- * "System.Taffybar.Widget.Backlight"
  , module System.Taffybar.Widget.Backlight

  -- * "System.Taffybar.Widget.Battery"
  , module System.Taffybar.Widget.Battery

  -- * "System.Taffybar.Widget.CPUMonitor"
  , module System.Taffybar.Widget.CPUMonitor

  -- * "System.Taffybar.Widget.CommandRunner"
  , module System.Taffybar.Widget.CommandRunner

#ifdef WIDGET_CRYPTO
  -- * "System.Taffybar.Widget.Crypto"
  , module System.Taffybar.Widget.Crypto
#endif

  -- * "System.Taffybar.Widget.DiskIOMonitor"
  , module System.Taffybar.Widget.DiskIOMonitor

  -- * "System.Taffybar.Widget.FSMonitor"
  , module System.Taffybar.Widget.FSMonitor

  -- * "System.Taffybar.Widget.FreedesktopNotifications"
  , module System.Taffybar.Widget.FreedesktopNotifications

  -- * "System.Taffybar.Widget.Layout"
  , module System.Taffybar.Widget.Layout

  -- * "System.Taffybar.Widget.MPRIS2"
  , module System.Taffybar.Widget.MPRIS2

  -- * "System.Taffybar.Widget.NetworkGraph"
  , module System.Taffybar.Widget.NetworkGraph

  -- * "System.Taffybar.Widget.NetworkManager"
  , module System.Taffybar.Widget.NetworkManager

  -- * "System.Taffybar.Widget.SNITray"
  , module System.Taffybar.Widget.SNITray

  -- * "System.Taffybar.Widget.SimpleClock"
  , module System.Taffybar.Widget.SimpleClock

  -- * "System.Taffybar.Widget.SimpleCommandButton"
  , module System.Taffybar.Widget.SimpleCommandButton

  -- * "System.Taffybar.Widget.Text.CPUMonitor"
  , module System.Taffybar.Widget.Text.CPUMonitor

  -- * "System.Taffybar.Widget.Text.MemoryMonitor"
  , module System.Taffybar.Widget.Text.MemoryMonitor

  -- * "System.Taffybar.Widget.Text.NetworkMonitor"
  , module System.Taffybar.Widget.Text.NetworkMonitor

  -- * "System.Taffybar.Widget.Weather"
  , module System.Taffybar.Widget.Weather

  -- * "System.Taffybar.Widget.Windows"
  , module System.Taffybar.Widget.Windows

  -- * "System.Taffybar.Widget.HyprlandWorkspaces"
  , HyprlandWorkspacesConfig(..)
  , defaultHyprlandWorkspacesConfig
  , hyprlandWorkspacesNew
  , HyprlandWorkspace(..)
  , HyprlandWindow(..)
  , HyprlandWindowIconPixbufGetter
  , defaultHyprlandGetWindowIconPixbuf
  , getHyprlandWorkspaces
  , hyprlandSwitchToWorkspace
  , getActiveWindowAddress
  , runHyprctlJson
  , HyprlandClient(..)
  , windowFromClient

  -- * "System.Taffybar.Widget.Workspaces"
  , module System.Taffybar.Widget.Workspaces

  -- * "System.Taffybar.Widget.XDGMenu.MenuWidget"
  , module System.Taffybar.Widget.XDGMenu.MenuWidget
  ) where

import System.Taffybar.Widget.Battery
import System.Taffybar.Widget.CPUMonitor
import System.Taffybar.Widget.CommandRunner
#ifdef WIDGET_CRYPTO
import System.Taffybar.Widget.Crypto
#endif
import System.Taffybar.Widget.DiskIOMonitor
import System.Taffybar.Widget.FSMonitor
import System.Taffybar.Widget.FreedesktopNotifications
import System.Taffybar.Widget.Layout
import System.Taffybar.Widget.MPRIS2
import System.Taffybar.Widget.NetworkGraph
import System.Taffybar.Widget.NetworkManager
import System.Taffybar.Widget.SNITray
import System.Taffybar.Widget.SimpleClock
import System.Taffybar.Widget.SimpleCommandButton
import System.Taffybar.Widget.Text.CPUMonitor
import System.Taffybar.Widget.Text.MemoryMonitor
import System.Taffybar.Widget.Text.NetworkMonitor
import System.Taffybar.Widget.Util
import System.Taffybar.Widget.PulseAudio
import System.Taffybar.Widget.Backlight
import System.Taffybar.Widget.Weather
import System.Taffybar.Widget.Windows
import System.Taffybar.Widget.HyprlandWorkspaces
  ( HyprlandWorkspacesConfig(..)
  , defaultHyprlandWorkspacesConfig
  , hyprlandWorkspacesNew
  , HyprlandWorkspace(..)
  , HyprlandWindow(..)
  , HyprlandWindowIconPixbufGetter
  , defaultHyprlandGetWindowIconPixbuf
  , getHyprlandWorkspaces
  , hyprlandSwitchToWorkspace
  , getActiveWindowAddress
  , runHyprctlJson
  , HyprlandClient(..)
  , windowFromClient
  )
import System.Taffybar.Widget.Workspaces
import System.Taffybar.Widget.XDGMenu.MenuWidget
