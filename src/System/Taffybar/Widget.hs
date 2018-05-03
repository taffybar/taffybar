module System.Taffybar.Widget
  ( module System.Taffybar.Widget.Util
  -- | * "System.Taffybar.Widget.Battery"
  , batteryBarNew
  , batteryBarNewWithFormat
  , textBatteryNew
  , defaultBatteryConfig

  -- | * "System.Taffybar.Widget.CPUMonitor"
  , cpuMonitorNew

  -- | * "System.Taffybar.Widget.CommandRunner"
  , commandRunnerNew

  -- * "System.Taffybar.Widget.Decorators"
  , module System.Taffybar.Widget.Decorators

  -- | * "System.Taffybar.Widget.DiskIOMonitor"
  , dioMonitorNew

  -- | * "System.Taffybar.Widget.FSMonitor"
  , fsMonitorNew

  -- | * "System.Taffybar.Widget.FreedesktopNotifications"
  , Notification(..)
  , NotificationConfig(..)
  , defaultNotificationConfig
  , notifyAreaNew

  -- | * "System.Taffybar.Widget.Layout"
  , LayoutConfig(..)
  , defaultLayoutConfig
  , layoutNew

  -- | * "System.Taffybar.Widget.MPRIS"
  , TrackInfo (..)
  , MPRISConfig (..)
  , defaultMPRISConfig
  , mprisNew

  -- | * "System.Taffybar.Widget.MPRIS2"
  , mpris2New

  -- | * "System.Taffybar.Widget.NetMonitor"
  , defaultNetFormat
  , netMonitorMultiNew
  , netMonitorMultiNewWith
  , netMonitorNew
  , netMonitorNewWith

  -- | * "System.Taffybar.Widget.SNITray"
  , sniTrayNew

  -- | * "System.Taffybar.Widget.SimpleClock"
  , textClockNew
  , textClockNewWith
  , defaultClockConfig
  , ClockConfig(..)

  -- | * "System.Taffybar.Widget.Volume"
  , volumeTextNew
  , volumeControlNew

  -- | * "System.Taffybar.Widget.Weather"
  , WeatherConfig(..)
  , WeatherInfo(..)
  , WeatherFormatter(WeatherFormatter)
  , weatherNew
  , weatherCustomNew
  , defaultWeatherConfig

  -- | * "System.Taffybar.Widget.Windows"
  , windowsNew
  , WindowsConfig(..)
  , defaultWindowsConfig
  , truncatedGetActiveLabel
  , truncatedGetMenuLabel

  -- | * "System.Taffybar.Widget.Workspaces"
  , IconInfo(..)
  , Workspace(..)
  , WorkspacesConfig(..)
  , WorkspacesIO
  , hideEmpty
  , workspacesNew

  -- | * "System.Taffybar.Widget.XDGMenu.MenuWidget"
  , module System.Taffybar.Widget.XDGMenu.MenuWidget
  ) where

import System.Taffybar.Widget.Battery
import System.Taffybar.Widget.CPUMonitor
import System.Taffybar.Widget.CommandRunner
import System.Taffybar.Widget.Decorators
import System.Taffybar.Widget.DiskIOMonitor
import System.Taffybar.Widget.FSMonitor
import System.Taffybar.Widget.FreedesktopNotifications
import System.Taffybar.Widget.Layout
import System.Taffybar.Widget.MPRIS
import System.Taffybar.Widget.MPRIS2
import System.Taffybar.Widget.NetMonitor
import System.Taffybar.Widget.SNITray
import System.Taffybar.Widget.SimpleClock
import System.Taffybar.Widget.Util
import System.Taffybar.Widget.Volume
import System.Taffybar.Widget.Weather
import System.Taffybar.Widget.Windows
import System.Taffybar.Widget.Workspaces
import System.Taffybar.Widget.XDGMenu.MenuWidget
