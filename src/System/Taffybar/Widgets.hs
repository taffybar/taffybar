module System.Taffybar.Widgets
  ( module System.Taffybar.Widgets.Util
  -- | * "System.Taffybar.Widgets.Battery"
  , batteryBarNew
  , batteryBarNewWithFormat
  , textBatteryNew
  , defaultBatteryConfig

  -- | * "System.Taffybar.Widgets.CPUMonitor"
  , cpuMonitorNew

  -- | * "System.Taffybar.Widgets.CommandRunner"
  , commandRunnerNew

  -- | * "System.Taffybar.Widgets.DiskIOMonitor"
  , dioMonitorNew

  -- | * "System.Taffybar.Widgets.FSMonitor"
  , fsMonitorNew

  -- | * "System.Taffybar.Widgets.FreedesktopNotifications"
  , Notification(..)
  , NotificationConfig(..)
  , defaultNotificationConfig
  , notifyAreaNew

  -- | * "System.Taffybar.Widgets.Layout"
  , LayoutConfig(..)
  , defaultLayoutConfig
  , layoutNew

  -- | * "System.Taffybar.Widgets.MPRIS"
  , TrackInfo (..)
  , MPRISConfig (..)
  , defaultMPRISConfig
  , mprisNew

  -- | * "System.Taffybar.Widgets.MPRIS2"
  , mpris2New

  -- | * "System.Taffybar.Widgets.NetMonitor"
  , defaultNetFormat
  , netMonitorMultiNew
  , netMonitorMultiNewWith
  , netMonitorNew
  , netMonitorNewWith

  -- | * "System.Taffybar.Widgets.SNITray"
  , sniTrayNew

  -- | * "System.Taffybar.Widgets.SimpleClock"
  , textClockNew
  , textClockNewWith
  , defaultClockConfig
  , ClockConfig(..)

  -- | * "System.Taffybar.Widgets.Volume"
  , volumeTextNew
  , volumeControlNew

  -- | * "System.Taffybar.Widgets.Weather"
  , WeatherConfig(..)
  , WeatherInfo(..)
  , WeatherFormatter(WeatherFormatter)
  , weatherNew
  , weatherCustomNew
  , defaultWeatherConfig

  -- | * "System.Taffybar.Widgets.Windows"
  , windowsNew
  , WindowsConfig(..)
  , defaultWindowsConfig
  , truncatedGetActiveLabel
  , truncatedGetMenuLabel

  -- | * "System.Taffybar.Widgets.Workspaces"
  , IconInfo(..)
  , Workspace(..)
  , WorkspacesConfig(..)
  , WorkspacesIO
  , hideEmpty
  , workspacesNew
  ) where

import System.Taffybar.Widgets.Battery
import System.Taffybar.Widgets.CPUMonitor
import System.Taffybar.Widgets.CommandRunner
import System.Taffybar.Widgets.DiskIOMonitor
import System.Taffybar.Widgets.FSMonitor
import System.Taffybar.Widgets.FreedesktopNotifications
import System.Taffybar.Widgets.Layout
import System.Taffybar.Widgets.MPRIS
import System.Taffybar.Widgets.MPRIS2
import System.Taffybar.Widgets.NetMonitor
import System.Taffybar.Widgets.SNITray
import System.Taffybar.Widgets.SimpleClock
import System.Taffybar.Widgets.Util
import System.Taffybar.Widgets.Volume
import System.Taffybar.Widgets.Weather
import System.Taffybar.Widgets.Windows
import System.Taffybar.Widgets.Workspaces
