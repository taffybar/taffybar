{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module DhallConfig
  ( runDhallConfigFromFile,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.Default (def)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Dhall
import GHC.Generics (Generic)
import qualified GI.Gtk as Gtk
import Numeric.Natural (Natural)
import System.Taffybar (startTaffybar)
import System.Taffybar.Context (TaffyIO, TaffybarConfig)
import System.Taffybar.DBus (withLogServer, withToggleServer)
import System.Taffybar.Hooks (withBatteryRefresh, withLogLevels)
import qualified System.Taffybar.Information.Wlsunset as WlsunsetInfo
import qualified System.Taffybar.Information.Workspaces.Model as WorkspaceModel
import System.Taffybar.SimpleConfig
import System.Taffybar.Widget
import qualified System.Taffybar.Widget.Backlight as Backlight
import qualified System.Taffybar.Widget.DiskUsage as DiskUsage
import qualified System.Taffybar.Widget.NetworkManager as NetworkManager
import qualified System.Taffybar.Widget.PulseAudio as PulseAudio
import qualified System.Taffybar.Widget.ScreenLock as ScreenLock
import qualified System.Taffybar.Widget.Windows as Windows
import qualified System.Taffybar.Widget.Wlsunset as Wlsunset

runDhallConfigFromFile :: FilePath -> IO ()
runDhallConfigFromFile path = do
  cfg <- Dhall.inputFile Dhall.auto path
  let simpleConfig = toSimpleConfig cfg
      taffyConfig = applyHookSpec (dhallHooks cfg) (toTaffybarConfig simpleConfig)
  startTaffybar taffyConfig

data DhallTaffybarConfig = DhallTaffybarConfig
  { dhallMonitors :: MonitorSpec,
    dhallBar :: BarSpec,
    dhallHooks :: HookSpec,
    dhallWidgets :: WidgetSections
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Dhall.FromDhall)

data MonitorSpec
  = AllMonitors
  | PrimaryMonitor
  | MonitorList [Natural]
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Dhall.FromDhall)

data PositionSpec = TopBar | BottomBar
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Dhall.FromDhall)

data BarHeightSpec
  = ScreenRatioHeight Double
  | ExactPixelHeight Natural
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Dhall.FromDhall)

data BarSpec = BarSpec
  { barPositionSpec :: PositionSpec,
    barHeightSpec :: BarHeightSpec,
    barPaddingPx :: Natural,
    barWidgetSpacing :: Natural,
    barCssPaths :: [T.Text]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Dhall.FromDhall)

data HookSpec = HookSpec
  { hookLogServer :: Bool,
    hookToggleServer :: Bool,
    hookLogLevels :: Bool,
    hookBatteryRefresh :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Dhall.FromDhall)

data WidgetSections = WidgetSections
  { widgetStart :: [WidgetSpec],
    widgetCenter :: [WidgetSpec],
    widgetEnd :: [WidgetSpec]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Dhall.FromDhall)

data WidgetSpec
  = Clock (Maybe ClockSpec)
  | PulseAudio (Maybe PulseAudioSpec)
  | Backlight (Maybe BacklightSpec)
  | DiskUsage (Maybe DiskUsageSpec)
  | Windows (Maybe WindowsSpec)
  | WorkspacesEWMH (Maybe EWMHWorkspacesSpec)
  | WorkspacesHyprland (Maybe HyprlandWorkspacesSpec)
  | ScreenLock (Maybe ScreenLockSpec)
  | Wlsunset (Maybe WlsunsetSpec)
  | Layout
  | SNITray
  | SNITrayWithWatcher
  | MPRIS2
  | Battery
  | NetworkManagerWifi
  | WithClass WithClassSpec
  | WithContentsBox WidgetSpec
  | Box BoxSpec
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Dhall.FromDhall)

data WithClassSpec = WithClassSpec
  { withClassName :: T.Text,
    withClassWidget :: WidgetSpec
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Dhall.FromDhall)

data BoxOrientation = Horizontal | Vertical
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Dhall.FromDhall)

data BoxSpec = BoxSpec
  { boxOrientation :: BoxOrientation,
    boxSpacing :: Natural,
    boxChildren :: [WidgetSpec]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Dhall.FromDhall)

data ClockUpdateSpec
  = ConstantUpdateInterval Double
  | RoundedTarget RoundedTargetSpec
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Dhall.FromDhall)

data RoundedTargetSpec = RoundedTargetSpec
  { roundedSeconds :: Natural,
    roundedOffset :: Double
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Dhall.FromDhall)

data ClockSpec = ClockSpec
  { clockFormat :: T.Text,
    clockUpdateSpec :: ClockUpdateSpec
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Dhall.FromDhall)

data PulseAudioSpec = PulseAudioSpec
  { pulseSink :: T.Text,
    pulseFormat :: T.Text,
    pulseMuteFormat :: T.Text,
    pulseUnknownFormat :: T.Text,
    pulseTooltipFormat :: Maybe T.Text,
    pulseScrollStepPercent :: Maybe Natural,
    pulseToggleMuteOnClick :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Dhall.FromDhall)

data BacklightSpec = BacklightSpec
  { backlightPollInterval :: Double,
    backlightDeviceName :: Maybe T.Text,
    backlightFormatSpec :: T.Text,
    backlightUnknownFormatSpec :: T.Text,
    backlightTooltipFormatSpec :: Maybe T.Text,
    backlightScrollStepPercentSpec :: Maybe Natural,
    backlightBrightnessctlPathSpec :: T.Text,
    backlightIconSpec :: T.Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Dhall.FromDhall)

data DiskUsageSpec = DiskUsageSpec
  { diskPath :: T.Text,
    diskPollInterval :: Double,
    diskFormat :: T.Text,
    diskTooltipFormat :: Maybe T.Text,
    diskIcon :: T.Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Dhall.FromDhall)

data WindowsSpec = WindowsSpec
  { windowsMenuLabelLength :: Natural,
    windowsActiveLabelLength :: Natural,
    windowsShowIcon :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Dhall.FromDhall)

data EWMHWorkspacesSpec = EWMHWorkspacesSpec
  { ewmhMinIcons :: Natural,
    ewmhMaxIcons :: Maybe Natural,
    ewmhWidgetGap :: Natural,
    ewmhShowEmpty :: Bool,
    ewmhUrgentWorkspaceState :: Bool,
    ewmhBorderWidth :: Natural,
    ewmhUpdateRateLimitMicroseconds :: Natural
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Dhall.FromDhall)

data HyprlandWorkspacesSpec = HyprlandWorkspacesSpec
  { hyprlandMinIcons :: Natural,
    hyprlandMaxIcons :: Maybe Natural,
    hyprlandWidgetGap :: Natural,
    hyprlandShowEmpty :: Bool,
    hyprlandShowSpecial :: Bool,
    hyprlandUrgentWorkspaceState :: Bool,
    hyprlandUpdateIntervalSeconds :: Double,
    hyprlandIconSize :: Natural
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Dhall.FromDhall)

data ScreenLockSpec = ScreenLockSpec
  { screenLockIconSpec :: T.Text,
    screenLockManageIdle :: Bool,
    screenLockManageSleep :: Bool,
    screenLockManageShutdown :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Dhall.FromDhall)

data WlsunsetSpec = WlsunsetSpec
  { wlsunsetCommandSpec :: T.Text,
    wlsunsetHighTempSpec :: Natural,
    wlsunsetLowTempSpec :: Natural,
    wlsunsetPollIntervalSpec :: Natural,
    wlsunsetIconSpec :: T.Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Dhall.FromDhall)

toSimpleConfig :: DhallTaffybarConfig -> SimpleTaffyConfig
toSimpleConfig DhallTaffybarConfig {dhallMonitors, dhallBar, dhallWidgets} =
  defaultSimpleTaffyConfig
    { monitorsAction = monitorSpecAction dhallMonitors,
      barHeight = barHeightFromSpec (barHeightSpec dhallBar),
      barPadding = naturalToInt (barPaddingPx dhallBar),
      barPosition = positionFromSpec (barPositionSpec dhallBar),
      widgetSpacing = naturalToInt (barWidgetSpacing dhallBar),
      cssPaths = map T.unpack (barCssPaths dhallBar),
      startWidgets = map widgetFromSpec (widgetStart dhallWidgets),
      centerWidgets = map widgetFromSpec (widgetCenter dhallWidgets),
      endWidgets = map widgetFromSpec (widgetEnd dhallWidgets)
    }

monitorSpecAction :: MonitorSpec -> TaffyIO [Int]
monitorSpecAction = \case
  AllMonitors -> useAllMonitors
  PrimaryMonitor -> usePrimaryMonitor
  MonitorList monitors -> pure (map naturalToInt monitors)

barHeightFromSpec :: BarHeightSpec -> StrutSize
barHeightFromSpec = \case
  ScreenRatioHeight r -> ScreenRatio (toRational r)
  ExactPixelHeight px -> ExactSize (fromIntegral px)

positionFromSpec :: PositionSpec -> Position
positionFromSpec = \case
  TopBar -> Top
  BottomBar -> Bottom

applyHookSpec :: HookSpec -> TaffybarConfig -> TaffybarConfig
applyHookSpec HookSpec {hookLogServer, hookToggleServer, hookLogLevels, hookBatteryRefresh} =
  applyWhen hookBatteryRefresh withBatteryRefresh
    . applyWhen hookLogLevels withLogLevels
    . applyWhen hookToggleServer withToggleServer
    . applyWhen hookLogServer withLogServer
  where
    applyWhen True f = f
    applyWhen False _ = id

widgetFromSpec :: WidgetSpec -> TaffyIO Gtk.Widget
widgetFromSpec = \case
  Clock maybeClockSpec -> textClockNewWith (clockConfigFromSpec maybeClockSpec)
  PulseAudio maybePulseSpec -> PulseAudio.pulseAudioNewWith (pulseAudioConfigFromSpec maybePulseSpec)
  Backlight maybeBacklightSpec -> Backlight.backlightNewChanWith (backlightConfigFromSpec maybeBacklightSpec)
  DiskUsage maybeDiskSpec -> DiskUsage.diskUsageNewWith (diskUsageConfigFromSpec maybeDiskSpec)
  Windows maybeWindowsSpec -> Windows.windowsNew (windowsConfigFromSpec maybeWindowsSpec)
  WorkspacesEWMH maybeSpec -> workspacesNew (ewmhConfigFromSpec maybeSpec)
  WorkspacesHyprland maybeSpec -> workspacesNew (hyprlandConfigFromSpec maybeSpec)
  ScreenLock maybeSpec -> ScreenLock.screenLockNewWithConfig (screenLockConfigFromSpec maybeSpec)
  Wlsunset maybeSpec -> Wlsunset.wlsunsetNewWithConfig (wlsunsetConfigFromSpec maybeSpec)
  Layout -> layoutNew defaultLayoutConfig
  SNITray -> sniTrayNew
  SNITrayWithWatcher -> sniTrayThatStartsWatcherEvenThoughThisIsABadWayToDoIt
  MPRIS2 -> mpris2New
  Battery -> batteryIconNew
  NetworkManagerWifi -> NetworkManager.networkManagerWifiIconLabelNew
  WithClass WithClassSpec {withClassName, withClassWidget} ->
    widgetFromSpec withClassWidget >>= flip widgetSetClassGI withClassName
  WithContentsBox wrappedWidget ->
    widgetFromSpec wrappedWidget >>= buildContentsBox
  Box BoxSpec {boxOrientation, boxSpacing, boxChildren} -> do
    children <- mapM widgetFromSpec boxChildren
    let orientation = case boxOrientation of
          Horizontal -> Gtk.OrientationHorizontal
          Vertical -> Gtk.OrientationVertical
    liftIO $ do
      box <- Gtk.boxNew orientation (fromIntegral boxSpacing)
      mapM_ (\child -> Gtk.boxPackStart box child False False 0) children
      Gtk.widgetShowAll box
      Gtk.toWidget box

clockConfigFromSpec :: Maybe ClockSpec -> ClockConfig
clockConfigFromSpec Nothing = defaultClockConfig
clockConfigFromSpec (Just ClockSpec {clockFormat, clockUpdateSpec}) =
  defaultClockConfig
    { clockFormatString = T.unpack clockFormat,
      clockUpdateStrategy =
        case clockUpdateSpec of
          ConstantUpdateInterval interval -> ConstantInterval interval
          RoundedTarget RoundedTargetSpec {roundedSeconds, roundedOffset} ->
            RoundedTargetInterval (naturalToInt roundedSeconds) roundedOffset
    }

pulseAudioConfigFromSpec :: Maybe PulseAudioSpec -> PulseAudio.PulseAudioWidgetConfig
pulseAudioConfigFromSpec Nothing = PulseAudio.defaultPulseAudioWidgetConfig
pulseAudioConfigFromSpec
  ( Just
      PulseAudioSpec
        { pulseSink,
          pulseFormat,
          pulseMuteFormat,
          pulseUnknownFormat,
          pulseTooltipFormat,
          pulseScrollStepPercent,
          pulseToggleMuteOnClick
        }
    ) =
    PulseAudio.defaultPulseAudioWidgetConfig
      { PulseAudio.pulseAudioSink = T.unpack pulseSink,
        PulseAudio.pulseAudioFormat = T.unpack pulseFormat,
        PulseAudio.pulseAudioMuteFormat = T.unpack pulseMuteFormat,
        PulseAudio.pulseAudioUnknownFormat = T.unpack pulseUnknownFormat,
        PulseAudio.pulseAudioTooltipFormat = fmap T.unpack pulseTooltipFormat,
        PulseAudio.pulseAudioScrollStepPercent = fmap naturalToInt pulseScrollStepPercent,
        PulseAudio.pulseAudioToggleMuteOnClick = pulseToggleMuteOnClick
      }

backlightConfigFromSpec :: Maybe BacklightSpec -> Backlight.BacklightWidgetConfig
backlightConfigFromSpec Nothing = Backlight.defaultBacklightWidgetConfig
backlightConfigFromSpec
  ( Just
      BacklightSpec
        { backlightPollInterval,
          backlightDeviceName,
          backlightFormatSpec,
          backlightUnknownFormatSpec,
          backlightTooltipFormatSpec,
          backlightScrollStepPercentSpec,
          backlightBrightnessctlPathSpec,
          backlightIconSpec
        }
    ) =
    Backlight.defaultBacklightWidgetConfig
      { Backlight.backlightPollingInterval = backlightPollInterval,
        Backlight.backlightDevice = fmap T.unpack backlightDeviceName,
        Backlight.backlightFormat = T.unpack backlightFormatSpec,
        Backlight.backlightUnknownFormat = T.unpack backlightUnknownFormatSpec,
        Backlight.backlightTooltipFormat = fmap T.unpack backlightTooltipFormatSpec,
        Backlight.backlightScrollStepPercent = fmap naturalToInt backlightScrollStepPercentSpec,
        Backlight.backlightBrightnessctlPath = T.unpack backlightBrightnessctlPathSpec,
        Backlight.backlightIcon = backlightIconSpec
      }

diskUsageConfigFromSpec :: Maybe DiskUsageSpec -> DiskUsage.DiskUsageWidgetConfig
diskUsageConfigFromSpec Nothing = DiskUsage.defaultDiskUsageWidgetConfig
diskUsageConfigFromSpec
  ( Just
      DiskUsageSpec
        { diskPath,
          diskPollInterval,
          diskFormat,
          diskTooltipFormat,
          diskIcon
        }
    ) =
    DiskUsage.defaultDiskUsageWidgetConfig
      { DiskUsage.diskUsagePath = T.unpack diskPath,
        DiskUsage.diskUsagePollInterval = diskPollInterval,
        DiskUsage.diskUsageFormat = T.unpack diskFormat,
        DiskUsage.diskUsageTooltipFormat = fmap T.unpack diskTooltipFormat,
        DiskUsage.diskUsageIcon = diskIcon
      }

windowsConfigFromSpec :: Maybe WindowsSpec -> Windows.WindowsConfig
windowsConfigFromSpec Nothing = Windows.defaultWindowsConfig
windowsConfigFromSpec
  (Just WindowsSpec {windowsMenuLabelLength, windowsActiveLabelLength, windowsShowIcon}) =
    Windows.defaultWindowsConfig
      { Windows.getMenuLabel = Windows.truncatedGetMenuLabel (naturalToInt windowsMenuLabelLength),
        Windows.getActiveLabel = Windows.truncatedGetActiveLabel (naturalToInt windowsActiveLabelLength),
        Windows.getActiveWindowIconPixbuf =
          if windowsShowIcon
            then Windows.getActiveWindowIconPixbuf Windows.defaultWindowsConfig
            else Nothing
      }

defaultEWMHWorkspacesSpec :: EWMHWorkspacesSpec
defaultEWMHWorkspacesSpec =
  EWMHWorkspacesSpec
    { ewmhMinIcons = 0,
      ewmhMaxIcons = Nothing,
      ewmhWidgetGap = 0,
      ewmhShowEmpty = True,
      ewmhUrgentWorkspaceState = False,
      ewmhBorderWidth = 2,
      ewmhUpdateRateLimitMicroseconds = 100000
    }

ewmhConfigFromSpec :: Maybe EWMHWorkspacesSpec -> WorkspacesConfig
ewmhConfigFromSpec maybeSpec =
  let spec = fromMaybe defaultEWMHWorkspacesSpec maybeSpec
      base = defaultEWMHWorkspacesConfig
   in base
        { minIcons = naturalToInt (ewmhMinIcons spec),
          maxIcons = fmap naturalToInt (ewmhMaxIcons spec),
          widgetGap = naturalToInt (ewmhWidgetGap spec),
          showWorkspaceFn = if ewmhShowEmpty spec then const True else hideEmpty,
          urgentWorkspaceState = ewmhUrgentWorkspaceState spec
        }

defaultHyprlandWorkspacesSpec :: HyprlandWorkspacesSpec
defaultHyprlandWorkspacesSpec =
  HyprlandWorkspacesSpec
    { hyprlandMinIcons = 0,
      hyprlandMaxIcons = Nothing,
      hyprlandWidgetGap = 0,
      hyprlandShowEmpty = False,
      hyprlandShowSpecial = False,
      hyprlandUrgentWorkspaceState = False,
      hyprlandUpdateIntervalSeconds = 1.0,
      hyprlandIconSize = 16
    }

hyprlandConfigFromSpec :: Maybe HyprlandWorkspacesSpec -> WorkspacesConfig
hyprlandConfigFromSpec maybeSpec =
  let spec = fromMaybe defaultHyprlandWorkspacesSpec maybeSpec
      base = defaultWorkspacesConfig
      shouldShowWorkspace ws =
        let isEmpty = WorkspaceModel.workspaceState ws == WorkspaceModel.WorkspaceEmpty
            isSpecial = WorkspaceModel.workspaceIsSpecial ws
            emptyAllowed = hyprlandShowEmpty spec || not isEmpty
            specialAllowed = hyprlandShowSpecial spec || not isSpecial
         in emptyAllowed && specialAllowed
   in base
        { minIcons = naturalToInt (hyprlandMinIcons spec),
          maxIcons = fmap naturalToInt (hyprlandMaxIcons spec),
          widgetGap = naturalToInt (hyprlandWidgetGap spec),
          showWorkspaceFn = shouldShowWorkspace,
          urgentWorkspaceState = hyprlandUrgentWorkspaceState spec,
          iconSize = Just (fromIntegral (hyprlandIconSize spec))
        }

screenLockConfigFromSpec :: Maybe ScreenLockSpec -> ScreenLock.ScreenLockConfig
screenLockConfigFromSpec Nothing = ScreenLock.defaultScreenLockConfig
screenLockConfigFromSpec
  ( Just
      ScreenLockSpec
        { screenLockIconSpec,
          screenLockManageIdle,
          screenLockManageSleep,
          screenLockManageShutdown
        }
    ) =
    let inhibitTypes =
          concat
            [ [InhibitIdle | screenLockManageIdle],
              [InhibitSleep | screenLockManageSleep],
              [InhibitShutdown | screenLockManageShutdown]
            ]
        selectedTypes = if null inhibitTypes then [InhibitIdle] else inhibitTypes
     in ScreenLock.defaultScreenLockConfig
          { ScreenLock.screenLockIcon = screenLockIconSpec,
            ScreenLock.screenLockInhibitTypes = selectedTypes
          }

wlsunsetConfigFromSpec :: Maybe WlsunsetSpec -> Wlsunset.WlsunsetWidgetConfig
wlsunsetConfigFromSpec Nothing = Wlsunset.defaultWlsunsetWidgetConfig
wlsunsetConfigFromSpec
  ( Just
      WlsunsetSpec
        { wlsunsetCommandSpec,
          wlsunsetHighTempSpec,
          wlsunsetLowTempSpec,
          wlsunsetPollIntervalSpec,
          wlsunsetIconSpec
        }
    ) =
    let infoConfig =
          (def :: WlsunsetInfo.WlsunsetConfig)
            { WlsunsetInfo.wlsunsetCommand = T.unpack wlsunsetCommandSpec,
              WlsunsetInfo.wlsunsetHighTemp = naturalToInt wlsunsetHighTempSpec,
              WlsunsetInfo.wlsunsetLowTemp = naturalToInt wlsunsetLowTempSpec,
              WlsunsetInfo.wlsunsetPollIntervalSec = naturalToInt wlsunsetPollIntervalSpec
            }
     in Wlsunset.defaultWlsunsetWidgetConfig
          { Wlsunset.wlsunsetWidgetInfoConfig = infoConfig,
            Wlsunset.wlsunsetWidgetIcon = wlsunsetIconSpec
          }

naturalToInt :: Natural -> Int
naturalToInt = fromIntegral
