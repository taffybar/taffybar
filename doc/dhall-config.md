# Dhall Config (Experimental)

The `taffybar dhall` subcommand reads `~/.config/taffybar/taffybar.dhall` and decodes it into a data-only configuration.

`taffybar auto` (and plain `taffybar`) prefers `taffybar.hs` first, then `taffybar.dhall`, then built-in example config.

## Top-level shape

The Dhall expression must decode to a record with these fields:

- `dhallMonitors : MonitorSpec`
- `dhallBar : BarSpec`
- `dhallHooks : HookSpec`
- `dhallWidgets : WidgetSections`

## MonitorSpec constructors

- `AllMonitors`
- `PrimaryMonitor`
- `MonitorList [Natural]`

## BarSpec fields

- `barPositionSpec : PositionSpec` (`TopBar` or `BottomBar`)
- `barHeightSpec : BarHeightSpec` (`ScreenRatioHeight Double` or `ExactPixelHeight Natural`)
- `barPaddingPx : Natural`
- `barWidgetSpacing : Natural`
- `barCssPaths : List Text`

## HookSpec fields

- `hookLogServer : Bool`
- `hookToggleServer : Bool`
- `hookLogLevels : Bool`
- `hookBatteryRefresh : Bool`

## Widget sections

`WidgetSections` has:

- `widgetStart : List WidgetSpec`
- `widgetCenter : List WidgetSpec`
- `widgetEnd : List WidgetSpec`

## WidgetSpec constructors

Plain/default constructors:

- `Layout`
- `SNITray`
- `SNITrayWithWatcher`
- `MPRIS2`
- `Battery`
- `NetworkManagerWifi`

Configurable constructors (payload type is `Optional ...`; use `None` for defaults):

- `Clock (Optional ClockSpec)`
- `PulseAudio (Optional PulseAudioSpec)`
- `Backlight (Optional BacklightSpec)`
- `DiskUsage (Optional DiskUsageSpec)`
- `Windows (Optional WindowsSpec)`
- `WorkspacesEWMH (Optional EWMHWorkspacesSpec)`
- `WorkspacesHyprland (Optional HyprlandWorkspacesSpec)`
- `ScreenLock (Optional ScreenLockSpec)`
- `Wlsunset (Optional WlsunsetSpec)`

Wrappers/combinators:

- `WithClass WithClassSpec`
- `WithContentsBox WidgetSpec`
- `Box BoxSpec`

## Wrapper specs

`WithClassSpec`:

- `withClassName : Text`
- `withClassWidget : WidgetSpec`

`BoxSpec`:

- `boxOrientation : BoxOrientation` (`Horizontal` or `Vertical`)
- `boxSpacing : Natural`
- `boxChildren : List WidgetSpec`

## Config record field names

`ClockSpec`

- `clockFormat : Text`
- `clockUpdateSpec : ClockUpdateSpec` (`ConstantUpdateInterval Double` or `RoundedTarget { roundedSeconds : Natural, roundedOffset : Double }`)

`PulseAudioSpec`

- `pulseSink : Text`
- `pulseFormat : Text`
- `pulseMuteFormat : Text`
- `pulseUnknownFormat : Text`
- `pulseTooltipFormat : Optional Text`
- `pulseScrollStepPercent : Optional Natural`
- `pulseToggleMuteOnClick : Bool`

`BacklightSpec`

- `backlightPollInterval : Double`
- `backlightDeviceName : Optional Text`
- `backlightFormatSpec : Text`
- `backlightUnknownFormatSpec : Text`
- `backlightTooltipFormatSpec : Optional Text`
- `backlightScrollStepPercentSpec : Optional Natural`
- `backlightBrightnessctlPathSpec : Text`
- `backlightIconSpec : Text`

`DiskUsageSpec`

- `diskPath : Text`
- `diskPollInterval : Double`
- `diskFormat : Text`
- `diskTooltipFormat : Optional Text`
- `diskIcon : Text`

`WindowsSpec`

- `windowsMenuLabelLength : Natural`
- `windowsActiveLabelLength : Natural`
- `windowsShowIcon : Bool`

`EWMHWorkspacesSpec`

- `ewmhMinIcons : Natural`
- `ewmhMaxIcons : Optional Natural`
- `ewmhWidgetGap : Natural`
- `ewmhShowEmpty : Bool`
- `ewmhUrgentWorkspaceState : Bool`
- `ewmhBorderWidth : Natural`
- `ewmhUpdateRateLimitMicroseconds : Natural`

`HyprlandWorkspacesSpec`

- `hyprlandMinIcons : Natural`
- `hyprlandMaxIcons : Optional Natural`
- `hyprlandWidgetGap : Natural`
- `hyprlandShowEmpty : Bool`
- `hyprlandShowSpecial : Bool`
- `hyprlandUrgentWorkspaceState : Bool`
- `hyprlandUpdateIntervalSeconds : Double`
- `hyprlandIconSize : Natural`

`ScreenLockSpec`

- `screenLockIconSpec : Text`
- `screenLockManageIdle : Bool`
- `screenLockManageSleep : Bool`
- `screenLockManageShutdown : Bool`

`WlsunsetSpec`

- `wlsunsetCommandSpec : Text`
- `wlsunsetHighTempSpec : Natural`
- `wlsunsetLowTempSpec : Natural`
- `wlsunsetPollIntervalSpec : Natural`
- `wlsunsetIconSpec : Text`
