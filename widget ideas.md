**Overview**
Ideas pulled from Waybar module docs in `/home/imalison/Projects/waybar/man`, excluding widgets already present in Taffybar (`src/System/Taffybar/Widget`).

**Top 10 (Hyprland-Focused, No Other Compositor Widgets)**
- `pulseaudio` or `wireplumber`: volume and mute indicator (choose backend).
- `backlight`: show current backlight level, optionally with scroll to adjust.
- `hyprland-submap`: active Hyprland submap indicator.
- `hyprland-language`: current Hyprland input language/layout.
- `hyprland-windowcount`: number of windows in the current Hyprland workspace.
- `power-profiles-daemon`: show and cycle active power profile.
- `idle-inhibitor`: toggle presentation/idle inhibition.
- `privacy`: show mic/camera/screen-share capture activity.
- `systemd-failed-units`: count failed units and link to details.
- `bluetooth`: controller state and connected devices (with battery where available).

**Power & Hardware**
- `backlight`: show current backlight level, optionally with scroll to adjust.
- `backlight-slider`: brightness slider widget.
- `temperature`: thermal zone temperature readout with thresholds.
- `power-profiles-daemon`: show and cycle active power profile.
- `upower`: battery plus other UPower devices in tooltip.
- `systemd-failed-units`: count failed units and link to details.
- `idle-inhibitor`: toggle presentation/idle inhibition.
- `inhibitor`: systemd-inhibit lock indicator/toggle.
- `gamemode`: indicator when Feral GameMode is active.
- `gps`: gpsd fix status and coordinates.

**Audio & Media**
- `pulseaudio`: volume and mute indicator.
- `pulseaudio-slider`: volume slider widget.
- `wireplumber`: volume indicator for WirePlumber/PipeWire.
- `jack`: JACK server running/active indicator.
- `sndio`: sndio volume indicator (BSD).
- `cava`: audio visualizer.
- `mpd`: MPD now-playing and playback status.

**Connectivity & Privacy**
- `bluetooth`: controller state and connected devices (with battery where available).
- `privacy`: show mic/camera/screen-share capture activity.

**Input & Language**
- `keyboard-state`: caps/num/scroll lock indicators.
- `hyprland-language`: current Hyprland input language/layout.
- `sway-language`: current Sway keyboard layout.
- `niri-language`: current Niri keyboard layout.
- `hyprland-submap`: active Hyprland submap indicator.
- `sway-mode`: active Sway binding mode indicator.
- `river-mode`: current River key mapping mode.

**Wayland Integration**
- `wlr-taskbar`: taskbar via `wlr-foreign-toplevel-management` (likely redundant if workspaces already cover your needs).
- `ext-workspaces`: external or persistent workspaces for Wayland compositors.
- `hyprland-windowcount`: number of windows in the current Hyprland workspace.

**Custom Extensibility**
- `custom`: JSON-output script widget with click/scroll/tooltip hooks (richer than `CommandRunner`).
- `cffi`: load external GTK widgets from a shared library for advanced custom modules.
