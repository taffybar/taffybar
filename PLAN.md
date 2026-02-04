Wayland Plan (Short)

Goal
- Get core taffybar running on Wayland/Hyprland with layer-shell and minimal widgets (no X11-only widgets).

State
- Wayland backend detection added.
- Bar window setup now uses gtk-layer-shell on Wayland.
- X11 init/event loop guarded.
- `gi-gtk-layer-shell` added to cabal deps.

Next Steps
1. Nix integration
- Ensure `gtk-layer-shell` C library is in the build inputs as needed by `gi-gtk-layer-shell`.
- Add any overrides if nixpkgs doesn’t include GI introspection data for gtk-layer-shell.

2. Runtime validation
- Start taffybar on Hyprland with a minimal config (clock, graph) and verify exclusive zone and positioning.
- If exclusive zone is wrong, adjust margin/exclusive calculations.

3. Cleanup/compat
- Decide how to handle X11-only widgets (error message or disable).
- Add a note in docs about Wayland requirements and limitations.
