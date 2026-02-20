# Dhall Config Proposal for `taffybar` (Issue #462)

## Context
Issue [#462](https://github.com/taffybar/taffybar/issues/462) asks for a plain text config path so users do not need a local GHC toolchain just to customize Taffybar.

Current startup behavior in `app/Main.hs` is:

1. If `~/.config/taffybar/taffybar.hs` exists, run Dyre (compile Haskell config).
2. Otherwise, run built-in example config.

There is no middle path for data-driven configuration.

## Goals
1. Allow users to configure Taffybar without compiling a Haskell config.
2. Keep existing `taffybar.hs` + Dyre workflow fully supported.
3. Cover a meaningful subset of real-world configs (including most of the structure in `imalison`'s current config).
4. Preserve an escape hatch for advanced behavior that fundamentally needs Haskell.

## Non-goals
1. Full parity with arbitrary Haskell configuration in v1.
2. Serializing every existing widget config type directly as-is.
3. Removing Dyre or `taffybar.hs` support.

## Investigation Findings

### 1. Existing config types are mixed data + functions
Many useful widget config records are data-oriented and Dhall-friendly:
- `PulseAudioWidgetConfig`
- `BacklightWidgetConfig`
- `DiskUsageWidgetConfig`
- `ClockConfig`
- `ScreenLockConfig`
- `WlsunsetWidgetConfig`

But key types include function fields, so they are not directly serializable:
- `LayoutConfig` (`formatLayout :: Text -> TaffyIO Text`)
- `WindowsConfig` (menu label and active label builders)
- `MPRIS2Config` (widget wrapper + update function)
- workspace configs (`EWMH` and `Hyprland`) with many callback fields
- `SimpleTaffyConfig` and `TaffybarConfig` hold widget constructors (`TaffyIO Gtk.Widget`) and hooks

### 2. Direct `dhall-haskell` derivation on current types is not enough
Even with `dhall` + `dhall-haskell`, we cannot derive `FromDhall` for function-valued fields. A direct "just decode existing records" approach only works for a narrow subset.

### 3. User config parity requires a widget DSL, not only record decoding
The referenced local config includes:
- backend and hostname conditional widget sets
- wrapper/composition patterns (boxed widgets, vertical stacks)
- custom callbacks (workspace label/icon behavior, MPRIS label formatting, custom polling rows)

A viable plain-text path needs a small interpreted widget/config AST.

## Options Considered

### Option A: Decode existing config records directly (minimal)
Approach:
- Add `FromDhall` instances for data-only records.
- Expose a few fixed widget constructors.

Pros:
- Smallest initial patch.

Cons:
- Low coverage; cannot express many commonly customized widgets.
- Does not get close to "mostly parity" with advanced configs.

### Option B: Dhall AST + interpreter to runtime config (recommended)
Approach:
- Introduce Dhall-specific config ADTs (pure data).
- Decode Dhall to those ADTs.
- Interpret ADTs into `SimpleTaffyConfig` / `TaffybarConfig` + widget constructors.

Pros:
- Solves plain-text requirement without requiring local GHC.
- Allows structured growth of supported features.
- Keeps hard-to-serialize internals private.

Cons:
- More design and implementation effort.
- Requires maintaining interpreter + schema docs.

### Option C: Dhall generates Haskell, still run Dyre
Approach:
- Dhall expression renders `taffybar.hs`, then Dyre compiles.

Pros:
- Reuses existing Haskell pipeline.

Cons:
- Does not solve core issue (still needs compilation toolchain).

## Recommended Design

### 1. Make startup mode explicit via subcommands
Add subcommands for all current startup behaviors:

- `taffybar auto` (default when no subcommand is given)
- `taffybar dyre`
- `taffybar example`
- `taffybar dhall`

Semantics:

- `dyre`: run current `taffybar.hs` + Dyre flow.
- `example`: run built-in example config only.
- `dhall`: load and run `taffybar.dhall` only.
- `auto` (compatibility mode):
  1. if `taffybar.hs` exists -> `dyre`
  2. else if `taffybar.dhall` exists -> `dhall`
  3. else -> `example`

Rationale:
- Keeps `taffybar.hs` as the primary/first-class path.
- Makes Dhall support explicit and discoverable.
- Preserves existing behavior for users who run `taffybar` with no args.

### 2. Introduce a data-only Dhall schema + interpreter
New modules (proposed):
- `System.Taffybar.Config.Dhall.Types`
- `System.Taffybar.Config.Dhall.Decode`
- `System.Taffybar.Config.Dhall.Interpret`

Core schema shape:
- top-level bar settings (position, height, padding, spacing, css paths)
- monitor policy (`all`, `primary`, explicit list)
- conditional blocks:
  - `whenBackend` (`x11` / `wayland`)
  - `whenHostIn` (hostname allow-list)
- widget spec sum type:
  - builtin widgets (pulse, backlight, disk, clock, tray, etc.)
  - wrappers/combinators:
    - `WithClass`
    - `WithContentsBox`
    - `Box` (`horizontal`/`vertical`, spacing)
    - `Sequence` (list of widgets)
- optional hook toggles:
  - `enableLogServer`
  - `enableToggleServer`
  - `enableLogLevels`

### 3. Keep an explicit Haskell escape hatch
Document that these remain Haskell-only (initially):
- arbitrary callback functions
- deeply custom workspace icon/label logic
- custom MPRIS widget update functions
- bespoke polling loops not represented by existing widget primitives

Recommendation: if a feature does not fit the Dhall DSL, use `taffybar.hs`.

### 4. Use `dhall-haskell` where it helps, not as the whole model
Use `dhall-haskell` for decoding the Dhall-facing ADTs. Do not try to decode internal runtime config records directly.

## Coverage vs `imalison` current `taffybar.hs`

Likely supportable in v1/v2:
- widget list composition by section (start/center/end)
- host/backend-dependent widget selection
- css file selection
- most standard widget options (clock, pulse, backlight, disk, tray basics)
- wrapper patterns (class + boxed + stacked groups)

Needs additional design (v2+ or Haskell-only):
- custom workspace callbacks (`labelSetter`, `getWindowIconPixbuf`, custom `widgetBuilder`)
- custom MPRIS text transformation callback
- custom runtime icon remap algorithm for Hyprland windows
- fully custom polling widgets (e.g., bespoke RAM/SWAP row loop)

Practical outcome: "mostly possible" for this config is realistic if we include compositional widgets + conditional logic; full parity is not realistic without embedding a programmable language layer beyond Dhall data.

## Implementation Plan

### Phase 0: RFC and schema freeze
1. Add this proposal and request feedback on schema scope.
2. Lock v1 supported widgets/combinators.

### Phase 1: MVP Dhall path
1. Refactor CLI to add startup subcommands (`auto`, `dyre`, `example`, `dhall`).
2. Preserve current no-arg behavior by mapping it to `auto`.
3. Add `dhall` and `dhall-haskell` dependencies.
4. Implement Dhall decoding for top-level config + 6-10 common widgets.
5. Implement interpreter to `SimpleTaffyConfig`.
6. Wire `taffybar dhall` path in `app/Main.hs`.
7. Add docs in `doc/config.md` with CLI usage and a `taffybar.dhall` example.

### Phase 2: Composition + conditions
1. Add widget combinators (`WithClass`, `WithContentsBox`, `Box`, `Sequence`).
2. Add backend and hostname conditions.
3. Add hook toggles.

### Phase 3: Advanced widget presets
1. Add workspace preset options (data-only toggles for common settings).
2. Add tray/menu backend options and more widget-specific fields.
3. Add compatibility test fixtures for several real configs.

## Testing Strategy
1. Unit test Dhall decode (valid + invalid schema cases).
2. Unit test interpreter for deterministic mapping from spec to config structures.
3. Integration smoke test: load sample `taffybar.dhall`, build widget trees, no runtime exceptions during startup path.
4. Golden tests for error messages from invalid Dhall.

## Risks
1. Schema drift: adding widgets over time can bloat schema.
2. User expectations of full Haskell parity.
3. Long-term maintenance cost of interpreter.

Mitigations:
- Version schema docs.
- Clearly label unsupported features and Haskell escape hatch.
- Keep v1 scope intentionally small, then iterate.

## Open Questions
1. Should `taffybar auto` and plain `taffybar` remain strict compatibility mode forever, or eventually warn users toward explicit subcommands?
2. For unsupported advanced behavior, should we add a limited plugin hook system, or keep explicit "use Haskell config" guidance only?
3. Do we want a built-in converter to help migrate from common `SimpleTaffyConfig` Haskell patterns to Dhall templates?
