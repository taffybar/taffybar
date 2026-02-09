# Icon+Label Box Abstraction Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add `buildIconLabelBox` utility for composing an icon widget + text widget with standardized CSS classes, then refactor PulseAudio and NetworkManager to use separate icon/text widgets instead of Pango markup hacks with hardcoded fonts.

**Architecture:** A small utility in `Widget.Util` applies CSS classes `.icon-label` (box), `.icon` (first child), `.label` (second child). Each widget module exposes three public functions: icon-only, text-only, and combined. The combined variant uses `buildIconLabelBox`. Format strings drop `$icon$`; icon text goes to a plain label styled via CSS.

**Tech Stack:** gi-gtk, Haskell

---

### Task 1: Add `buildIconLabelBox` to Widget.Util

**Files:**
- Modify: `src/System/Taffybar/Widget/Util.hs`

**Step 1: Add the function**

Add after `buildContentsBox` (around line 314):

```haskell
-- | Combine an icon widget and a label widget in a horizontal box with
-- standardised CSS classes. The box gets class @icon-label@, the first child
-- gets @icon@, and the second child gets @label@.
buildIconLabelBox :: MonadIO m => Gtk.Widget -> Gtk.Widget -> m Gtk.Widget
buildIconLabelBox iconWidget labelWidget = liftIO $ do
  box <- Gtk.boxNew Gtk.OrientationHorizontal 0
  _ <- widgetSetClassGI iconWidget "icon"
  _ <- widgetSetClassGI labelWidget "label"
  Gtk.containerAdd box iconWidget
  Gtk.containerAdd box labelWidget
  _ <- widgetSetClassGI box "icon-label"
  Gtk.widgetShowAll box
  Gtk.toWidget box
```

**Step 2: Build to verify it compiles**

Run: `cd ~/dotfiles/dotfiles/config/taffybar && cabal build taffybar`
Expected: compiles (function is not yet used)

**Step 3: Commit**

```
feat: add buildIconLabelBox utility for icon+text widget composition
```

---

### Task 2: Refactor PulseAudio to separate icon and text widgets

**Files:**
- Modify: `src/System/Taffybar/Widget/PulseAudio.hs`

**Step 1: Update exports**

Change the module export list to:

```haskell
module System.Taffybar.Widget.PulseAudio
  ( PulseAudioWidgetConfig(..)
  , defaultPulseAudioWidgetConfig
  , pulseAudioIconNew
  , pulseAudioIconNewWith
  , pulseAudioLabelNew
  , pulseAudioLabelNewWith
  , pulseAudioNew
  , pulseAudioNewWith
  ) where
```

**Step 2: Add `Util` import**

Add to the import block:

```haskell
import System.Taffybar.Widget.Util (buildIconLabelBox)
```

**Step 3: Remove `$icon$` from format strings**

In `defaultPulseAudioWidgetConfig`, change:

```haskell
    , pulseAudioFormat = "$volume$%"
    , pulseAudioMuteFormat = "muted"
    , pulseAudioUnknownFormat = "n/a"
```

Remove the comment about `escapeIconText` spacing (line 55-56).

**Step 4: Remove `$icon$` from `buildAttrs` and `buildUnknownAttrs`**

In `buildAttrs` (line 142): remove the `iconText` computation, the `icon <- escapeIconText iconText` line, and the `("icon", icon)` entry from the returned list.

In `buildUnknownAttrs` (line 163): remove the `icon <- escapeIconText ...` line and the `("icon", icon)` entry.

**Step 5: Delete `escapeIconText`, `isPUA`**

Remove lines 194-221 entirely.

**Step 6: Create `pulseAudioIconNew` / `pulseAudioIconNewWith`**

These create a plain `Gtk.Label` whose text is the nerd font icon character. The label text is updated via the same channel used by the text label.

```haskell
pulseAudioIconNew :: TaffyIO Gtk.Widget
pulseAudioIconNew = pulseAudioIconNewWith defaultPulseAudioWidgetConfig

pulseAudioIconNewWith :: PulseAudioWidgetConfig -> TaffyIO Gtk.Widget
pulseAudioIconNewWith config = do
  let sinkSpec = pulseAudioSink config
  chan <- getPulseAudioInfoChan sinkSpec
  initialInfo <- getPulseAudioInfoState sinkSpec
  liftIO $ do
    label <- Gtk.labelNew Nothing
    let updateIcon info = do
          let iconText = case info of
                Nothing -> T.pack "\xF026"
                Just i -> pulseAudioTextIcon (pulseAudioMuted i) (pulseAudioVolumePercent i)
          postGUIASync $ Gtk.labelSetText label iconText
    void $ Gtk.onWidgetRealize label $ updateIcon initialInfo
    Gtk.widgetShowAll label
    Gtk.toWidget =<< channelWidgetNew label chan updateIcon
```

**Step 7: Rename existing `pulseAudioLabelNew` and strip icon logic**

The existing `pulseAudioLabelNew`/`pulseAudioLabelNewWith` stay as text-only. They already work correctly once `$icon$` is removed from the format strings and `buildAttrs`/`buildUnknownAttrs` no longer produce an `icon` attribute. No code change needed beyond what was done in steps 3-5.

**Step 8: Create `pulseAudioNew` / `pulseAudioNewWith`**

```haskell
pulseAudioNew :: TaffyIO Gtk.Widget
pulseAudioNew = pulseAudioNewWith defaultPulseAudioWidgetConfig

pulseAudioNewWith :: PulseAudioWidgetConfig -> TaffyIO Gtk.Widget
pulseAudioNewWith config = do
  iconWidget <- pulseAudioIconNewWith config
  labelWidget <- pulseAudioLabelNewWith config
  liftIO $ buildIconLabelBox iconWidget labelWidget
```

**Step 9: Build to verify**

Run: `cd ~/dotfiles/dotfiles/config/taffybar && cabal build taffybar`
Expected: compiles clean

**Step 10: Commit**

```
refactor: PulseAudio - separate icon and text into independent widgets

Remove hardcoded "Iosevka Nerd Font" Pango markup. The icon is now a
plain label styled via CSS class .icon inside an .icon-label box.
Expose pulseAudioIconNew, pulseAudioLabelNew, and pulseAudioNew.
```

---

### Task 3: Refactor NetworkManager to separate icon and text widgets

**Files:**
- Modify: `src/System/Taffybar/Widget/NetworkManager.hs`

This follows the same pattern as PulseAudio. NetworkManager has two widget families (Wifi and Network), each of which gets the icon/text/combined split.

**Step 1: Update exports**

Add to the module export list:

```haskell
  -- Wifi text icon
  , networkManagerWifiIconLabelNew
  , networkManagerWifiIconLabelNewWith
  , networkManagerWifiTextIconNew
  , networkManagerWifiTextIconNewWith

  -- Network text icon
  , networkManagerNetworkIconLabelNew
  , networkManagerNetworkIconLabelNewWith
  , networkManagerNetworkTextIconNew
  , networkManagerNetworkTextIconNewWith
```

Note: the existing `networkManagerWifiIconNew` is a *theme icon* (GTK `Image`), not a text icon. We keep it. The new text-icon variants use nerd font chars in a `Label`.

**Step 2: Add `Util` import**

```haskell
import System.Taffybar.Widget.Util (buildIconLabelBox)
```

**Step 3: Remove `$icon$` from all format strings**

In `defaultWifiWidgetConfig`:
```haskell
    { wifiConnectedFormat = "$ssid$ $strength$%"
    , wifiDisconnectedFormat = "disconnected"
    , wifiDisabledFormat = "off"
    , wifiUnknownFormat = "unknown"
```

In `defaultNetworkWidgetConfig`:
```haskell
    { networkWifiFormat = "$ssid$ $strength$%"
    , networkWiredFormat = "$connection$"
    , networkVpnFormat = "$connection$"
    , networkOtherFormat = "$type$ $connection$"
    , networkDisconnectedFormat = "disconnected"
    , networkWifiDisabledFormat = "disconnected (wifi off)"
    , networkUnknownFormat = "unknown"
```

Remove the `escapeIconText` spacing comments.

**Step 4: Remove `icon` from `buildAttrs` and `buildNetworkAttrs`**

In `buildAttrs` (wifi): remove `iconText = wifiTextIcon info`, `icon <- escapeIconText iconText`, and `("icon", icon)`.

In `buildNetworkAttrs`: remove `iconText = networkTextIcon info`, `icon <- escapeIconText iconText`, and `("icon", icon)`.

**Step 5: Delete `escapeIconText`, `isPUA`**

Remove the duplicated `escapeIconText` and `isPUA` functions (lines 282-309).

**Step 6: Create wifi text-icon functions**

```haskell
networkManagerWifiTextIconNew :: TaffyIO Widget
networkManagerWifiTextIconNew =
  networkManagerWifiTextIconNewWith defaultWifiWidgetConfig

networkManagerWifiTextIconNewWith :: WifiWidgetConfig -> TaffyIO Widget
networkManagerWifiTextIconNewWith _config = do
  chan <- getWifiInfoChan
  ctx <- ask
  liftIO $ do
    label <- labelNew Nothing
    let updateIcon info = do
          let iconText = wifiTextIcon info
          postGUIASync $ labelSetText label iconText
    void $ onWidgetRealize label $
      runReaderT getWifiInfoState ctx >>= updateIcon
    toWidget =<< channelWidgetNew label chan updateIcon
```

**Step 7: Create wifi icon-label combined function**

```haskell
networkManagerWifiIconLabelNew :: TaffyIO Widget
networkManagerWifiIconLabelNew =
  networkManagerWifiIconLabelNewWith defaultWifiWidgetConfig

networkManagerWifiIconLabelNewWith :: WifiWidgetConfig -> TaffyIO Widget
networkManagerWifiIconLabelNewWith config = do
  iconWidget <- networkManagerWifiTextIconNewWith config
  labelWidget <- networkManagerWifiLabelNewWith config
  liftIO $ buildIconLabelBox iconWidget labelWidget
```

**Step 8: Create network text-icon functions**

```haskell
networkManagerNetworkTextIconNew :: TaffyIO Widget
networkManagerNetworkTextIconNew =
  networkManagerNetworkTextIconNewWith defaultNetworkWidgetConfig

networkManagerNetworkTextIconNewWith :: NetworkWidgetConfig -> TaffyIO Widget
networkManagerNetworkTextIconNewWith _config = do
  chan <- getNetworkInfoChan
  ctx <- ask
  liftIO $ do
    label <- labelNew Nothing
    let updateIcon info = do
          let iconText = networkTextIcon info
          postGUIASync $ labelSetText label iconText
    void $ onWidgetRealize label $
      runReaderT getNetworkInfoState ctx >>= updateIcon
    toWidget =<< channelWidgetNew label chan updateIcon
```

**Step 9: Create network icon-label combined function**

```haskell
networkManagerNetworkIconLabelNew :: TaffyIO Widget
networkManagerNetworkIconLabelNew =
  networkManagerNetworkIconLabelNewWith defaultNetworkWidgetConfig

networkManagerNetworkIconLabelNewWith :: NetworkWidgetConfig -> TaffyIO Widget
networkManagerNetworkIconLabelNewWith config = do
  iconWidget <- networkManagerNetworkTextIconNewWith config
  labelWidget <- networkManagerNetworkLabelNewWith config
  liftIO $ buildIconLabelBox iconWidget labelWidget
```

**Step 10: Build to verify**

Run: `cd ~/dotfiles/dotfiles/config/taffybar && cabal build taffybar`
Expected: compiles clean

**Step 11: Commit**

```
refactor: NetworkManager - separate icon and text into independent widgets

Remove hardcoded "Iosevka Nerd Font" Pango markup. Add text-icon-only
and icon-label combined variants for both Wifi and Network widget
families.
```

---

### Task 4: Update user config to use new combined widgets

**Files:**
- Modify: `/home/imalison/dotfiles/dotfiles/config/taffybar/taffybar.hs`

**Step 1: Switch audioWidget to use `pulseAudioNew`**

```haskell
audioWidget :: TaffyIO Gtk.Widget
audioWidget =
  decorateWithClassAndBoxM "audio" PulseAudio.pulseAudioNew
```

**Step 2: Switch networkWidget to use `networkManagerWifiIconLabelNew`**

```haskell
networkWidget :: TaffyIO Gtk.Widget
networkWidget =
  decorateWithClassAndBoxM "network" NetworkManager.networkManagerWifiIconLabelNew
```

**Step 3: Add CSS for `.icon` class in user stylesheet**

Add to the user's `taffybar.css`:

```css
.icon-label > .icon {
  font-family: "Iosevka Nerd Font";
}
```

**Step 4: Build and test visually**

Run: `cd ~/dotfiles/dotfiles/config/taffybar && cabal run taffybar -- --no-dyre`
Expected: audio and network widgets render with icon + text, styled by CSS

**Step 5: Commit**

```
chore: update user config to use new icon-label widget variants
```
