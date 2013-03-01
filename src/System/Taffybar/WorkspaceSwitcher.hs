-----------------------------------------------------------------------------
-- |
-- Module      : System.Taffybar.WorkspaceSwitcher
-- Copyright   : (c) José A. Romero L.
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : José A. Romero L. <escherdragon@gmail.com>
-- Stability   : unstable
-- Portability : unportable
--
-- Composite widget that displays all currently configured workspaces and
-- allows to switch to any of them by clicking on its label. Supports also
-- urgency hints and (with an additional hook) display of other visible
-- workspaces besides the active one (in Xinerama or XRandR installations).
--
-- N.B. If you're just looking for a drop-in replacement for the
-- "System.Taffybar.XMonadLog" widget that is clickable and doesn't require
-- DBus, you may want to see first "System.Taffybar.TaffyPager".
--
-----------------------------------------------------------------------------

module System.Taffybar.WorkspaceSwitcher (
  -- * Usage
  -- $usage
  wspaceSwitcherNew
) where

import Control.Monad
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef
import Graphics.UI.Gtk
import Graphics.X11.Xlib.Extras

import System.Taffybar.Pager
import System.Information.EWMHDesktopInfo

type Desktop = [Workspace]
type Workspace = (Label, String)

-- $usage
--
-- This widget requires that the EwmhDesktops hook from the XMonadContrib
-- project be installed in your @xmonad.hs@ file:
--
-- > import XMonad.Hooks.EwmhDesktops (ewmh)
-- > main = do
-- >   xmonad $ ewmh $ defaultConfig
-- > ...
--
-- Urgency hooks are not required for the urgency hints displaying to work
-- (since it is also based on desktop events), but if you use @focusUrgent@
-- you may want to keep the \"@withUrgencyHook NoUrgencyHook@\" anyway.
--
-- Unfortunately, in multiple monitor installations EWMH does not provide a
-- way to determine what desktops are shown in secondary displays. Thus, if
-- you have more than one monitor you may want to additionally install the
-- "System.Taffybar.Hooks.PagerHints" hook in your @xmonad.hs@:
--
-- > import System.Taffybar.Hooks.PagerHints (tbph)
-- > main = do
-- >   xmonad $ ewmh $ tbph $ defaultConfig
-- > ...
--
-- Once you've properly configured @xmonad.hs@, you can use the widget in
-- your @taffybar.hs@ file:
--
-- > import System.Taffybar.WorkspaceSwitcher
-- > main = do
-- >   pager <- pagerNew defaultPagerConfig
-- >   let wss = wspaceSwitcherNew pager
--
-- now you can use @wss@ as any other Taffybar widget.

-- | Create a new WorkspaceSwitcher widget that will use the given Pager as
-- its source of events.
wspaceSwitcherNew :: Pager -> IO Widget
wspaceSwitcherNew pager = do
  desktop <- getDesktop
  widget  <- assembleWidget desktop
  idxRef  <- newIORef []
  let cfg = config pager
      activecb = activeCallback cfg desktop idxRef
      urgentcb = urgentCallback cfg desktop
  subscribe pager activecb "_NET_CURRENT_DESKTOP"
  subscribe pager urgentcb "WM_HINTS"
  return widget

-- | Return a list of two-element tuples, one for every workspace,
-- containing the Label widget used to display the name of that specific
-- workspace and a String with its default (unmarked) representation.
getDesktop :: IO Desktop
getDesktop = do
  names  <- withDefaultCtx getWorkspaceNames
  labels <- toLabels names
  return $ zip labels names

-- | Build the graphical representation of the widget.
assembleWidget :: Desktop -> IO Widget
assembleWidget desktop = do
  hbox <- hBoxNew False 3
  mapM_ (addButton hbox desktop) $ [0..(length desktop - 1)]
  widgetShowAll hbox
  return $ toWidget hbox

-- | Build a suitable callback function that can be registered as Listener
-- of "_NET_CURRENT_DESKTOP" standard events. It will track the position of
-- the active workspace in the desktop.
activeCallback :: PagerConfig -> Desktop -> IORef [Int] -> Event -> IO ()
activeCallback cfg desktop ref _ = do
  prev <- readIORef ref
  curr <- withDefaultCtx getVisibleWorkspaces
  transition cfg desktop prev curr
  writeIORef ref curr

-- | Build a suitable callback function that can be registered as Listener
-- of "WM_HINTS" standard events. It will display in a different color any
-- workspace (other than the active one) containing one or more windows
-- with its urgency hint set.
urgentCallback :: PagerConfig -> Desktop -> Event -> IO ()
urgentCallback cfg desktop event = withDefaultCtx $ do
  let window = ev_window event
  isUrgent <- isWindowUrgent window
  when isUrgent $ do
    this <- getCurrentWorkspace
    that <- getWorkspace window
    when (this /= that) $ do
      liftIO $ mark desktop (urgentWorkspace cfg) that

-- | Convert the given list of Strings to a list of Label widgets.
toLabels :: [String] -> IO [Label]
toLabels = sequence . map (labelNew . Just)

-- | Build a new clickable event box containing the Label widget that
-- corresponds to the given index, and add it to the given container.
addButton :: HBox    -- ^ Graphical container.
          -> Desktop -- ^ List of all workspace Labels available.
          -> Int     -- ^ Index of the workspace to use.
          -> IO ()
addButton hbox desktop idx = do
  let label = fst (desktop !! idx)
  ebox <- eventBoxNew
  eventBoxSetVisibleWindow ebox False
  _ <- on ebox buttonPressEvent (switch desktop idx)
  containerAdd ebox label
  boxPackStart hbox ebox PackNatural 0

-- | Perform all changes needed whenever the active workspace changes.
transition :: PagerConfig -- ^ Configuration settings.
           -> Desktop -- ^ All available Labels with their default values.
           -> [Int] -- ^ Previously visible workspaces (first was active).
           -> [Int] -- ^ Currently visible workspaces (first is active).
           -> IO ()
transition cfg desktop prev curr =
  when (curr /= prev) $ do
    mapM_ (mark desktop id) prev
    mark desktop (activeWorkspace cfg) (head curr)
    mapM_ (mark desktop $ visibleWorkspace cfg) (tail curr)

-- | Apply the given marking function to the Label of the workspace with
-- the given index.
mark :: Desktop -- ^ List of all available labels.
     -> (String -> Markup) -- ^ Marking function.
     -> Int -- ^ Index of the Label to modify.
     -> IO ()
mark desktop decorate idx = do
  let ws = desktop !! idx
  postGUIAsync $ labelSetMarkup (fst ws) $ decorate (snd ws)

-- | Switch to the workspace with the given index.
switch :: (MonadIO m) => Desktop -> Int -> m Bool
switch desktop idx = do
  liftIO $ withDefaultCtx (switchToWorkspace idx)
  return True