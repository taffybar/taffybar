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
import Data.List ((\\), findIndices)
import Graphics.UI.Gtk hiding (get)
import Graphics.X11.Xlib.Extras

import System.Taffybar.Pager
import System.Information.EWMHDesktopInfo

type Desktop = [Workspace]
data Workspace = Workspace { label  :: Label
                           , name   :: String
                           , urgent :: Bool
                           }
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
-- > import System.Taffybar.Hooks.PagerHints (pagerHints)
-- > main = do
-- >   xmonad $ ewmh $ pagerHints $ defaultConfig
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
  desktop <- getDesktop pager
  widget  <- assembleWidget desktop
  deskRef <- newIORef desktop
  wRef    <- newIORef widget
  let cfg = config pager
      activecb = activeCallback cfg deskRef
      nodeskscb = nodesksCallback cfg deskRef wRef
      urgentcb = urgentCallback cfg deskRef
  subscribe pager activecb "_NET_CURRENT_DESKTOP"
  subscribe pager nodeskscb "_NET_NUMBER_OF_DESKTOPS"
  subscribe pager urgentcb "WM_HINTS"
  return widget

-- | List of indices of all available workspaces.
allWorkspaces :: Desktop -> [Int]
allWorkspaces desktop = [0 .. length desktop - 1]

-- | Return a list of two-element tuples, one for every workspace,
-- containing the Label widget used to display the name of that specific
-- workspace and a String with its default (unmarked) representation.
getDesktop :: Pager -> IO Desktop
getDesktop pager = do
  names  <- withDefaultCtx getWorkspaceNames
  labels <- toLabels $ map (hiddenWorkspace $ config pager) names
  return $ zipWith (\n l -> Workspace l n False) names labels

-- | Take an existing Desktop IORef and update it, storing the result in the
-- IORef.
updateDesktop :: PagerConfig -> IORef Desktop -> IO ()
updateDesktop cfg deskRef = do
  names  <- withDefaultCtx getWorkspaceNames
  labels <- toLabels $ map (hiddenWorkspace cfg) names
  atomicWriteIORef deskRef $ zipWith (\n l -> Workspace l n False) names labels

-- | Build the graphical representation of the widget.
assembleWidget :: Desktop -> IO Widget
assembleWidget desktop = do
  hbox <- hBoxNew False 0
  mapM_ (addButton hbox desktop) (allWorkspaces desktop)
  widgetShowAll hbox
  return $ toWidget hbox

-- | Insert widgets into an existing Box.
updateWidget :: BoxClass self => self -> Desktop -> IO ()
updateWidget box desktop = do
  mapM_ (addButton box desktop) (allWorkspaces desktop)
  widgetShowAll box

-- | Build a suitable callback function that can be registered as Listener
-- of "_NET_CURRENT_DESKTOP" standard events. It will track the position of
-- the active workspace in the desktop.
activeCallback :: PagerConfig -> IORef Desktop -> Event -> IO ()
activeCallback cfg deskRef _ = do
  curr <- withDefaultCtx getVisibleWorkspaces
  desktop <- readIORef deskRef
  let visible = head curr
  when (urgent $ desktop !! visible) $
    liftIO $ toggleUrgent deskRef visible False
  transition cfg desktop curr

-- | Build a suitable callback function that can be registered as Listener
-- of "WM_HINTS" standard events. It will display in a different color any
-- workspace (other than the active one) containing one or more windows
-- with its urgency hint set.
urgentCallback :: PagerConfig -> IORef Desktop -> Event -> IO ()
urgentCallback cfg deskRef event = do
  desktop <- readIORef deskRef
  withDefaultCtx $ do
    let window = ev_window event
    isUrgent <- isWindowUrgent window
    when isUrgent $ do
      this <- getCurrentWorkspace
      that <- getWorkspace window
      when (this /= that) $ liftIO $ do
        toggleUrgent deskRef that True
        mark desktop (urgentWorkspace cfg) that

-- | Build a suitable callback function that can be registered as Listener
-- of "_NET_NUMBER_OF_DESKTOPS" standard events. It will handle dynamically
-- adding and removing workspaces.
nodesksCallback :: PagerConfig -> IORef Desktop -> IORef Widget -> Event -> IO ()
nodesksCallback cfg deskRef wRef _ = do
  updateDesktop cfg deskRef
  desktop <- readIORef deskRef
  visible <- withDefaultCtx getVisibleWorkspaces
  widget <- readIORef wRef
  let box = castToBox widget
  containerClear box
  updateWidget box desktop 
  transition cfg desktop visible

-- | Remove all children of a container.
containerClear :: ContainerClass self => self -> IO ()
containerClear container = containerForeach container (containerRemove container)

-- | Convert the given list of Strings to a list of Label widgets.
toLabels :: [String] -> IO [Label]
toLabels = mapM labelNewMarkup
  where labelNewMarkup markup = do
          label <- labelNew Nothing
          labelSetMarkup label markup
          return label

-- | Build a new clickable event box containing the Label widget that
-- corresponds to the given index, and add it to the given container.
addButton :: BoxClass self
          => self    -- ^ Graphical container.
          -> Desktop -- ^ List of all workspace Labels available.
          -> Int     -- ^ Index of the workspace to use.
          -> IO ()
addButton hbox desktop idx = do
  let index = desktop !! idx
      lbl = label index
  ebox <- eventBoxNew
  widgetSetName ebox $ name index
  eventBoxSetVisibleWindow ebox False
  _ <- on ebox buttonPressEvent $ switch idx
  containerAdd ebox lbl
  boxPackStart hbox ebox PackNatural 0

-- | List of indices of all the workspaces that contain at least one window.
nonEmptyWorkspaces :: IO [Int]
nonEmptyWorkspaces = withDefaultCtx $ mapM getWorkspace =<< getWindows

-- | Re-mark all workspace labels.
transition :: PagerConfig -- ^ Configuration settings.
           -> Desktop     -- ^ All available Labels with their default values.
           -> [Int]       -- ^ Currently visible workspaces (first is active).
           -> IO ()
transition cfg desktop wss = do
  nonEmpty <- fmap (filter (>=0)) nonEmptyWorkspaces
  let urgentWs = findIndices urgent desktop
      allWs    = (allWorkspaces desktop) \\ urgentWs
      nonEmptyWs = nonEmpty \\ urgentWs
  mapM_ (mark desktop $ hiddenWorkspace cfg) nonEmptyWs
  mapM_ (mark desktop $ emptyWorkspace cfg) (allWs \\ nonEmpty)
  mark desktop (activeWorkspace cfg) (head wss)
  mapM_ (mark desktop $ visibleWorkspace cfg) (tail wss)
  mapM_ (mark desktop $ urgentWorkspace cfg) urgentWs

-- | Apply the given marking function to the Label of the workspace with
-- the given index.
mark :: Desktop            -- ^ List of all available labels.
     -> (String -> Markup) -- ^ Marking function.
     -> Int                -- ^ Index of the Label to modify.
     -> IO ()
mark desktop decorate idx = do
  let ws = desktop !! idx
  postGUIAsync $ labelSetMarkup (label ws) $ decorate' (name ws)
  where decorate' = pad . decorate
        pad m | m == [] = m
              | otherwise = ' ' : m

-- | Switch to the workspace with the given index.
switch :: (MonadIO m) => Int -> m Bool
switch idx = do
  liftIO $ withDefaultCtx (switchToWorkspace idx)
  return True

-- | Modify the Desktop inside the given IORef, so that the Workspace at the
-- given index has its "urgent" flag set to the given value.
toggleUrgent :: IORef Desktop -- ^ IORef to modify.
             -> Int           -- ^ Index of the Workspace to replace.
             -> Bool          -- ^ New value of the "urgent" flag.
             -> IO ()
toggleUrgent deskRef idx isUrgent = do
  desktop <- readIORef deskRef
  let ws = desktop !! idx
  unless (isUrgent == urgent ws) $ do
    let ws' = (desktop !! idx) { urgent = isUrgent }
    let (ys, zs) = splitAt idx desktop
        in writeIORef deskRef $ ys ++ (ws' : tail zs)
