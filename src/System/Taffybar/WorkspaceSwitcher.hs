{-# LANGUAGE ScopedTypeVariables #-}
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

import qualified Control.Concurrent.MVar as MV
import Control.Monad
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.List ((\\), findIndices)
import qualified Graphics.UI.Gtk as Gtk
import Graphics.X11.Xlib.Extras

import System.Taffybar.Pager
import System.Information.EWMHDesktopInfo

type Desktop = [Workspace]
data Workspace = Workspace { label  :: Gtk.Label
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
wspaceSwitcherNew :: Pager -> IO Gtk.Widget
wspaceSwitcherNew pager = do
  switcher <- Gtk.hBoxNew False 0
  desktop  <- getDesktop pager
  deskRef  <- MV.newMVar desktop
  populateSwitcher switcher deskRef

  -- These callbacks need to use postGUIAsync since they run in
  -- another thread
  let cfg = config pager
      activecb = activeCallback cfg deskRef
      redrawcb = redrawCallback pager deskRef switcher
      urgentcb = urgentCallback cfg deskRef
  subscribe pager activecb "_NET_CURRENT_DESKTOP"
  subscribe pager redrawcb "_NET_DESKTOP_NAMES"
  subscribe pager redrawcb "_NET_NUMBER_OF_DESKTOPS"
  subscribe pager urgentcb "WM_HINTS"

  return $ Gtk.toWidget switcher

-- | List of indices of all available workspaces.
allWorkspaces :: Desktop -> [Int]
allWorkspaces desktop = [0 .. length desktop - 1]

-- | List of indices of all the workspaces that contain at least one window.
nonEmptyWorkspaces :: IO [Int]
nonEmptyWorkspaces = withDefaultCtx $ mapM getWorkspace =<< getWindows

-- | Return a list of two-element tuples, one for every workspace,
-- containing the Label widget used to display the name of that specific
-- workspace and a String with its default (unmarked) representation.
getDesktop :: Pager -> IO Desktop
getDesktop pager = do
  names  <- withDefaultCtx getWorkspaceNames
  labels <- toLabels $ map (hiddenWorkspace $ config pager) names
  return $ zipWith (\n l -> Workspace l n False) names labels

-- | Take an existing Desktop IORef and update it if necessary, store the result
-- in the IORef, then return True if the reference was actually updated, False
-- otherwise.
updateDesktop :: Pager -> MV.MVar Desktop -> IO Bool
updateDesktop pager deskRef = do
  wsnames <- withDefaultCtx getWorkspaceNames
  MV.modifyMVar deskRef $ \desktop ->
    case length wsnames /= length desktop of
      True -> do
        desk' <- getDesktop pager
        return (desk', True)
      False -> return (desktop, False)

-- | Clean up the given box, then fill it up with the buttons for the current
-- state of the desktop.
populateSwitcher :: Gtk.BoxClass box => box -> MV.MVar Desktop -> IO ()
populateSwitcher switcher deskRef = do
  containerClear switcher
  desktop <- MV.readMVar deskRef
  mapM_ (addButton switcher desktop) (allWorkspaces desktop)
  Gtk.widgetShowAll switcher

-- | Build a suitable callback function that can be registered as Listener
-- of "_NET_CURRENT_DESKTOP" standard events. It will track the position of
-- the active workspace in the desktop.
activeCallback :: PagerConfig -> MV.MVar Desktop -> Event -> IO ()
activeCallback cfg deskRef _ = Gtk.postGUIAsync $ do
  curr <- withDefaultCtx getVisibleWorkspaces
  desktop <- MV.readMVar deskRef
  case curr of
    visible : _ | length desktop > visible -> do
      when (urgent (desktop !! visible)) $ do
        toggleUrgent deskRef visible False
      transition cfg desktop curr
    _ -> return ()

-- | Build a suitable callback function that can be registered as Listener
-- of "WM_HINTS" standard events. It will display in a different color any
-- workspace (other than the active one) containing one or more windows
-- with its urgency hint set.
urgentCallback :: PagerConfig -> MV.MVar Desktop -> Event -> IO ()
urgentCallback cfg deskRef event = Gtk.postGUIAsync $ do
  desktop <- MV.readMVar deskRef
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
redrawCallback :: Gtk.BoxClass box => Pager -> MV.MVar Desktop -> box -> Event -> IO ()
redrawCallback pager deskRef box _ = Gtk.postGUIAsync $ do
  -- updateDesktop indirectly invokes some gtk functions, so it also
  -- needs to be guarded by postGUIAsync
  deskChanged <- updateDesktop pager deskRef
  when deskChanged $ populateSwitcher box deskRef

-- | Remove all children of a container.
containerClear :: Gtk.ContainerClass self => self -> IO ()
containerClear container = Gtk.containerForeach container (Gtk.containerRemove container)

-- | Convert the given list of Strings to a list of Label widgets.
toLabels :: [String] -> IO [Gtk.Label]
toLabels = mapM labelNewMarkup
  where labelNewMarkup markup = do
          lbl <- Gtk.labelNew (Nothing :: Maybe String)
          Gtk.labelSetMarkup lbl markup
          return lbl

-- | Build a new clickable event box containing the Label widget that
-- corresponds to the given index, and add it to the given container.
addButton :: Gtk.BoxClass self
          => self    -- ^ Graphical container.
          -> Desktop -- ^ List of all workspace Labels available.
          -> Int     -- ^ Index of the workspace to use.
          -> IO ()
addButton hbox desktop idx
  | length desktop > idx = do
    let index = desktop !! idx
        lbl = label index
    ebox <- Gtk.eventBoxNew
    Gtk.widgetSetName ebox $ name index
    Gtk.eventBoxSetVisibleWindow ebox False
    _ <- Gtk.on ebox Gtk.buttonPressEvent $ switch idx
    Gtk.containerAdd ebox lbl
    Gtk.boxPackStart hbox ebox Gtk.PackNatural 0
  | otherwise = return ()

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
  case wss of
    active:rest -> do
      mark desktop (activeWorkspace cfg) active
      mapM_ (mark desktop $ visibleWorkspace cfg) rest
    _ -> return ()
  mapM_ (mark desktop $ urgentWorkspace cfg) urgentWs

-- | Apply the given marking function to the Label of the workspace with
-- the given index.
mark :: Desktop            -- ^ List of all available labels.
     -> (String -> String) -- ^ Marking function.
     -> Int                -- ^ Index of the Label to modify.
     -> IO ()
mark desktop decorate idx
  | length desktop > idx = do
    let ws = desktop !! idx
    Gtk.postGUIAsync $ Gtk.labelSetMarkup (label ws) $ decorate' (name ws)
  | otherwise = return ()
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
toggleUrgent :: MV.MVar Desktop -- ^ IORef to modify.
             -> Int           -- ^ Index of the Workspace to replace.
             -> Bool          -- ^ New value of the "urgent" flag.
             -> IO ()
toggleUrgent deskRef idx isUrgent =
  MV.modifyMVar_ deskRef $ \desktop -> do
    let ws = desktop !! idx
    case length desktop > idx of
      True | isUrgent /= urgent ws -> do
               let ws' = ws { urgent = isUrgent }
                   (ys, zs) = splitAt idx desktop
               case zs of
                 _ : rest -> return $ ys ++ (ws' : rest)
                 _ -> return (ys ++ [ws'])
      _ -> return desktop

