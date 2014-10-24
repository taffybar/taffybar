{-# LANGUAGE TupleSections #-}

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
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import Graphics.UI.Gtk hiding (get)
import Graphics.X11.Xlib.Extras
import qualified Data.Map as M

import System.Taffybar.Pager
import System.Information.EWMHDesktopInfo
import System.Information.X11DesktopInfo

type Desktop = M.Map WorkspaceIdx Workspace
data Workspace = Workspace { label      :: Label
                           , name       :: String
                           , visibility :: WSVisibility
                           , urgent     :: Bool
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
  switcher <- hBoxNew False 0
  desktop  <- getDesktop
  deskRef  <- newIORef desktop
  populateSwitcher switcher deskRef

  let cfg = config pager
      activecb = activeCallback cfg deskRef
      redrawcb = redrawCallback deskRef switcher
      urgentcb = urgentCallback cfg deskRef
  subscribe pager activecb "_NET_CURRENT_DESKTOP"
  subscribe pager redrawcb "_NET_NUMBER_OF_DESKTOPS"
  subscribe pager urgentcb "WM_HINTS"

  return $ toWidget switcher

-- | List the workspaces and their windows
workspaceWindows :: X11Property (M.Map WorkspaceIdx [X11Window])
workspaceWindows = do
    windows <- getWindows
    wss <- mapM (\win->getWorkspace win >>= return . flip M.singleton [win]) windows
    return $ M.unionsWith (++) wss

-- | Return a list of workspaces
getDesktop :: IO Desktop
getDesktop = do
  names  <- M.fromList `fmap` withDefaultCtx getWorkspaceNames
  T.traverse nameToWorkspace names
  where
    nameToWorkspace :: String -> IO Workspace
    nameToWorkspace wsName = do
      lbl <- labelNew (Nothing :: Maybe String)
      return $ Workspace lbl wsName Hidden False

-- | Take an existing Desktop IORef and update it if necessary, store the result
-- in the IORef, then return True if the reference was actually updated, False
-- otherwise.
updateDesktop :: IORef Desktop -> IO Bool
updateDesktop deskRef = do
  wsnames <- withDefaultCtx getWorkspaceNames
  desktop <- readIORef deskRef
  if length wsnames /= M.size desktop
    then getDesktop >>= writeIORef deskRef >> return True
    else return False

-- | Clean up the given box, then fill it up with the buttons for the current
-- state of the desktop.
populateSwitcher :: BoxClass box => box -> IORef Desktop -> IO ()
populateSwitcher switcher deskRef = do
  containerClear switcher
  desktop <- readIORef deskRef
  _ <- M.traverseWithKey (\wsIdx ws -> addButton switcher wsIdx ws) desktop
  widgetShowAll switcher

-- | Build a suitable callback function that can be registered as Listener
-- of "_NET_CURRENT_DESKTOP" standard events. It will track the position of
-- the active workspace in the desktop.
activeCallback :: PagerConfig -> IORef Desktop -> Event -> IO ()
activeCallback cfg deskRef _ = do
  visible <- withDefaultCtx getVisibleWorkspaces
  case visible of
    active:_ -> do
      toggleUrgent deskRef active False
      desktop <- readIORef deskRef
      let setVisible wsIdx ws = ws { visibility =
                                       case () of
                                         _ | wsIdx == active      -> Active
                                           | wsIdx `elem` visible -> Visible
                                           | otherwise            -> Hidden
                                   }
      writeIORef deskRef $ M.mapWithKey setVisible desktop
    _ -> return ()
  transition cfg deskRef

-- | Build a suitable callback function that can be registered as Listener
-- of "WM_HINTS" standard events. It will display in a different color any
-- workspace (other than the active one) containing one or more windows
-- with its urgency hint set.
urgentCallback :: PagerConfig -> IORef Desktop -> Event -> IO ()
urgentCallback cfg deskRef event = do
  withDefaultCtx $ do
    let window = ev_window event
    isUrgent <- isWindowUrgent window
    when isUrgent $ do
      this <- getCurrentWorkspace
      that <- getWorkspace window
      when (this /= that) $ liftIO $ do
        toggleUrgent deskRef that True
        transition cfg deskRef

-- | Build a suitable callback function that can be registered as Listener
-- of "_NET_NUMBER_OF_DESKTOPS" standard events. It will handle dynamically
-- adding and removing workspaces.
redrawCallback :: BoxClass box => IORef Desktop -> box -> Event -> IO ()
redrawCallback deskRef box _ =
  updateDesktop deskRef >>= \deskChanged ->
    when deskChanged $ postGUIAsync (populateSwitcher box deskRef)

-- | Remove all children of a container.
containerClear :: ContainerClass self => self -> IO ()
containerClear container = containerForeach container (containerRemove container)

-- | Build a new clickable event box containing the Label widget that
-- corresponds to the given index, and add it to the given container.
addButton :: BoxClass self
          => self         -- ^ Graphical container.
          -> WorkspaceIdx -- ^ Index of the workspace
          -> Workspace    -- ^ The workspace itself
          -> IO ()
addButton hbox wsidx ws = do
  let lbl = label ws
  ebox <- eventBoxNew
  widgetSetName ebox $ name ws
  eventBoxSetVisibleWindow ebox False
  _ <- on ebox buttonPressEvent $ switch wsidx
  containerAdd ebox lbl
  boxPackStart hbox ebox PackNatural 0

newtype AssocList a b = AList {getAList :: [(a,b)]}

instance Functor (AssocList a) where
  fmap f (AList xs) = AList $ map (\(a,b) -> (a, f b)) xs

instance F.Foldable (AssocList a) where
  foldMap = T.foldMapDefault

instance T.Traversable (AssocList a) where
  traverse f (AList xs) = AList `fmap` T.traverse (\(a,b)->(a,) `fmap` f b) xs

-- | Re-mark all workspace labels.
transition :: PagerConfig    -- ^ Configuration settings.
           -> IORef Desktop  -- ^ All available Labels with their default values.
           -> IO ()
transition cfg deskRef = do
  desktop <- readIORef deskRef
  windowCounts <- fmap length `fmap` withDefaultCtx workspaceWindows
  let toWSInfo wsIdx ws =
          WSInfo { wsiName       = name ws
                 , wsiWindows    = M.findWithDefault 0 wsIdx windowCounts
                 , wsiVisibility = visibility ws
                 , wsiUrgent     = urgent ws
                 }
  let markup :: AssocList Workspace Markup
      markup = markupWorkspaces cfg
               $ AList $ map (\(wsIdx, ws)->(ws, toWSInfo wsIdx ws)) $ M.toList desktop
  postGUIAsync $ mapM_ (\(ws, m) -> labelSetMarkup (label ws) (pad m)) (getAList markup)
  where
    pad "" = ""
    pad m  = ' ' : m

-- | Switch to the workspace with the given index.
switch :: (MonadIO m) => WorkspaceIdx -> m Bool
switch idx = do
  liftIO $ withDefaultCtx (switchToWorkspace idx)
  return True

-- | Modify the Desktop inside the given IORef, so that the Workspace at the
-- given index has its "urgent" flag set to the given value.
toggleUrgent :: IORef Desktop -- ^ IORef to modify.
             -> WorkspaceIdx  -- ^ Index of the Workspace to replace.
             -> Bool          -- ^ New value of the "urgent" flag.
             -> IO ()
toggleUrgent deskRef idx isUrgent =
  modifyIORef deskRef $ M.adjust f idx
  where f ws = ws { urgent = isUrgent }
