-----------------------------------------------------------------------------
-- |
-- Module      : System.Information.EWMHDesktopInfo
-- Copyright   : (c) José A. Romero L.
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : José A. Romero L. <escherdragon@gmail.com>
-- Stability   : unstable
-- Portability : unportable
--
-- Functions to access data provided by the X11 desktop via EWHM hints. This
-- module requires that the EwmhDesktops hook from the XMonadContrib project
-- be installed in your @~\/.xmonad\/xmonad.hs@ configuration:
--
-- > import XMonad
-- > import XMonad.Hooks.EwmhDesktops (ewmh)
-- >
-- > main = xmonad $ ewmh $ ...
--
-----------------------------------------------------------------------------

module System.Information.EWMHDesktopInfo
  ( X11Window      -- re-exported from X11DesktopInfo
  , withDefaultCtx -- re-exported from X11DesktopInfo
  , isWindowUrgent -- re-exported from X11DesktopInfo
  , getCurrentWorkspace
  , getVisibleWorkspaces
  , getWorkspaceNames
  , switchToWorkspace
  , getWindowTitle
  , getActiveWindowTitle
  , getWindowHandles
  , getWorkspace
  , focusWindow
  ) where

import Data.List (elemIndex)
import Data.Maybe (fromJust)
import System.Information.X11DesktopInfo

noFocus :: String
noFocus = "..."

-- | Retrieve the index of the current workspace in the desktop,
-- starting from 0.
getCurrentWorkspace :: X11Property Int
getCurrentWorkspace = readAsInt Nothing "_NET_CURRENT_DESKTOP"

-- | Retrieve the indexes of all currently visible workspaces
-- with the active workspace at the head of the list.
getVisibleWorkspaces :: X11Property [Int]
getVisibleWorkspaces = do
  vis <- getVisibleTags
  all <- getWorkspaceNames
  cur <- getCurrentWorkspace
  return $ cur : (map fromJust $ map (flip elemIndex all) vis)

-- | Return a list with the names of all the workspaces currently
-- available.
getWorkspaceNames :: X11Property [String]
getWorkspaceNames = readAsListOfString Nothing "_NET_DESKTOP_NAMES"

-- | Ask the window manager to switch to the workspace with the given
-- index, starting from 0.
switchToWorkspace :: Int -> X11Property ()
switchToWorkspace idx = do
  cmd <- getAtom "_NET_CURRENT_DESKTOP"
  sendCommandEvent cmd (fromIntegral idx)

-- | Get the title of the given X11 window.
getWindowTitle :: X11Window -> X11Property String
getWindowTitle window = do
  let w = Just window
  prop <- readAsString w "_NET_WM_NAME"
  case prop of
    "" -> return =<< readAsString w "WM_NAME"
    _  -> return prop

-- | Get the title of the currently focused window.
getActiveWindowTitle :: X11Property String
getActiveWindowTitle = do
  awt <- readAsListOfWindow Nothing "_NET_ACTIVE_WINDOW"
  case awt of
    w:ws -> if w > 0
              then return =<< getWindowTitle w
              else return noFocus
    _ -> return noFocus

-- | Return a list of two-element tuples, each containing the title
-- and the internal ID of one window, for all the windows currently
-- open, independently of their workspace.
getWindowHandles :: X11Property [(String, X11Window)]
getWindowHandles = do
  windows <- readAsListOfWindow Nothing "_NET_CLIENT_LIST"
  wtitles <- sequence $ map getWindowTitle windows
  return $ zip wtitles windows

-- | Return the index (starting from 0) of the workspace on which the
-- given window is being displayed.
getWorkspace :: X11Window -> X11Property Int
getWorkspace window = readAsInt (Just window) "_NET_WM_DESKTOP"

-- | Ask the window manager to give focus to the given window.
focusWindow :: X11Window -> X11Property ()
focusWindow wh = do
  cmd <- getAtom "_NET_ACTIVE_WINDOW"
  sendWindowEvent cmd (fromIntegral wh)