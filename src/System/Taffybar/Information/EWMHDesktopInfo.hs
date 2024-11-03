-----------------------------------------------------------------------------
-- |
-- Module      : System.Taffybar.Information.EWMHDesktopInfo
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

module System.Taffybar.Information.EWMHDesktopInfo
  ( EWMHIcon(..)
  , EWMHIconData
  , WorkspaceId(..)
  , X11Window
  , allEWMHProperties
  , ewmhActiveWindow
  , ewmhClientList
  , ewmhClientListStacking
  , ewmhCurrentDesktop
  , ewmhDesktopNames
  , ewmhNumberOfDesktops
  , ewmhStateHidden
  , ewmhWMClass
  , ewmhWMDesktop
  , ewmhWMIcon
  , ewmhWMName
  , ewmhWMName2
  , ewmhWMState
  , ewmhWMStateHidden
  , focusWindow
  , getActiveWindow
  , getCurrentWorkspace
  , getVisibleWorkspaces
  , getWindowClass
  , getWindowIconsData
  , getWindowMinimized
  , getWindowState
  , getWindowStateProperty
  , getWindowTitle
  , getWindows
  , getWindowsStacking
  , getWorkspace
  , getWorkspaceNames
  , isWindowUrgent
  , parseWindowClasses
  , switchOneWorkspace
  , switchToWorkspace
  , withX11Context
  , withEWMHIcons
  ) where

import Control.Monad ((>=>))
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Tuple
import Data.Word
import Foreign.ForeignPtr
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import System.Log.Logger
import System.Taffybar.Information.SafeX11
import System.Taffybar.Information.X11DesktopInfo

logHere :: MonadIO m => Priority -> String -> m ()
logHere p = liftIO . logM "System.Taffybar.Information.EWMHDesktopInfo" p

newtype WorkspaceId = WorkspaceId Int deriving (Show, Read, Ord, Eq)

-- A super annoying detail of the XGetWindowProperty interface is that: "If the
-- returned format is 32, the returned data is represented as a long array and
-- should be cast to that type to obtain the elements." This means that even
-- though only the 4 least significant bits will ever contain any data, the
-- array that is returned from X11 can have a larger word size. This means that
-- we need to manipulate the underlying data in annoying ways to pass it to gtk
-- appropriately.
type PixelsWordType = Word64

type EWMHProperty = String

ewmhActiveWindow, ewmhClientList, ewmhClientListStacking, ewmhCurrentDesktop, ewmhDesktopNames, ewmhNumberOfDesktops, ewmhStateHidden, ewmhWMDesktop, ewmhWMStateHidden, ewmhWMClass, ewmhWMState, ewmhWMIcon, ewmhWMName, ewmhWMName2 :: EWMHProperty
ewmhActiveWindow = "_NET_ACTIVE_WINDOW"
ewmhClientList = "_NET_CLIENT_LIST"
ewmhClientListStacking = "_NET_CLIENT_LIST_STACKING"
ewmhCurrentDesktop = "_NET_CURRENT_DESKTOP"
ewmhDesktopNames = "_NET_DESKTOP_NAMES"
ewmhNumberOfDesktops = "_NET_NUMBER_OF_DESKTOPS"
ewmhStateHidden = "_NET_WM_STATE_HIDDEN"
ewmhWMClass = "WM_CLASS"
ewmhWMDesktop = "_NET_WM_DESKTOP"
ewmhWMIcon = "_NET_WM_ICON"
ewmhWMName = "_NET_WM_NAME"
ewmhWMName2 = "WM_NAME"
ewmhWMState = "_NET_WM_STATE"
ewmhWMStateHidden = "_NET_WM_STATE_HIDDEN"

allEWMHProperties :: [EWMHProperty]
allEWMHProperties =
  [ ewmhActiveWindow
  , ewmhClientList
  , ewmhClientListStacking
  , ewmhCurrentDesktop
  , ewmhDesktopNames
  , ewmhNumberOfDesktops
  , ewmhStateHidden
  , ewmhWMClass
  , ewmhWMDesktop
  , ewmhWMIcon
  , ewmhWMName
  , ewmhWMName2
  , ewmhWMState
  , ewmhWMStateHidden
  ]

type EWMHIconData = (ForeignPtr PixelsWordType, Int)

data EWMHIcon = EWMHIcon
  { ewmhWidth :: Int
  , ewmhHeight :: Int
  , ewmhPixelsARGB :: Ptr PixelsWordType
  } deriving (Show, Eq)

getWindowStateProperty :: String -> X11Window -> X11Property Bool
getWindowStateProperty property window =
  not . null <$> getWindowState window [property]

getWindowState :: X11Window -> [String] -> X11Property [String]
getWindowState window request = do
  let getAsLong s = fromIntegral <$> getAtom s
  integers <- mapM getAsLong request
  properties <- fetch getWindowProperty32 (Just window) ewmhWMState
  let integerToString = zip integers request
      present = intersect integers $ fromMaybe [] properties
      presentStrings = map (`lookup` integerToString) present
  return $ catMaybes presentStrings

-- | Get a bool reflecting whether window with provided X11Window is minimized
-- or not.
getWindowMinimized :: X11Window -> X11Property Bool
getWindowMinimized = getWindowStateProperty ewmhStateHidden

-- | Retrieve the index of the current workspace in the desktop, starting from
-- 0.
getCurrentWorkspace :: X11Property WorkspaceId
getCurrentWorkspace = WorkspaceId <$> readAsInt Nothing ewmhCurrentDesktop

-- | Retrieve the indexes of all currently visible workspaces
-- with the active workspace at the head of the list.
getVisibleWorkspaces :: X11Property [WorkspaceId]
getVisibleWorkspaces = do
  vis <- getVisibleTags
  allNames <- map swap <$> getWorkspaceNames
  cur <- getCurrentWorkspace
  return $ cur : mapMaybe (`lookup` allNames) vis

-- | Return a list with the names of all the workspaces currently
-- available.
getWorkspaceNames :: X11Property [(WorkspaceId, String)]
getWorkspaceNames = go <$> readAsListOfString Nothing ewmhDesktopNames
  where go = zip [WorkspaceId i | i <- [0..]]

-- | Ask the window manager to switch to the workspace with the given
-- index, starting from 0.
switchToWorkspace :: WorkspaceId -> X11Property ()
switchToWorkspace (WorkspaceId idx) = do
  cmd <- getAtom ewmhCurrentDesktop
  sendCommandEvent cmd (fromIntegral idx)

-- | Move one workspace up or down from the current workspace
switchOneWorkspace :: Bool -> Int -> X11Property ()
switchOneWorkspace dir end = do
  cur <- getCurrentWorkspace
  switchToWorkspace $ if dir then getPrev cur end else getNext cur end

-- | Check for corner case and switch one workspace up
getPrev :: WorkspaceId -> Int -> WorkspaceId
getPrev (WorkspaceId idx) end
  | idx > 0 = WorkspaceId $ idx-1
  | otherwise = WorkspaceId end

-- | Check for corner case and switch one workspace down
getNext :: WorkspaceId -> Int -> WorkspaceId
getNext (WorkspaceId idx) end
  | idx < end = WorkspaceId $ idx+1
  | otherwise = WorkspaceId 0

-- | Get the title of the given X11 window.
getWindowTitle :: X11Window -> X11Property String
getWindowTitle window = do
  let w = Just window
  prop <- readAsString w ewmhWMName
  case prop of
    "" -> readAsString w ewmhWMName2
    _  -> return prop

-- | Get the class of the given X11 window.
getWindowClass :: X11Window -> X11Property String
getWindowClass window = readAsString (Just window) ewmhWMClass

parseWindowClasses :: String -> [String]
parseWindowClasses = filter (not . null) . splitOn "\NUL"

-- | Get EWMHIconData for the given X11Window
getWindowIconsData :: X11Window -> X11Property (Maybe EWMHIconData)
getWindowIconsData window = do
  dpy <- getDisplay
  atom <- getAtom ewmhWMIcon
  lift $ rawGetWindowPropertyBytes 32 dpy atom window

-- | Operate on the data contained in 'EWMHIconData' in the easier to interact
-- with format offered by 'EWMHIcon'. This function is much like
-- 'withForeignPtr' in that the 'EWMHIcon' values that are provided to the
-- callable argument should not be kept around in any way, because it can not be
-- guaranteed that the finalizer for the memory to which those icon objects
-- point will not be executed, after the call to 'withEWMHIcons' completes.
withEWMHIcons :: EWMHIconData -> ([EWMHIcon] -> IO a) -> IO a
withEWMHIcons (fptr, size) action =
  withForeignPtr fptr (parseIcons size >=> action)

-- | Split icon raw integer data into EWMHIcons. Each icon raw data is an
-- integer for width, followed by height, followed by exactly (width*height)
-- ARGB pixels, optionally followed by the next icon.
--
-- XXX: This function should not be made public, because its return value contains
-- (sub)pointers whose allocation we do not control.
parseIcons :: Int -> Ptr PixelsWordType -> IO [EWMHIcon]
parseIcons 0 _ = return []
parseIcons totalSize arr = do
  iwidth <- fromIntegral <$> peek arr
  iheight <- fromIntegral <$> peekElemOff arr 1
  let pixelsPtr = advancePtr arr 2
      thisSize = iwidth * iheight
      newArr = advancePtr pixelsPtr thisSize
      thisIcon =
        EWMHIcon
        { ewmhWidth = iwidth
        , ewmhHeight = iheight
        , ewmhPixelsARGB = pixelsPtr
        }
      getRes newSize
        | newSize < 0 =
          logHere ERROR "Attempt to recurse on negative value in parseIcons"
                    >> return []
        | otherwise = (thisIcon :) <$> parseIcons newSize newArr
  getRes $ totalSize - fromIntegral (thisSize + 2)

-- | Get the window that currently has focus if such a window exists.
getActiveWindow :: X11Property (Maybe X11Window)
getActiveWindow = find (> 0) <$> readAsListOfWindow Nothing ewmhActiveWindow

-- | Return a list of all @X11Window@s, sorted by initial mapping order, oldest to newest.
getWindows :: X11Property [X11Window]
getWindows = readAsListOfWindow Nothing ewmhClientList

-- | Return a list of all @X11Window@s, sorted in stacking order, bottom-to-top.
getWindowsStacking :: X11Property [X11Window]
getWindowsStacking = readAsListOfWindow Nothing ewmhClientListStacking

-- | Return the index (starting from 0) of the workspace on which the given
-- window is being displayed.
getWorkspace :: X11Window -> X11Property WorkspaceId
getWorkspace window = WorkspaceId <$> readAsInt (Just window) ewmhWMDesktop

-- | Ask the window manager to give focus to the given window.
focusWindow :: X11Window -> X11Property ()
focusWindow wh = do
  cmd <- getAtom ewmhActiveWindow
  sendWindowEvent cmd (fromIntegral wh)
