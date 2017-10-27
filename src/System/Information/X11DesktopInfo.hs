{-# LANGUAGE StandaloneDeriving #-}
-----------------------------------------------------------------------------
-- |
-- Module      : System.Information.X11DesktopInfo
-- Copyright   : (c) José A. Romero L.
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : José A. Romero L. <escherdragon@gmail.com>
-- Stability   : unstable
-- Portability : unportable
--
-- Low-level functions to access data provided by the X11 desktop via window
-- properties. One of them ('getVisibleTags') depends on the PagerHints hook
-- being installed in your @~\/.xmonad\/xmonad.hs@ configuration:
--
-- > import System.Taffybar.Hooks.PagerHints (pagerHints)
-- >
-- > main = xmonad $ ewmh $ pagerHints $ ...
--
-----------------------------------------------------------------------------

module System.Information.X11DesktopInfo
  ( X11Context(..)
  , X11Property
  , X11Window
  , withDefaultCtx
  , getDefaultCtx
  , getWindowState
  , getWindowStateProperty
  , readAsInt
  , readAsListOfInt
  , readAsString
  , readAsListOfString
  , readAsListOfWindow
  , isWindowUrgent
  , getVisibleTags
  , getAtom
  , getDisplay
  , eventLoop
  , sendCommandEvent
  , sendWindowEvent
  , postX11RequestSyncProp
  , getPrimaryOutputNumber
  ) where

import Data.List
import Data.Maybe

import Codec.Binary.UTF8.String as UTF8
import Control.Concurrent
import GHC.IO.Exception
import Control.Concurrent.MVar
import Control.Monad.Reader
import Data.Bits (testBit, (.|.))
import Data.Functor ((<$>))
import Data.List.Split (endBy)
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras
       hiding (rawGetWindowProperty, getWindowProperty8,
               getWindowProperty16, getWindowProperty32,
               getWMHints)
import Graphics.X11.Xrandr
import System.Information.SafeX11
import System.IO
import System.Timeout

data X11Context = X11Context { contextDisplay :: Display, _contextRoot :: Window }
type X11Property a = ReaderT X11Context IO a
type X11Window = Window
type PropertyFetcher a = Display -> Atom -> Window -> IO (Maybe [a])

-- | Put the current display and root window objects inside a Reader
-- transformer for further computation.
withDefaultCtx :: X11Property a -> IO a
withDefaultCtx fun = do
  ctx <- getDefaultCtx
  res <- runReaderT fun ctx
  closeDisplay (contextDisplay ctx)
  return res

getDisplay :: X11Property Display
getDisplay = contextDisplay <$> ask

-- | Retrieve the property of the given window (or the root window,
-- if Nothing) with the given name as a value of type Int. If that
-- property hasn't been set, then return -1.
readAsInt :: Maybe X11Window -- ^ window to read from. Nothing means the root window.
          -> String -- ^ name of the property to retrieve
          -> X11Property Int
readAsInt window name = do
  prop <- fetch getWindowProperty32 window name
  case prop of
    Just (x:_) -> return (fromIntegral x)
    _          -> return (-1)

-- | Retrieve the property of the given window (or the root window,
-- if Nothing) with the given name as a list of Ints. If that
-- property hasn't been set, then return an empty list.
readAsListOfInt :: Maybe X11Window -- ^ window to read from. Nothing means the root window.
                -> String          -- ^ name of the property to retrieve
                -> X11Property [Int]
readAsListOfInt window name = do
  prop <- fetch getWindowProperty32 window name
  case prop of
    Just xs -> return (map fromIntegral xs)
    _       -> return []

-- | Retrieve the property of the given window (or the root window,
-- if Nothing) with the given name as a String. If the property
-- hasn't been set, then return an empty string.
readAsString :: Maybe X11Window -- ^ window to read from. Nothing means the root window.
             -> String          -- ^ name of the property to retrieve
             -> X11Property String
readAsString window name = do
  prop <- fetch getWindowProperty8 window name
  case prop of
    Just xs -> return . UTF8.decode . map fromIntegral $ xs
    _       -> return []

-- | Retrieve the property of the given window (or the root window,
-- if Nothing) with the given name as a list of Strings. If the
-- property hasn't been set, then return an empty list.
readAsListOfString :: Maybe X11Window -- ^ window to read from. Nothing means the root window.
                   -> String          -- ^ name of the property to retrieve
                   -> X11Property [String]
readAsListOfString window name = do
  prop <- fetch getWindowProperty8 window name
  case prop of
    Just xs -> return (parse xs)
    _       -> return []
  where
    parse = endBy "\0" . UTF8.decode . map fromIntegral

-- | Retrieve the property of the given window (or the root window,
-- if Nothing) with the given name as a list of X11 Window IDs. If
-- the property hasn't been set, then return an empty list.
readAsListOfWindow :: Maybe X11Window -- ^ window to read from. Nothing means the root window.
                   -> String          -- ^ name of the property to retrieve
                   -> X11Property [X11Window]
readAsListOfWindow window name = do
  prop <- fetch getWindowProperty32 window name
  case prop of
    Just xs -> return $ map fromIntegral xs
    _       -> return []

-- | Determine whether the \"urgent\" flag is set in the WM_HINTS of
-- the given window.
isWindowUrgent :: X11Window -> X11Property Bool
isWindowUrgent window = do
  hints <- fetchWindowHints window
  return $ testBit (wmh_flags hints) urgencyHintBit

-- | Retrieve the value of the special _XMONAD_VISIBLE_WORKSPACES hint set
-- by the PagerHints hook provided by Taffybar (see module documentation for
-- instructions on how to do this), or an empty list of strings if the
-- PagerHints hook is not available.
getVisibleTags :: X11Property [String]
getVisibleTags = readAsListOfString Nothing "_XMONAD_VISIBLE_WORKSPACES"

-- | Return the Atom with the given name.
getAtom :: String -> X11Property Atom
getAtom s = do
  (X11Context d _) <- ask
  liftIO $ internAtom d s False

-- | Spawn a new thread and listen inside it to all incoming events,
-- invoking the given function to every event of type @MapNotifyEvent@ that
-- arrives, and subscribing to all events of this type emitted by newly
-- created windows.
eventLoop :: (Event -> IO ()) -> X11Property ()
eventLoop dispatch = do
  (X11Context d w) <- ask
  liftIO $ do
    selectInput d w $ propertyChangeMask .|. substructureNotifyMask
    allocaXEvent $ \e -> forever $ do
      event <- nextEvent d e >> getEvent e
      case event of
        MapNotifyEvent _ _ _ _ _ window _ ->
          selectInput d window propertyChangeMask
        _ -> return ()
      dispatch event

-- | Emit a \"command\" event with one argument for the X server. This is
-- used to send events that can be received by event hooks in the XMonad
-- process and acted upon in that context.
sendCommandEvent :: Atom -> Atom -> X11Property ()
sendCommandEvent cmd arg = do
  (X11Context dpy root) <- ask
  sendCustomEvent dpy cmd arg root root

-- | Similar to 'sendCommandEvent', but with an argument of type Window.
sendWindowEvent :: Atom -> X11Window -> X11Property ()
sendWindowEvent cmd win = do
  (X11Context dpy root) <- ask
  sendCustomEvent dpy cmd cmd root win

-- | Build a new X11Context containing the current X11 display and its root
-- window.
getDefaultCtx :: IO X11Context
getDefaultCtx = do
  d <- openDisplay ""
  w <- rootWindow d $ defaultScreen d
  return $ X11Context d w

getWindowStateProperty :: X11Window -> String -> X11Property Bool
getWindowStateProperty window property = not . null <$> getWindowState window [property]

getWindowState :: X11Window -> [String] -> X11Property [String]
getWindowState window request = do
  let getAsLong s = fromIntegral <$> getAtom s
  integers <- mapM getAsLong request
  properties <- fetch getWindowProperty32 (Just window) "_NET_WM_STATE"
  let integerToString = zip integers request
      present = intersect integers $ fromMaybe [] properties
      presentStrings = map (`lookup` integerToString) present
  return $ catMaybes presentStrings

-- | Apply the given function to the given window in order to obtain the X11
-- property with the given name, or Nothing if no such property can be read.
fetch :: (Integral a)
      => PropertyFetcher a -- ^ Function to use to retrieve the property.
      -> Maybe X11Window   -- ^ Window to read from. Nothing means the root Window.
      -> String            -- ^ Name of the property to retrieve.
      -> X11Property (Maybe [a])
fetch fetcher window name = do
  (X11Context dpy root) <- ask
  atom <- getAtom name
  liftIO $ fetcher dpy atom (fromMaybe root window)

-- | Retrieve the @WM_HINTS@ mask assigned by the X server to the given window.
fetchWindowHints :: X11Window -> X11Property WMHints
fetchWindowHints window = do
  (X11Context d _) <- ask
  liftIO $ getWMHints d window

-- | Emit an event of type @ClientMessage@ that can be listened to and
-- consumed by XMonad event hooks.
sendCustomEvent :: Display
                -> Atom
                -> Atom
                -> X11Window
                -> X11Window
                -> X11Property ()
sendCustomEvent dpy cmd arg root win =
  liftIO $ allocaXEvent $ \e -> do
    setEventType e clientMessage
    setClientMessageEvent e win cmd 32 arg currentTime
    sendEvent dpy root False structureNotifyMask e
    sync dpy False

postX11RequestSyncProp :: X11Property a -> a -> X11Property a
postX11RequestSyncProp prop def = do
  c <- ask
  let action = runReaderT prop c
  lift $ postX11RequestSyncDef def action

isActiveOutput :: XRRScreenResources -> RROutput -> X11Property Bool
isActiveOutput sres output = do
  (X11Context display _) <- ask
  maybeOutputInfo <- liftIO $ xrrGetOutputInfo display sres output
  return $ maybe 0 xrr_oi_crtc maybeOutputInfo /= 0

getActiveOutputs :: X11Property [RROutput]
getActiveOutputs = do
  (X11Context display rootw) <- ask
  maybeSres <- liftIO $ xrrGetScreenResources display rootw
  case maybeSres of
    Just sres ->
      filterM (isActiveOutput sres) $ xrr_sr_outputs sres
    otherwise ->
      return []

-- | Get the index of the primary monitor as set and ordered by Xrandr.
getPrimaryOutputNumber :: X11Property (Maybe Int)
getPrimaryOutputNumber = do
  (X11Context display rootw) <- ask
  primary <- liftIO $ xrrGetOutputPrimary display rootw
  outputs <- getActiveOutputs
  return $ primary `elemIndex` outputs
