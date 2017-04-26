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
  , readAsInt
  , readAsListOfInt
  , readAsString
  , readAsListOfString
  , readAsListOfWindow
  , isWindowUrgent
  , getVisibleTags
  , getAtom
  , eventLoop
  , sendCommandEvent
  , sendWindowEvent
  ) where

import Codec.Binary.UTF8.String as UTF8
import Control.Monad.Reader
import Data.Bits (testBit, (.|.))
import Data.List.Split (endBy)
import Data.Maybe (fromMaybe)
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras

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
getVisibleTags = return =<<
  readAsListOfString Nothing "_XMONAD_VISIBLE_WORKSPACES"

-- | Return the Atom with the given name.
getAtom :: String -> X11Property Atom
getAtom s = do
  (X11Context d _) <- ask
  atom <- liftIO $ internAtom d s False
  return atom

-- | Spawn a new thread and listen inside it to all incoming events,
-- invoking the given function to every event of type @MapNotifyEvent@ that
-- arrives, and subscribing to all events of this type emitted by newly
-- created windows.
eventLoop :: (Event -> IO ()) -> X11Property ()
eventLoop dispatch = do
  (X11Context d w) <- ask
  liftIO $ do
    xSetErrorHandler
    selectInput d w $ propertyChangeMask .|. substructureNotifyMask
    allocaXEvent $ \e -> forever $ do
      event <- nextEvent d e >> getEvent e
      case event of
        MapNotifyEvent _ _ _ _ _ window _ -> do
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
  prop <- liftIO $ fetcher dpy atom (fromMaybe root window)
  return prop

-- | Retrieve the @WM_HINTS@ mask assigned by the X server to the given window.
fetchWindowHints :: X11Window -> X11Property WMHints
fetchWindowHints window = do
  (X11Context d _) <- ask
  hints <- liftIO $ getWMHints d window
  return hints

-- | Emit an event of type @ClientMessage@ that can be listened to and
-- consumed by XMonad event hooks.
sendCustomEvent :: Display
                -> Atom
                -> Atom
                -> X11Window
                -> X11Window
                -> X11Property ()
sendCustomEvent dpy cmd arg root win = do
  liftIO $ allocaXEvent $ \e -> do
    setEventType e clientMessage
    setClientMessageEvent e win cmd 32 arg currentTime
    sendEvent dpy root False structureNotifyMask e
    sync dpy False
