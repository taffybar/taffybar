-----------------------------------------------------------------------------
-- |
-- Module      : System.Taffybar.Information.X11DesktopInfo
-- Copyright   : (c) José A. Romero L.
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan Malison <IvanMalison@gmail.com>
-- Stability   : unstable
-- Portability : unportable
--
-- Low-level functions to access data provided by the X11 desktop via window
-- properties. One of them ('getVisibleTags') depends on the
-- 'XMonad.Hooks.TaffybarPagerHints.pagerHints' hook
-- being installed in your @~\/.xmonad\/xmonad.hs@ configuration:
--
-- > import XMonad.Hooks.TaffybarPagerHints (pagerHints)
-- >
-- > main = xmonad $ ewmh $ pagerHints $ ...
--
-----------------------------------------------------------------------------

module System.Taffybar.Information.X11DesktopInfo
  ( -- * Context
    X11Context(..)
  , getDefaultCtx
  , withDefaultCtx

  -- * Properties
  , X11Property

  -- ** Event loop
  , eventLoop

  -- ** Context getters
  , getDisplay
  , getAtom

  -- ** Basic properties of windows
  , X11Window
  , PropertyFetcher
  , fetch
  , readAsInt
  , readAsListOfInt
  , readAsListOfString
  , readAsListOfWindow
  , readAsString

  -- ** Getters
  , isWindowUrgent
  , getPrimaryOutputNumber
  , getVisibleTags

  -- ** Operations
  , doLowerWindow
  , postX11RequestSyncProp
  , sendCommandEvent
  , sendWindowEvent
  ) where

import Codec.Binary.UTF8.String as UTF8
import qualified Control.Concurrent.MVar as MV
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.Bits (testBit, (.|.))
import Data.List (elemIndex)
import Data.List.Split (endBy)
import Data.Maybe (fromMaybe, listToMaybe)
import Graphics.X11.Xrandr (XRRScreenResources(..), XRROutputInfo(..), xrrGetOutputInfo, xrrGetScreenResources, xrrGetOutputPrimary)
import System.Taffybar.Information.SafeX11 hiding (displayName)

-- | Represents a connection to an X11 display.
-- Use 'getDefaultCtx' to construct one of these.
data X11Context = X11Context
  { contextDisplay :: Display
  , _contextRoot :: Window
  , atomCache :: MV.MVar [(String, Atom)]
  }

-- | 'IO' actions with access to an 'X11Context'.
type X11Property a = ReaderT X11Context IO a
type X11Window = Window
type PropertyFetcher a = Display -> Atom -> X11Window -> IO (Maybe [a])

-- | Makes a connection to the default X11 display using
-- 'getDefaultCtx' and puts the current display and root window
-- objects inside a 'ReaderT' transformer for further computation.
withDefaultCtx :: X11Property a -> IO a
withDefaultCtx fun = do
  ctx <- getDefaultCtx
  res <- runReaderT fun ctx
  closeDisplay (contextDisplay ctx)
  return res

-- | An X11Property that returns the 'Display' object stored in the
-- 'X11Context'.
getDisplay :: X11Property Display
getDisplay = contextDisplay <$> ask

doRead :: Integral a => b -> ([a] -> b)
       -> PropertyFetcher a
       -> Maybe X11Window
       -> String
       -> X11Property b
doRead def transform windowPropFn window name =
  maybe def transform <$> fetch windowPropFn window name

-- | Retrieve the property of the given window (or the root window, if Nothing)
-- with the given name as a value of type Int. If that property hasn't been set,
-- then return -1.
readAsInt :: Maybe X11Window -- ^ window to read from. Nothing means the root window.
          -> String -- ^ name of the property to retrieve
          -> X11Property Int
readAsInt = doRead (-1) (maybe (-1) fromIntegral . listToMaybe) getWindowProperty32

-- | Retrieve the property of the given window (or the root window, if Nothing)
-- with the given name as a list of Ints. If that property hasn't been set, then
-- return an empty list.
readAsListOfInt :: Maybe X11Window -- ^ window to read from. Nothing means the root window.
                -> String          -- ^ name of the property to retrieve
                -> X11Property [Int]
readAsListOfInt = doRead [] (map fromIntegral) getWindowProperty32

-- | Retrieve the property of the given window (or the root window, if Nothing)
-- with the given name as a String. If the property hasn't been set, then return
-- an empty string.
readAsString :: Maybe X11Window -- ^ window to read from. Nothing means the root window.
             -> String          -- ^ name of the property to retrieve
             -> X11Property String
readAsString = doRead "" (UTF8.decode . map fromIntegral) getWindowProperty8

-- | Retrieve the property of the given window (or the root window, if Nothing)
-- with the given name as a list of Strings. If the property hasn't been set,
-- then return an empty list.
readAsListOfString :: Maybe X11Window -- ^ window to read from. Nothing means the root window.
                   -> String          -- ^ name of the property to retrieve
                   -> X11Property [String]
readAsListOfString = doRead [] parse getWindowProperty8
  where parse = endBy "\0" . UTF8.decode . map fromIntegral

-- | Retrieve the property of the given window (or the root window, if Nothing)
-- with the given name as a list of X11 Window IDs. If the property hasn't been
-- set, then return an empty list.
readAsListOfWindow :: Maybe X11Window -- ^ window to read from. Nothing means the root window.
                   -> String          -- ^ name of the property to retrieve
                   -> X11Property [X11Window]
readAsListOfWindow = doRead [] (map fromIntegral) getWindowProperty32

-- | Determine whether the \"urgent\" flag is set in the WM_HINTS of the given
-- window.
isWindowUrgent :: X11Window -> X11Property Bool
isWindowUrgent window = do
  hints <- fetchWindowHints window
  return $ testBit (wmh_flags hints) urgencyHintBit

-- | Retrieve the value of the special @_XMONAD_VISIBLE_WORKSPACES@
-- hint set by the 'XMonad.Hooks.TaffybarPagerHints.pagerHints' hook
-- provided by [xmonad-contrib]("XMonad.Hooks.TaffybarPagerHints")
-- (see module documentation for instructions on how to do this), or
-- an empty list of strings if the @pagerHints@ hook is not available.
getVisibleTags :: X11Property [String]
getVisibleTags = readAsListOfString Nothing "_XMONAD_VISIBLE_WORKSPACES"

-- | Return the 'Atom' with the given name.
getAtom :: String -> X11Property Atom
getAtom s = do
  (X11Context d _ cacheVar) <- ask
  a <- lift $ lookup s <$> MV.readMVar cacheVar
  let updateCacheAction = lift $ MV.modifyMVar cacheVar updateCache
      updateCache currentCache =
        do
          atom <- internAtom d s False
          return ((s, atom):currentCache, atom)
  maybe updateCacheAction return a

-- | Spawn a new thread and listen inside it to all incoming events, invoking
-- the given function to every event of type @MapNotifyEvent@ that arrives, and
-- subscribing to all events of this type emitted by newly created windows.
eventLoop :: (Event -> IO ()) -> X11Property ()
eventLoop dispatch = do
  (X11Context d w _) <- ask
  liftIO $ do
    selectInput d w $ propertyChangeMask .|. substructureNotifyMask
    allocaXEvent $ \e -> forever $ do
      event <- nextEvent d e >> getEvent e
      case event of
        MapNotifyEvent { ev_window = window } ->
          selectInput d window propertyChangeMask
        _ -> return ()
      dispatch event

-- | Emit a \"command\" event with one argument for the X server. This is used
-- to send events that can be received by event hooks in the XMonad process and
-- acted upon in that context.
sendCommandEvent :: Atom -> Atom -> X11Property ()
sendCommandEvent cmd arg = do
  (X11Context dpy root _) <- ask
  sendCustomEvent dpy cmd arg root root

-- | Similar to 'sendCommandEvent', but with an argument of type 'X11Window'.
sendWindowEvent :: Atom -> X11Window -> X11Property ()
sendWindowEvent cmd win = do
  (X11Context dpy root _) <- ask
  sendCustomEvent dpy cmd cmd root win

-- | Builds a new 'X11Context' containing a connection to the default
-- X11 display and its root window.
--
-- If the X11 connection could not be opened, it will throw
-- @'Control.Exception.userError' "openDisplay"@. This can occur if the
-- @X -maxclients@ limit has been exceeded.
getDefaultCtx :: IO X11Context
getDefaultCtx = do
  d <- openDisplay ""
  w <- rootWindow d $ defaultScreen d
  cache <- MV.newMVar []
  return $ X11Context d w cache

-- | Apply the given function to the given window in order to obtain the X11
-- property with the given name, or Nothing if no such property can be read.
fetch :: (Integral a)
      => PropertyFetcher a -- ^ Function to use to retrieve the property.
      -> Maybe X11Window   -- ^ Window to read from. Nothing means the root Window.
      -> String            -- ^ Name of the property to retrieve.
      -> X11Property (Maybe [a])
fetch fetcher window name = do
  (X11Context dpy root _) <- ask
  atom <- getAtom name
  liftIO $ fetcher dpy atom (fromMaybe root window)

-- | Retrieve the @WM_HINTS@ mask assigned by the X server to the given window.
fetchWindowHints :: X11Window -> X11Property WMHints
fetchWindowHints window = do
  (X11Context d _ _) <- ask
  liftIO $ getWMHints d window

-- | Emit an event of type @ClientMessage@ that can be listened to and consumed
-- by XMonad event hooks.
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

-- | Post the provided X11Property to taffybar's dedicated X11 thread, and wait
-- for the result. The provided default value will be returned in the case of an
-- error.
postX11RequestSyncProp :: X11Property a -> a -> X11Property a
postX11RequestSyncProp prop def = do
  c <- ask
  let action = runReaderT prop c
  lift $ postX11RequestSyncDef def action

-- | 'X11Property' which reflects whether or not the provided 'RROutput' is active.
isActiveOutput :: XRRScreenResources -> RROutput -> X11Property Bool
isActiveOutput sres output = do
  (X11Context display _ _) <- ask
  maybeOutputInfo <- liftIO $ xrrGetOutputInfo display sres output
  return $ maybe 0 xrr_oi_crtc maybeOutputInfo /= 0

-- | Return all the active RANDR outputs.
getActiveOutputs :: X11Property [RROutput]
getActiveOutputs = do
  (X11Context display rootw _) <- ask
  maybeSres <- liftIO $ xrrGetScreenResources display rootw
  maybe (return []) (\sres -> filterM (isActiveOutput sres) $ xrr_sr_outputs sres)
        maybeSres

-- | Get the index of the primary monitor as set and ordered by Xrandr.
getPrimaryOutputNumber :: X11Property (Maybe Int)
getPrimaryOutputNumber = do
  (X11Context display rootw _) <- ask
  primary <- liftIO $ xrrGetOutputPrimary display rootw
  outputs <- getActiveOutputs
  return $ primary `elemIndex` outputs

-- | Move the given 'X11Window' to the bottom of the X11 window stack.
doLowerWindow :: X11Window -> X11Property ()
doLowerWindow window =
  asks contextDisplay >>= lift . flip lowerWindow window
