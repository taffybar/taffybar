{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-----------------------------------------------------------------------------
-- |
-- Module      : System.Taffybar.Information.X11DesktopInfo
-- Description : Access X properties and respond to X events
-- Copyright   : (c) José A. Romero L.
--                   Rodney Lorrimar, 2024
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan Malison <IvanMalison@gmail.com>
-- Stability   : unstable
-- Portability : GHC
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
    X11Context
  , DisplayName(..)
  , withX11Context
  , OpenDisplayException(..)

  -- * Properties
  , X11Property
  , X11PropertyT
  , runSafeX11

  -- ** Context getters
  , rootWindow
  , Atom
  , getAtom
  , getCachedAtomName
  , getCachedAtomNames
  , unsafeGetDisplay

  -- ** Reading properties of windows
  , X11Window
  , PropertyFetcher
  , basicFetch
  , fallback
  , fetch
  , readAsString
  , readAsListOfString
  , readAsInt
  , readAsListOfInt
  , readAsListOfWindow

  -- ** Getters
  , isWindowUrgent
  , getPrimaryOutputNumber
  , getVisibleTags

  -- ** Operations
  , doLowerWindow
  , sendCommandEvent
  , sendWindowEvent

  -- * Event loop
  , withX11EventLoop
  , eventLoop
  , Event(PropertyEvent, ClientMessageEvent, ConfigureEvent, MapNotifyEvent, ev_window, ev_atom, ev_data, ev_message_type, ev_event_type)
  ) where

import qualified Codec.Binary.UTF8.String as UTF8
import Control.Monad
import Control.Monad.IO.Unlift (MonadUnliftIO(..), MonadIO(..))
import Control.Monad.Trans.Reader
import Data.Bits (testBit, (.|.))
import Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as HM
import Data.List (elemIndex)
import Data.List.Split (endBy)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Typeable (Typeable)
import GHC.Stack (HasCallStack)
import Graphics.X11.Xrandr (XRRScreenResources(..), XRROutputInfo(..), xrrGetOutputInfo, xrrGetScreenResources, xrrGetOutputPrimary)
import System.Log.Logger (Priority (..), logM)
import System.Taffybar.Information.SafeX11
import System.Taffybar.Information.ResourceThread (unsafeGetResource)
import UnliftIO.Exception (Exception(..), bracket_, displayException, catches, Handler (..))
import qualified UnliftIO.MVar as MV
import UnliftIO.Async (withAsync)

-- | Represents a connection to an X11 display.
-- Use 'withX11Context' to construct one of these.
data X11Context = X11Context
  { ctxDisplayName :: DisplayName
  , ctxRoot :: X11Window
  , ctxAtomCache :: (String -> IO Atom, [Atom] -> IO [(Atom, String)])
  , ctxThread :: SafeX11Thread
  }

-- | A 'ReaderT' with 'X11Context'.
type X11PropertyT m a = ReaderT X11Context m a
-- | 'IO' actions with access to an 'X11Context'.
type X11Property a = X11PropertyT IO a

-- | Encapsulates some XLib call which gets a named property from a window.
type PropertyFetcher r = Atom -> X11Window -> SafeX11 r

-- | Connects to the X11 specified by 'DisplayName', then constructs
-- an 'X11Context' and applies it to the given action.
--
-- The 'X11Context' can be used with 'runReaderT' to run
-- 'X11PropertyT' actions.
--
-- The 'Display' connection is managed by a background thread
-- ('withSafeX11Thread'). When the given action returns, this thread
-- will be killed and the 'Display' connection will be closed.
withX11Context
  :: (HasCallStack, MonadUnliftIO m)
  => DisplayName -- ^ Display name.
  -> (X11Context -> m a) -- ^ Action to run.
  -> m a
withX11Context dn a =
  logAround DEBUG "withX11Context" $
  withSafeX11Thread dn (mkX11Context dn >=> a)

mkX11Context :: MonadIO m => DisplayName -> SafeX11Thread -> m X11Context
mkX11Context ctxDisplayName ctxThread = do
  ctxAtomCache <- liftIO $
    makeCache (flip runSafeX11Sync ctxThread . safeInternAtom)
  ctxRoot <- liftIO $ runSafeX11Sync safeRootWindow ctxThread
  return X11Context{..}

makeCache
  :: (Eq k, Hashable k, Eq v, Hashable v, MonadUnliftIO m)
  => (k -> m v) -- ^ Function whose results should be cached.
  -> m (k -> m v, [v] -> m [(v, k)]) -- ^ (Lookup, cache reverse lookup).
makeCache f = do
  var <- MV.newMVar (HM.empty, HM.empty)
  let cacheLookup k = HM.lookup k . fst <$> MV.readMVar var
      cachePut k = MV.modifyMVar var $ \(cache, rcache) -> do
        v <- f k
        pure ((HM.insert k v cache, HM.insert v k rcache), v)
      getKey k = maybe (cachePut k) pure =<< cacheLookup k
      getValues vs = do
        rcache <- snd <$> MV.readMVar var
        pure [ (v, k) | v <- vs, Just k <- [HM.lookup v rcache] ]

  pure (getKey, getValues)

-- | Apply the given function to the given window in order to obtain the X11
-- property with the given name, or 'Nothing' if no such property can be read.
basicFetch
  :: (HasCallStack, MonadUnliftIO m)
  => PropertyFetcher r -- ^ Function to use to retrieve the property.
  -> String            -- ^ Name of the property to retrieve.
  -> Maybe X11Window   -- ^ 'Just' an 'X11Window' to read from. 'Nothing' means the 'rootWindow'.
  -> X11PropertyT m r
basicFetch fetcher name window = runSafeX11 =<<
  fetcher <$> getAtom name <*> getWindowOrElseRoot window

data FetchError = FetchReqError SafeX11Exception
                | FetchPropError GetWindowPropertyException
                deriving (Show, Typeable)
instance Exception FetchError

-- | The provided default value will be returned in the case of a
-- 'SafeX11Exception' or 'GetWindowPropertyException'.
fallback :: forall m a. MonadUnliftIO m => X11PropertyT m a -> a -> X11PropertyT m a
fallback f dflt = catches f
  [ Handler $ handler @SafeX11Exception
  , Handler propHandler
  , Handler $ handler @WindowPropertyError
  ]
  where
    propHandler :: GetWindowPropertyException -> X11PropertyT m a
    propHandler e = do
      name <- getCachedAtomName (getWindowPropertyAtom e)
      let e' = e { getWindowPropertyName = name }
      handler e'
    handler :: Exception e => e -> X11PropertyT m a
    handler e = do
      logHere DEBUG ("fallback: " ++ displayException e)
      pure dflt

-- | Combine 'basicFetch' together with 'fmap' and 'fallback' on the
-- provided property. Return the provided default in the event of
-- failure.
fetch
  :: (HasCallStack, MonadUnliftIO m)
  => b -- ^ Default value when fetching fails.
  -> ([a] -> b) -- ^ Transform the fetched value array.
  -> PropertyFetcher [a] -- ^ Function to get value array (as a list).
  -> String -- ^ Name of the property to retrieve.
  -> Maybe X11Window   -- ^ 'Just' an 'X11Window' to read from. 'Nothing' means the 'rootWindow'.
  -> X11PropertyT m b
fetch dflt transform fetcher prop win =
  fmap transform (basicFetch fetcher prop win)
  `fallback` dflt

-- | Retrieve the property of the given window with the given name as
-- a value of type 'Int'. If that property hasn't been set, then
-- 'Nothing' is returned.
readAsInt
  :: HasCallStack
  => String -- ^ name of the property to retrieve
  -> Maybe X11Window -- ^ 'Just' an 'X11Window to read from. 'Nothing' means the 'rootWindow'.
  -> X11PropertyT IO (Maybe Int)
readAsInt = fetch Nothing (listToMaybe . map fromIntegral) getWindowProperty32

-- | Retrieve the property of the given window (or the 'rootWindow', if 'Nothing')
-- with the given name as a list of 'Int'. If that property hasn't been set, then
-- return an empty list.
readAsListOfInt
  :: HasCallStack
  => String          -- ^ name of the property to retrieve
  -> Maybe X11Window -- ^ window to read from. Nothing means the root window.
  -> X11Property [Int]
readAsListOfInt = fetch [] (map fromIntegral) getWindowProperty32

-- | Retrieve the property of the given window (or the root window, if Nothing)
-- with the given name as a String. If the property hasn't been set, then return
-- an empty string.
readAsString
  :: HasCallStack
  => String          -- ^ name of the property to retrieve
  -> Maybe X11Window -- ^ window to read from. Nothing means the root window.
  -> X11Property String
readAsString = fetch "" (UTF8.decode . map fromIntegral) getWindowProperty8

-- | Retrieve the property of the given window (or the root window, if Nothing)
-- with the given name as a list of Strings. If the property hasn't been set,
-- then return an empty list.
readAsListOfString
  :: HasCallStack
  => String          -- ^ name of the property to retrieve
  -> Maybe X11Window -- ^ window to read from. Nothing means the root window.
  -> X11Property [String]
readAsListOfString = fetch [] parse getWindowProperty8
  where parse = endBy "\0" . UTF8.decode . map fromIntegral

-- | Retrieve the property of the given window (or the root window, if Nothing)
-- with the given name as a list of X11 Window IDs. If the property hasn't been
-- set, then return an empty list.
readAsListOfWindow
  :: HasCallStack
  => String          -- ^ name of the property to retrieve
  -> Maybe X11Window -- ^ window to read from. Nothing means the root window.
  -> X11Property [X11Window]
readAsListOfWindow = fetch [] (map fromIntegral) getWindowProperty32

-- | Determine whether the \"urgent\" flag is set in the @WM_HINTS@ of
-- the given window.
isWindowUrgent :: HasCallStack => X11Window -> X11Property Bool
isWindowUrgent window = do
  hints <- fetchWindowHints window
  return $ testBit (wmh_flags hints) urgencyHintBit

-- | Retrieve the value of the special @_XMONAD_VISIBLE_WORKSPACES@
-- hint set by the 'XMonad.Hooks.TaffybarPagerHints.pagerHints' hook
-- provided by [xmonad-contrib]("XMonad.Hooks.TaffybarPagerHints")
-- (see module documentation for instructions on how to do this), or
-- an empty list of strings if the @pagerHints@ hook is not available.
getVisibleTags :: HasCallStack => X11Property [String]
getVisibleTags = readAsListOfString "_XMONAD_VISIBLE_WORKSPACES" Nothing

-- | Return the 'Atom' with the given name. Results are cached in the
-- 'X11Context'.
getAtom :: (HasCallStack, MonadUnliftIO m) => String -> X11PropertyT m Atom
getAtom s = liftIO =<< asks (($ s) . fst . ctxAtomCache)

safeInternAtom :: String -> SafeX11 Atom
safeInternAtom a = mkSafe2 internAtom a False

-- | Try to find the name of the given 'Atom', if it has been
-- previously cached via 'getAtom'.
getCachedAtomName :: (HasCallStack, MonadUnliftIO m) => Atom -> X11PropertyT m (Maybe String)
getCachedAtomName a = listToMaybe . map snd <$> getCachedAtomNames [a]

-- | Look up previously cached 'Atom's in the 'X11Context'. Known
-- names are returned, paired with their 'Atom'.
getCachedAtomNames :: (HasCallStack, MonadUnliftIO m) => [Atom] -> X11PropertyT m [(Atom, String)]
getCachedAtomNames a = liftIO =<< asks (($ a) . snd . ctxAtomCache)

-- | Starts an XLib event loop which listens to incoming events of:
--
--   * 'propertyChangeMask' → 'PropertyEvent'
--   * 'substructureNotifyMask' → 'MapNotifyEvent', 'ConfigureEvent'
--   * 'structureNotifyMask' → 'ClientMessageEvent'
--
-- The given handler is invoked for each
-- 'Graphics.X11.Xlib.Extras.Event' received.
--
-- When an event of type 'MapNotifyEvent' is
-- emitted by a newly created window, 'eventLoop' will also listen for
-- events matching 'propertyChangeMask' on that window.
--
-- This function uses 'withSafeX11Thread' to make a new connection to
-- the X display.
eventLoop
  :: (HasCallStack, MonadUnliftIO m)
  => DisplayName
  -> m () -- ^ Ready notification.
  -> (Event -> m ()) -- ^ Event handler.
  -> m a -- ^ Never returns.
eventLoop dn ready dispatch = logAround DEBUG "eventLoop" $ withRunInIO $ \runInIO -> do
  withSafeX11Thread dn $ \ctx -> logAround DEBUG "eventLoop_inner" $ do
    let
      x11 :: SafeX11 r -> IO r
      x11 = flip runSafeX11Sync ctx

      mask = propertyChangeMask .|. substructureNotifyMask .|. structureNotifyMask

    root <- x11 safeRootWindow
    x11 $ mkSafe2 selectInput root mask
    x11 $ mkSafe1 sync False
    runInIO ready
    allocaXEvent $ \xe -> forever $ do
      x11 $ interruptibleNextEvent dn xe
      event <- getEvent xe
      case event of
        MapNotifyEvent { ev_window = window } ->
          x11 $ mkSafe2 selectInput window propertyChangeMask
        _ -> return ()
      runInIO (dispatch event)

-- | Block until an 'Event' arrives on the given X11 'Display',
-- Special hacks added are added so that asynchronous
-- exceptions cause it to promptly unblock.
--
-- It's important that 'nextEvent' can be cancelled, so that Taffybar
-- itself can be stopped, reloaded, and restarted in the same process,
-- without leaking resources or being crashed by XLib.
--
-- Under normal operation, 'nextEvent' returns after an event arrives.
-- If no event arrives, 'nextEvent' never returns.
--
-- Being a FFI call, 'nextEvent' is not interruptible.
--
-- (Even if the @InterruptibleFFI@ extension is used, it's still
-- not interruptible. 'XNextEvent' seems to be resistant to the
-- [@SIGPIPE@ method](https://downloads.haskell.org/ghc/9.6.6/docs/users_guide/exts/ffi.html#interruptible-foreign-calls)
-- of interrupting FFI calls.)
--
-- Since 'nextEvent' runs in the 'SafeX11' thread, 'eventLoop' is not
-- actually blocked on a FFI call. It is just blocked waiting for an
-- MVar to receive the result of a FFI. So we can handle asynchronous
-- exceptions here, and run an \"unblocker\" to encourage 'nextEvent'
-- to return.
--
-- The unblocker first cancels the 'SafeX11' thread and waits up to
-- 200ms for it to exit.
--
-- If the thread hasn't exited, then the unblocker opens a /new
-- connection/ to the display, and sends a dummy X event known to
-- match the event mask which 'nextEvent' is waiting for.
--
-- After this, we allow another 1000ms for 'nextEvent' to return. If
-- 'nextEvent' is still blocked now, then it will probably never
-- unblock. So the unblocker throws an exception and returns control
-- to the caller.
--
-- Waiting for 'nextEvent' to actually return is most useful for unit
-- tests, where 'eventLoop' will be run repeatedly. We don't want
-- 'nextEvent' threads to pile up until the X server connection limit
-- is reached.
--
-- Also in unit tests, we don't want to stop @Xvfb@ until we have
-- closed our 'Display' connection. And we don't want to call
-- 'closeDisplay' while 'nextEvent' is still in progress. In either of
-- these cases, XLib will crash the process.
interruptibleNextEvent
  :: HasCallStack
  => DisplayName -- ^ For making a new 'Display' connection.
  -> XEventPtr -- ^ Pre-allocated destination for event.
  -> SafeX11 () -- ^ Blocks until an event arrives.
interruptibleNextEvent dn = makeInterruptible . safeNextEvent
  where
    makeInterruptible = setSafeX11Params SafeX11Params
      { safeX11TimeoutUsec = Nothing
      , safeX11UseErrorHandlers = True
      , safeX11Unblocker = Just (const nextEventUnblocker)
      }

    -- | Send a dummy 'ClientMessageEvent' event to root window in order
    -- to unblock 'nextEvent'.
    --
    -- Open a new 'Display' connection to do this because we shouldn't use
    -- the same display connection concurrently.
    nextEventUnblocker = logAround DEBUG "nextEventUnblocker" $
      withSafeX11Thread dn $ \ctx -> flip runSafeX11Sync ctx $ do
        root <- safeRootWindow
        safeSendClientMessageEvent 0 0 root Nothing
        mkSafe1 sync False -- very important

safeNextEvent
  :: HasCallStack
  => XEventPtr -- ^ Pre-allocated destination for event.
  -> SafeX11 () -- ^ Blocks until an event arrives.
safeNextEvent xe = mkSafe $ \d -> logAround DEBUG "safeNextEvent" $ nextEvent d xe

-- | Forks an 'eventLoop' thread in the background, handling X11
-- events, while running the given action.
--
-- Events will be dispatched synchronously in the 'eventLoop' thread
-- to the given handler function.
--
-- NB: The event loop needs its own 'SafeX11Thread' to separately
-- handle communications from the X server. It opens a new connection
-- to the given X11 display and runs an event-handling loop in a
-- dedicated thread. The 'Display' connection won't be shared with
-- other threads.
withX11EventLoop
  :: MonadUnliftIO m
  => DisplayName -- ^ Open connection to this X11 display.
  -> (Event -> m ()) -- ^ Event handler.
  -> m a -- ^ Main action.
  -> m a
withX11EventLoop dn dispatch action = logAround DEBUG "withX11EventLoop" $ do
  ready <- MV.newEmptyMVar
  let eventLoop' = eventLoop dn (MV.putMVar ready ()) dispatch
      action' = MV.takeMVar ready >> action
  withAsync eventLoop' (const action')

-- | Emit a \"command\" event with one argument for the X server. This is used
-- to send events that can be received by event hooks in the XMonad process and
-- acted upon in that context.
sendCommandEvent :: MonadIO m => Atom -> Atom -> X11PropertyT m ()
sendCommandEvent cmd arg = sendCustomEvent cmd arg Nothing

-- | Similar to 'sendCommandEvent', but with an argument of type 'X11Window'.
sendWindowEvent :: Atom -> X11Window -> X11Property ()
sendWindowEvent cmd win = sendCustomEvent cmd cmd (Just win)

-- | The root window XID is cached by 'X11Context' after opening the
-- 'Display' connection.
rootWindow :: X11Context -> X11Window
rootWindow = ctxRoot

getWindowOrElseRoot :: Monad m => Maybe X11Window -> X11PropertyT m X11Window
getWindowOrElseRoot window = asks (flip fromMaybe window . rootWindow)

-- | Retrieve the @WM_HINTS@ mask assigned by the X server to the given window.
fetchWindowHints :: X11Window -> X11Property WMHints
fetchWindowHints = runSafeX11 . getWMHints

-- | Emit an event of type @ClientMessage@ that can be listened to and consumed
-- by XMonad event hooks.
sendCustomEvent
  :: MonadIO m
  => Atom -- ^ Command
  -> Atom -- ^ Argument
  -> Maybe X11Window -- ^ 'Just' a window, or 'Nothing' for the root window
  -> X11PropertyT m ()
sendCustomEvent cmd arg win = do
  root <- asks ctxRoot
  runSafeX11 $ do
    safeSendClientMessageEvent cmd arg root win
    mkSafe1 sync False

-- | Sends a 'ClientMessageEvent' to a window. The event mask will be
-- 'structureNotifyMask'.
safeSendClientMessageEvent
  :: HasCallStack
  => Atom -- ^ Command.
  -> Atom -- ^ Argument.
  -> Window  -- ^ Root window.
  -> Maybe Window -- ^ Destination for event.
  -> SafeX11 ()
safeSendClientMessageEvent cmd arg root win = mkSafe $ \d ->
  allocaXEvent $ \e -> do
    setEventType e clientMessage
    setClientMessageEvent e (fromMaybe root win) cmd 32 arg currentTime
    sendEvent d root False structureNotifyMask e

-- | Post the provided 'SafeX11' action to Taffybar's dedicated X11
-- thread, and wait for the result.
--
-- Any Xlib errors are thrown as 'SafeX11Exception'.
runSafeX11 :: (HasCallStack, MonadIO m) => SafeX11 a -> X11PropertyT m a
runSafeX11 action = asks ctxThread >>= liftIO . runSafeX11Sync action

-- | 'X11Property' which reflects whether or not the provided 'RROutput' is active.
isActiveOutput :: XRRScreenResources -> RROutput -> X11Property Bool
isActiveOutput sres output = do
  maybeOutputInfo <- runSafeX11 (mkSafe2 xrrGetOutputInfo sres output)
  return $ maybe 0 xrr_oi_crtc maybeOutputInfo /= 0

-- | Return all the active RANDR outputs.
getActiveOutputs :: X11Property [RROutput]
getActiveOutputs =
  asks ctxRoot >>= runSafeX11 . mkSafe1 xrrGetScreenResources >>= \case
    Just sres -> filterM (isActiveOutput sres) (xrr_sr_outputs sres)
    Nothing -> return []

-- | Get the index of the primary monitor as set and ordered by RANDR.
getPrimaryOutputNumber :: X11Property (Maybe Int)
getPrimaryOutputNumber = do
  primary <- runSafeX11 . mkSafe1 xrrGetOutputPrimary =<< asks ctxRoot
  outputs <- getActiveOutputs
  return $ primary `elemIndex` outputs

-- | Move the given 'X11Window' to the bottom of the X11 window stack.
doLowerWindow :: MonadUnliftIO m => X11Window -> X11PropertyT m ()
doLowerWindow = runSafeX11 . mkSafe1 lowerWindow

------------------------------------------------------------------------

unsafeGetDisplay :: X11Context -> Display
unsafeGetDisplay = unsafeGetResource . ctxThread

logHere :: MonadIO m => Priority -> String -> m ()
logHere p = liftIO . logM "System.Taffybar.Information.X11DesktopInfo" p

logAround :: MonadUnliftIO m => Priority -> String -> m a -> m a
logAround p msg = bracket_ (logHere p (msg ++ "...")) (logHere p ("..." ++ msg))
