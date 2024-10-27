{-# LANGUAGE
  InterruptibleFFI, StandaloneDeriving, GeneralizedNewtypeDeriving,
  DeriveDataTypeable, DeriveGeneric, FlexibleInstances,
  MultiParamTypeClasses, ExistentialQuantification, ScopedTypeVariables,
  LambdaCase, RecordWildCards, NumericUnderscores
#-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_HADDOCK not-home #-}

-----------------------------------------------------------------------------
-- |
-- Module      : System.Taffybar.Information.SafeX11
-- Description : Error handling and safe concurrency for Xlib
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan A. Malison, Rodney Lorrimar
-- Stability   : unstable
-- Portability : POSIX
--
-- This module provides error handling for "Graphics.X11.Xlib"
-- operations, and a request queue to prevent concurrent access of a
-- 'Display'.
-----------------------------------------------------------------------------

module System.Taffybar.Information.SafeX11
  ( -- * XLib Error handling
    -- $errorHandling
    withErrorHandler
  , disableGlobalErrorHandler
  , ensureGlobalErrorHandler

  -- * Safe concurrent access to a 'Display'
  -- $concurrency
  , SafeX11Thread
  , withSafeX11Thread
  , OpenDisplayException(..)

  -- ** Constructing requests
  , SafeX11
  , mkSafe, mkSafe1, mkSafe2, mkSafe3

  -- ** Running 'SafeX11' operations
  , runSafeX11Sync
  , SafeX11Params(..)
  , setSafeX11Params
  , SafeX11Exception

  -- ** Property fetchers
  , X11Window
  , safeRootWindow
  , getWMHints
  , getWindowProperty8
  , getWindowProperty16
  , getWindowProperty32
  , getWindowPropertyBytes
  , safeGetGeometry

  -- *** Failure
  , GetWindowPropertyException(..)
  , WindowPropertyError(..)
  , throwWindowPropertyError
  , throwPropertyDecodeError

  -- * Opening a display
  , DisplayName(..)
  , withDisplay

  -- * Misc
  , closeDisplay
  , selectInput
  , sendEvent

  -- * Re-exports
  , module Graphics.X11.Xlib
  , module Graphics.X11.Xlib.Extras
  )
  where

import Control.Monad
import Control.Monad.IO.Unlift (MonadIO(..), MonadUnliftIO(..))
import Data.Bifunctor (first)
import Data.Default (Default(..))
import Data.Maybe (catMaybes, fromMaybe)
import Data.String (IsString (..))
import Data.Typeable (Typeable)
import Foreign hiding (void)
import Foreign.C.Types
import Foreign.C.String (CString, peekCString)
import GHC.ForeignPtr
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack, CallStack, callStack, prettyCallStack)
import System.Log.Logger
import System.Mem.Weak (Weak, deRefWeak, mkWeakPair)
import System.Taffybar.Information.ResourceThread hiding (mkSafe)
import System.Taffybar.Util (labelMyThread, bracketIO)
import System.IO.Unsafe (unsafePerformIO)
import Text.Printf
import UnliftIO.Async (cancelWith)
import UnliftIO.Environment (lookupEnv)
import UnliftIO.Exception (Exception(..), bracket, try, throwIO, bracket_, fromEitherIO, mapExceptionM, IOException, SomeAsyncException, throwString)
import qualified UnliftIO.MVar as MV
import UnliftIO.Timeout (timeout)

-- Where safe variants of foreign functions are defined, the unsafe
-- foreign functions definitions are hidden from re-export.
import Graphics.X11.Xlib hiding
  ( displayName, rootWindow, defaultRootWindow
  , closeDisplay, selectInput, sendEvent )
import Graphics.X11.Xlib.Extras hiding
  ( rawGetWindowProperty, getWindowProperty8
  , getWindowProperty16, getWindowProperty32
  , xGetWMHints, getWMHints, refreshKeyboardMapping
  )

logHere :: MonadIO m => Priority -> String -> m ()
logHere p = liftIO . logM "System.Taffybar.Information.SafeX11" p

logAround :: MonadUnliftIO m => Priority -> String -> m a -> m a
logAround p msg = bracket_ (logHere p (msg ++ "...")) (logHere p ("..." ++ msg))

-- | Specifies an X11 display to connect to.
data DisplayName = DefaultDisplay
                   -- ^ Use the @DISPLAY@ environment variable.
                 | DisplayName String
                   -- ^ Of the form @hostname:number.screen_number@
                 deriving (Show, Read, Eq, Ord, Generic)

instance Default DisplayName where
  def = DefaultDisplay

instance IsString DisplayName where
  fromString "" = DefaultDisplay
  fromString s = DisplayName s

-- | interface to the X11 library function @XCloseDisplay()@.
foreign import ccall safe "HsXlib.h XCloseDisplay"
        closeDisplay            :: Display -> IO ()

-- | An error occurred while opening an X display.
--
-- This would usually occur if the display socket does not exist.
--
-- But it could also occur if the @X -maxclients@ limit has been
-- exceeded. This might happen during unit testing of Taffybar.
data OpenDisplayException = OpenDisplayException
  { excDisplay :: String
  , excCallStack :: CallStack
  , excCause :: IOException
  } deriving (Show, Typeable)

instance Exception OpenDisplayException where
  displayException OpenDisplayException{..} =
    printf "Error opening display %s\n%s"
    excDisplay (prettyCallStack excCallStack)

withDisplay
  :: (HasCallStack, MonadUnliftIO m)
  => DisplayName -- ^ Display name.
  -> (Display -> m a) -- ^ Action to run.
  -> m a
withDisplay dn action = envDisplayName dn >>= flip withDisplay' action
  where
    withDisplay' d = bracketIO before after
      where
        before = do
          labelMyThread "x11Thread"
          logOpen $ handleErrors $ openDisplay d
        after = logClose . closeDisplay

        -- Convert userError "openDisplay" into something more descriptive
        handleErrors = mapExceptionM (OpenDisplayException d callStack)

        logOpen = logAround DEBUG ("XOpenDisplay " ++ d)
        logClose = logAround DEBUG ("XCloseDisplay " ++ d)

    envDisplayName DefaultDisplay = fromMaybe "" <$> lookupEnv "DISPLAY"
    envDisplayName (DisplayName displayName) = pure displayName


------------------------------------------------------------------------

-- $concurrency
-- Concurrency docs go here.
-- Also see "System.Taffybar.Information.ResourceThread".

-- | Provides an execution context for 'SafeX11' operations.
type SafeX11Thread = ResourceThread Display

-- | Builds a new 'X11Context' containing a connection to the given
-- X11 display and its root window.
--
-- If the X11 connection could not be opened, it will throw
-- 'OpenDisplayException'.
withSafeX11Thread
  :: (HasCallStack, MonadUnliftIO m)
  => DisplayName -- ^ Display name.
  -> (SafeX11Thread -> m a) -- ^ Action to run.
  -> m a
withSafeX11Thread dn = withResourceThread (withDisplay dn)

------------------------------------------------------------------------
-- | A type alias to disambiguate X11 windows (resource ids) from GTK
-- windows.
type X11Window = Window

-- | interface to the X11 library function @XDefaultRootWindow()@.
foreign import ccall safe "HsXlib.h XDefaultRootWindow"
        safeDefaultRootWindow       :: Display -> IO X11Window

safeRootWindow :: SafeX11 X11Window
safeRootWindow = mkSafe safeDefaultRootWindow

foreign import ccall safe "XlibExtras.h XGetWMHints"
    safeXGetWMHints :: Display -> X11Window -> IO (Ptr WMHints)

foreign import ccall interruptible "XlibExtras.h XGetWindowProperty"
               safeXGetWindowProperty ::
               Display ->
                 X11Window ->
                   Atom ->
                     CLong ->
                       CLong ->
                         Bool ->
                           Atom ->
                             Ptr Atom ->
                               Ptr CInt ->
                                 Ptr CULong ->
                                   Ptr CULong ->
                                     Ptr (Ptr CUChar) -> IO Status

data GetWindowPropertyException = GetWindowPropertyException
  { getWindowPropertyResourceId :: X11Window
  , getWindowPropertyAtom :: Atom
  , getWindowPropertyName :: Maybe String
  , getWindowPropertyError :: WindowPropertyError
  } deriving (Show)

instance Exception GetWindowPropertyException where
  displayException GetWindowPropertyException{..} =
    printf "Getting property %satom 0x%x of window 0x%x: %s"
    (maybe "" (printf "%s ") getWindowPropertyName)
    getWindowPropertyAtom getWindowPropertyResourceId
    (displayException getWindowPropertyError)

data WindowPropertyError
  = WindowPropertyNotFound
    { windowPropertyCallStack :: CallStack }
  | WindowPropertyWrongFormat
    { windowPropertyRequestedFormat :: Int
    , windowPropertyActualFormat :: Int
    , windowPropertyCallStack :: CallStack
    }
  | WindowPropertyDecodeFailed
    { windowPropertyDecodeError :: String
    , windowPropertyCallStack :: CallStack }
  deriving (Show)

instance Exception WindowPropertyError where
  displayException e = printf "%s\n%s" msg backtrace
    where
      msg = case e of
        WindowPropertyNotFound _ -> "property not found"
        WindowPropertyWrongFormat req act _ -> printf "wrong format: requested %d bits, got %d" req act
        WindowPropertyDecodeFailed err _ -> printf "decoding property value failed: %s" err
      backtrace = prettyCallStack (windowPropertyCallStack e)

throwWindowPropertyError :: HasCallStack => (CallStack -> WindowPropertyError) -> IO a
throwWindowPropertyError exc = throwIO $ exc callStack

throwPropertyDecodeError :: HasCallStack => String -> IO a
throwPropertyDecodeError = throwWindowPropertyError . WindowPropertyDecodeFailed

rawGetWindowPropertyBytes
  :: (HasCallStack, Storable a)
  => Display -> Int -> Atom -> X11Window
  -> IO (ForeignPtr a, Int)
rawGetWindowPropertyBytes d bits atom w =
  alloca $ \actual_type_return ->
    alloca $ \actual_format_return ->
      alloca $ \nitems_return ->
        alloca $ \bytes_after_return ->
          alloca $ \prop_return -> do
            ret <- safeXGetWindowProperty
                d
                w
                atom
                0
                0xFFFFFFFF
                False
                anyPropertyType
                actual_type_return
                actual_format_return
                nitems_return
                bytes_after_return
                prop_return

            when (ret /= 0) $ throwIO $ SafeX11CallFailed "XGetWindowProperty"

            prop_ptr <- peek prop_return
            actual_format <- fromIntegral <$> peek actual_format_return
            nitems <- fromIntegral <$> peek nitems_return
            getprop prop_ptr nitems actual_format

  where
    err = throwIO . GetWindowPropertyException w atom Nothing . ($ callStack)
    getprop prop_ptr nitems actual_format
      | actual_format == 0 = err WindowPropertyNotFound
      | prop_ptr == nullPtr = err WindowPropertyNotFound
      | actual_format /= bits = do
          void $ xFree prop_ptr
          err (WindowPropertyWrongFormat bits actual_format)
      | otherwise = do
          ptr <- newConcForeignPtr (castPtr prop_ptr) (void $ xFree prop_ptr)
          return (ptr, nitems)

getWindowPropertyBytes :: (HasCallStack, Storable a) => Int -> Atom -> X11Window -> SafeX11 (ForeignPtr a, Int)
getWindowPropertyBytes = mkSafe3 rawGetWindowPropertyBytes

------------------------------------------------------------------------

data SafeX11Exception = SafeX11Exception
  { safeX11Display :: Display
  , safeX11ErrorEvents :: [ErrorEvent]
  , safeX11Cause :: Either SafeReqTimedOut SafeX11CallFailed
  } deriving (Show, Eq, Typeable)

instance Exception SafeX11Exception where
  displayException SafeX11Exception{..} = unlines (cause:xerrors)
    where
      cause = either displayException displayException safeX11Cause
      xerrors = map (formatErrorEvent safeX11Display) safeX11ErrorEvents

newtype SafeX11CallFailed = SafeX11CallFailed { safeX11CallName :: String }
  deriving (Show, Read, Eq, Typeable)

instance Exception SafeX11CallFailed where
  displayException (SafeX11CallFailed name) = printf "XLib %s call failed" name

deriving instance Show ErrorEvent
deriving instance Eq ErrorEvent

foreign import ccall safe "HsXlib.h XGetErrorText"
    xGetErrorText :: Display -> CUChar -> CString -> CInt -> IO ()

getErrorText :: Display -> CUChar -> String
getErrorText d code = unsafePerformIO $ do
  let len = 1000
  allocaBytes len $ \strp -> do
    xGetErrorText d code strp (fromIntegral len)
    peekCString strp

formatErrorEvent :: Display -> ErrorEvent -> String
formatErrorEvent d ev = getErrorText d (ev_error_code ev) ++ ". " ++ show ev

------------------------------------------------------------------------

-- $errorHandling
--
-- Normally, if there is any error resulting from an X request (e.g.
-- requesting properties of a window which no longer exists), Xlib
-- will immediately exit the program.
--
-- Randomly exiting is bad behaviour for Taffybar. We would like to
-- install an error handler to recover from such errors.
--
-- The issue is that most foreign functions in "Graphics.X11.Xlib" and
-- "Graphics.X11.Xlib.Extras" are @unsafe@, meaning that they must
-- not call back into the Haskell system.
--
-- That is, they must not cause a Haskell error-handling function to
-- be called, or else undefined behaviour ensues. \"Undefined behaviour\"
-- seems to mean that the program deadlocks, which is perhaps even
-- more annoying than a segfault.
--
-- The solution is to provide @safe@ variants of any Xlib function
-- that could potentially fail, so that it's possible to use a Haskell
-- function as the Xlib errror handler.

{-# NOINLINE displayErrorHandlers #-}
displayErrorHandlers :: MV.MVar [Weak (Display, ErrorEvent -> IO ())]
displayErrorHandlers = unsafePerformIO MV.newEmptyMVar

{-# NOINLINE displayErrorHandlersLock #-}
displayErrorHandlersLock :: (MV.MVar Int, MV.MVar ())
displayErrorHandlersLock = ( unsafePerformIO (MV.newMVar 0)
                           , unsafePerformIO MV.newEmptyMVar)

findErrorHandler :: Display -> IO (Maybe (ErrorEvent -> IO ()))
findErrorHandler d = do
  hs <- mapM deRefWeak =<< MV.readMVar displayErrorHandlers
  pure $ lookup d . catMaybes $ hs

fallbackErrorHandler :: Display -> ErrorEvent -> IO ()
fallbackErrorHandler d e = logHere ERROR $
  "Unhandled XLib error: " ++ formatErrorEvent d e

runErrorHandler :: Display -> ErrorEvent -> IO ()
runErrorHandler d e = do
  h <- findErrorHandler d
  fromMaybe (fallbackErrorHandler d) h e

setGlobalErrorHandler :: IO (FunPtr CXErrorHandler)
setGlobalErrorHandler = mkXErrorHandler handler >>= _xSetErrorHandler
  where
    handler d e = getErrorEvent e >>= runErrorHandler d >> return 0

ensureGlobalErrorHandler :: MonadUnliftIO m => m ()
ensureGlobalErrorHandler = do
  initial <- MV.tryPutMVar displayErrorHandlers mempty
  when initial $ liftIO $ -- writeGlobalErrorHandler $
    void setGlobalErrorHandler

-- | Disable all Xlib error handlers while the given action is running.
--
-- This is useful when running @unsafe@ Xlib calls if it is more
-- preferable to exit on error than to deadlock.
--
-- This function is applied when 'safeX11UseErrorHandlers' is @False@.
disableGlobalErrorHandler :: IO a -> IO a
disableGlobalErrorHandler = writeGlobalErrorHandler .
  bracket (_xSetErrorHandler nullFunPtr) _xSetErrorHandler . const

-- | Lock global error handler.
writeGlobalErrorHandler :: IO a -> IO a
writeGlobalErrorHandler = bracket_ (MV.putMVar w ()) (MV.takeMVar w)
  where
    (_, w) = displayErrorHandlersLock

-- | Lock global error handler for reading. Multiple threads can read
-- concurrently, but only one thread can write.
readGlobalErrorHandler :: IO a -> IO a
readGlobalErrorHandler = bracket_ begin end
  where
    (r, w) = displayErrorHandlersLock
    begin = MV.modifyMVar_ r $ \b -> do
      when (b == 0) $ MV.putMVar w ()
      pure (succ b)
    end = MV.modifyMVar_ r $ \b -> do
      when (b == 1) $ MV.takeMVar w
      pure (pred b)

-- | Registers an Xlib error handler for the given 'Display', which
-- can be used with @safe@ Xlib functions.
--
-- After the given action finishes, the previous handler for that
-- 'Display' is restored.
--
-- Xlib error handlers are actually process-global, and not associated
-- with any particular display. So, registration of error handlers is
-- tracked by a global variable internal to this module.
--
-- On the first time this is used, it installs the global Xlib error
-- handler which dispatches 'ErrorEvents' to the per-display handlers.
withErrorHandler
  :: MonadUnliftIO m
  => Display -- ^ Handle errors for the given X11 connection.
  -> (ErrorEvent -> m ()) -- ^ Action run when an error occurs.
  -> m a
  -> m a
withErrorHandler d handler action = do
  ensureGlobalErrorHandler
  withRunInIO $ \run -> bracket_
    (installErrorHandler d (run . handler))
    (removeErrorHandler d)
    (run action)

installErrorHandler :: Display -> (ErrorEvent -> IO ()) -> IO ()
installErrorHandler d h = remove >> MV.modifyMVar_ displayErrorHandlers add
  where
    remove = removeErrorHandler d
    add hs = (:) <$> mkWeakPair d h (Just remove) <*> pure hs

removeErrorHandler :: Display -> IO ()
removeErrorHandler d = MV.modifyMVar_ displayErrorHandlers modify
  where
    modify :: [Weak (Display, ErrorEvent -> IO ())] -> IO [Weak (Display, ErrorEvent -> IO ())]
    modify hs = do
      ds <- mapM deRefWeak hs
      let (a, b) = break ((== Just d) . fmap fst . snd) (zip hs ds)
      pure $ map fst (a ++ drop 1 b)

------------------------------------------------------------------------

-- | Represents 'IO' actions which use an X11 'Display' connection.
-- XLib is not thread-safe, so all calls which use a 'Display' need
-- to be queued and run sequentially using 'runSafeX11Sync'.
type SafeX11 r = SafeReq Display r

data SafeX11Params = SafeX11Params
  { safeX11TimeoutUsec :: Maybe Int
    -- ^ Give up on action after a timeout, default is 5 seconds.
  , safeX11UseErrorHandlers :: Bool
    -- ^ Use error handlers, default is @True@.
  , safeX11Unblocker :: Maybe (SomeAsyncException -> IO ())
    -- ^ Optional unblocker function, if the action won't respond to
    -- async exceptions.
  }

instance Default SafeX11Params where
  def = SafeX11Params (Just 5_000_000) True Nothing

mkSafeReqParams :: SafeX11Params -> SafeReqParams Display
mkSafeReqParams SafeX11Params{..} = excl <> excr <> timeoutMaybe <> errors <> ub
  where
    excl = mkActionModifier $ \x -> mapExceptionM (SafeX11Exception x [] . Left)
    excr = mkActionModifier $ \x -> mapExceptionM (SafeX11Exception x [] . Right)
    timeoutMaybe = maybe mempty withTimeout safeX11TimeoutUsec
    errors = mkActionModifier $ \x -> if safeX11UseErrorHandlers
                                      then annotateSafeX11Request x
                                      else disableGlobalErrorHandler
    ub = maybe mempty mkUnblocker safeX11Unblocker

mkUnblocker :: (SomeAsyncException -> IO ()) -> SafeReqParams Display
mkUnblocker unblocker = mkCancelHandler $ \result e -> do
  logHere INFO $ "nextEvent: " ++ show e ++ ": it's unblocking time!"
  timeout 200_000 (cancelWith result e) >>= \case
    Just () -> logHere DEBUG "nextEvent: stopped within 200ms"
    Nothing -> do
      logHere INFO "nextEvent: didn't stop within 200ms - calling unblocker"
      unblocker e
      logHere DEBUG "nextEvent: waiting for effects of unblocker"
      timeout 1_000_000 (cancelWith result e) >>= \case
        Just () -> logHere NOTICE "nextEvent: unblocking success"
        Nothing -> do
          logHere WARNING "nextEvent: failed to unblock within 1000ms"
          throwString "nextEvent: failed to unblock within 1000ms"

annotateSafeX11Request :: Display -> IO a -> IO a
annotateSafeX11Request dpy = readGlobalErrorHandler . fromEitherIO . recordErrors . liftA2 (flip (first . addErrorEvents)) . try
  where
    addErrorEvents es e = e { safeX11ErrorEvents = safeX11ErrorEvents e ++ es }

    recordErrors :: (IO [ErrorEvent] -> IO a) -> IO a
    recordErrors f = do
      v <- MV.newMVar []
      let recordError e = MV.modifyMVar_ v (\es -> pure (e:es))
          getErrors = MV.modifyMVar v (\es -> pure ([], es))
      withErrorHandler dpy recordError (f getErrors)

setSafeX11Params :: SafeX11Params -> SafeX11 r -> SafeX11 r
setSafeX11Params p s = s { safeReqParams = mkSafeReqParams p }

-- | Construct a 'SafeX11' with default parameters from a function
-- which takes a single 'Display' argument.
mkSafe :: HasCallStack => (Display -> IO r) -> SafeX11 r
mkSafe = SafeReq (mkSafeReqParams def)

-- | Same as 'mkSafe' but for functions with an argument __after__ the
-- 'Display' argument.
mkSafe1 :: HasCallStack => (Display -> a -> IO r) -> a -> SafeX11 r
mkSafe1 f = mkSafe . flip f

-- | Same as 'mkSafe' but for functions with 'Display' then two arguments.
mkSafe2 :: HasCallStack => (Display -> a -> b -> IO r) -> a -> b -> SafeX11 r
mkSafe2 f a b = mkSafe (\x -> f x a b)

-- | Same as 'mkSafe' but for functions with 'Display' then three arguments.
mkSafe3 :: HasCallStack => (Display -> a -> b -> c -> IO r) -> a -> b -> c -> SafeX11 r
mkSafe3 f a b c = mkSafe (\x -> f x a b c)

-- | Post the provided 'SafeX11' action to Taffybar's dedicated X11
-- thread, and wait for the result.
--
-- Any exception caught while running the action will be rethrown here.
-- Xlib errors are thrown as 'SafeX11Exception'.
runSafeX11Sync :: HasCallStack => SafeX11 a -> SafeX11Thread -> IO a
runSafeX11Sync = resourceRunSync

------------------------------------------------------------------------

getWindowPropertyN :: (HasCallStack, Storable a) => Int -> Atom -> X11Window -> SafeX11 [a]
getWindowPropertyN bits atom w = do
  (ptr, count) <- getWindowPropertyBytes bits atom w
  liftIO $ withForeignPtr ptr $ peekArray count

getWindowProperty8 :: HasCallStack => Atom -> X11Window -> SafeX11 [CChar]
getWindowProperty8 = getWindowPropertyN 8

getWindowProperty16 :: HasCallStack => Atom -> X11Window -> SafeX11 [CShort]
getWindowProperty16 = getWindowPropertyN 16

getWindowProperty32 :: HasCallStack => Atom -> X11Window -> SafeX11 [CLong]
getWindowProperty32 = getWindowPropertyN 32

getWMHints :: HasCallStack => X11Window -> SafeX11 WMHints
getWMHints w = do
  p <- mkSafe1 safeXGetWMHints w
  when (p == nullPtr) $ throwIO $ SafeX11CallFailed "getWMHints"
  liftIO (peek p <* xFree p)

safeGetGeometry
  :: HasCallStack
  => Drawable
  -> SafeX11 (X11Window, Position, Position, Dimension, Dimension, Dimension, CInt)
safeGetGeometry drawable = mkSafe $ \dpy ->
  outParameters7 check $ xGetGeometry dpy drawable
  where
    check c = when (c == 0) $ throwIO $ SafeX11CallFailed "getGeometry"

outParameters7
  :: (HasCallStack, Storable a, Storable b, Storable c, Storable d, Storable e, Storable f, Storable g)
  => (r -> IO ())
  -> (Ptr a -> Ptr b -> Ptr c -> Ptr d -> Ptr e -> Ptr f -> Ptr g -> IO r)
  -> IO (a,b,c,d,e,f,g)
outParameters7 check fn =
        alloca $ \ a_return ->
        alloca $ \ b_return ->
        alloca $ \ c_return ->
        alloca $ \ d_return ->
        alloca $ \ e_return ->
        alloca $ \ f_return ->
        alloca $ \ g_return -> do
        check =<< fn a_return b_return c_return d_return e_return f_return g_return
        a <- peek a_return
        b <- peek b_return
        c <- peek c_return
        d <- peek d_return
        e <- peek e_return
        f <- peek f_return
        g <- peek g_return
        return (a,b,c,d,e,f,g)

foreign import ccall safe "HsXlib.h XGetGeometry"
        xGetGeometry :: Display -> Drawable ->
                Ptr X11Window -> Ptr Position -> Ptr Position -> Ptr Dimension ->
                Ptr Dimension -> Ptr Dimension -> Ptr CInt -> IO Status

-- | interface to the X11 library function @XSelectInput()@.
foreign import ccall safe "HsXlib.h XSelectInput"
        selectInput :: Display -> Window -> EventMask -> IO ()

-- | interface to the X11 library function @XSendEvent()@.
sendEvent :: Display -> Window -> Bool -> EventMask -> XEventPtr -> IO ()
sendEvent display w propagate event_mask event_send = do
  res <- xSendEvent display w propagate event_mask event_send
  when (res == 0) $ throwIO $ SafeX11CallFailed "sendEvent"

foreign import ccall safe "HsXlib.h XSendEvent"
  xSendEvent :: Display -> Window -> Bool -> EventMask -> XEventPtr -> IO Status
