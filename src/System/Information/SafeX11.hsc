{-# LANGUAGE MultiParamTypeClasses, StandaloneDeriving, FlexibleInstances,
  InterruptibleFFI, ExistentialQuantification, DeriveDataTypeable #-}
-----------------------------------------------------------------------------
-- |
-- Module      : System.Information.SafeX11
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan A. Malison
-- Stability   : unstable
-- Portability : unportable
-----------------------------------------------------------------------------
module System.Information.SafeX11 where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe (MaybeT(..))
import           Data.Either.Combinators
import           Data.Functor ((<$>))
import           Data.Typeable
import           Foreign hiding (void)
import           Foreign.C.Types
import           GHC.ForeignPtr
import           GHC.IO.Exception
import           Graphics.X11.Xlib
import           Graphics.X11.Xlib.Extras
       hiding (rawGetWindowProperty, getWindowProperty8,
               getWindowProperty16, getWindowProperty32,
               xGetWMHints, getWMHints)
import           Prelude
import           Graphics.X11.Xlib.Types
import           System.IO
import           System.IO.Unsafe
import           System.Timeout

foreign import ccall safe "XlibExtras.h XGetWMHints"
    safeXGetWMHints :: Display -> Window -> IO (Ptr WMHints)

foreign import ccall interruptible "XlibExtras.h XGetWindowProperty"
               safeXGetWindowProperty ::
               Display ->
                 Window ->
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

rawGetWindowPropertyBytes
  :: Storable a
  => Int -> Display -> Atom -> Window -> IO (Maybe (ForeignPtr a, Int))
rawGetWindowPropertyBytes bits d atom w =
  alloca $ \actual_type_return ->
    alloca $ \actual_format_return ->
      alloca $ \nitems_return ->
        alloca $ \bytes_after_return ->
          alloca $ \prop_return -> do
            ret <- postX11RequestSync $
              safeXGetWindowProperty
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
            if fromRight (-1) ret /= 0
              then return Nothing
              else do
                prop_ptr <- peek prop_return
                actual_format <- fromIntegral `fmap` peek actual_format_return
                nitems <- fromIntegral `fmap` peek nitems_return
                getprop prop_ptr nitems actual_format
  where
    getprop prop_ptr nitems actual_format
      | actual_format == 0 = return Nothing -- Property not found
      | actual_format /= bits = xFree prop_ptr >> return Nothing
      | otherwise = do
        ptr <- newConcForeignPtr (castPtr prop_ptr) (void $ xFree prop_ptr)
        return $ Just (ptr, nitems)

data SafeX11Exception = SafeX11Exception deriving (Show, Eq, Typeable)

instance Exception SafeX11Exception

data IORequest = forall a. IORequest
  { ioAction :: IO a
  , ioResponse :: Chan (Either SafeX11Exception a)
  }

{-# NOINLINE requestQueue #-}
requestQueue :: Chan IORequest
requestQueue = unsafePerformIO newChan

{-# NOINLINE x11Thread #-}
x11Thread :: ThreadId
x11Thread = unsafePerformIO $ forkIO startHandlingX11Requests

withErrorHandler :: XErrorHandler -> IO a -> IO a
withErrorHandler new_handler action = do
    handler <- mkXErrorHandler (\d e -> new_handler d e >> return 0)
    original <- _xSetErrorHandler handler
    res <- action
    _ <- _xSetErrorHandler original
    return res

deriving instance Show ErrorEvent

startHandlingX11Requests :: IO ()
startHandlingX11Requests =
  withErrorHandler handleError handleX11Requests
  where handleError _ xerrptr = do
          putStrLn "Got error"
          ee <- getErrorEvent xerrptr
          print ee

handleX11Requests :: IO ()
handleX11Requests = do
  IORequest {ioAction = action, ioResponse = responseChannel} <-
    readChan requestQueue
  res <-
    catch
      (maybe (Left SafeX11Exception) Right <$> timeout 500000 action)
      (\e -> do
         putStrLn "Got error on X11 thread"
         hFlush stdout
         print (e :: IOException)
         return $ Left SafeX11Exception)
  writeChan responseChannel res
  handleX11Requests
  return ()

postX11RequestSync :: IO a -> IO (Either SafeX11Exception a)
postX11RequestSync action = do
  let postAndWait = do
        responseChannel <- newChan :: IO (Chan (Either SafeX11Exception a))
        writeChan
          requestQueue
          IORequest {ioAction = action, ioResponse = responseChannel}
        readChan responseChannel
  currentTID <- myThreadId
  if currentTID == x11Thread
    then Right <$> action
    else postAndWait

postX11RequestSyncDef :: a -> IO a -> IO a
postX11RequestSyncDef def action =
  fromRight def <$> postX11RequestSync action

rawGetWindowProperty ::
  Storable a
  => Int -> Display -> Atom -> Window -> IO (Maybe [a])
rawGetWindowProperty bits d atom w =
  runMaybeT $ do
    (ptr, count) <- MaybeT $ rawGetWindowPropertyBytes bits d atom w
    lift $ withForeignPtr ptr $ peekArray count

getWindowProperty8 :: Display -> Atom -> Window -> IO (Maybe [CChar])
getWindowProperty8 = rawGetWindowProperty 8

getWindowProperty16 :: Display -> Atom -> Window -> IO (Maybe [CShort])
getWindowProperty16 = rawGetWindowProperty 16

getWindowProperty32 :: Display -> Atom -> Window -> IO (Maybe [CLong])
getWindowProperty32 = rawGetWindowProperty 32

getWMHints :: Display -> Window -> IO WMHints
getWMHints dpy w = do
    p <- safeXGetWMHints dpy w
    if p == nullPtr
        then return $ WMHints 0 False 0 0 0 0 0 0 0
        else do x <- peek p; _ <- xFree p; return x

safeGetGeometry :: Display -> Drawable ->
        IO (Window, Position, Position, Dimension, Dimension, Dimension, CInt)
safeGetGeometry display d =
        outParameters7 (throwIfZero "getGeometry") $
                xGetGeometry display d

outParameters7 :: (Storable a, Storable b, Storable c, Storable d, Storable e, Storable f, Storable g) =>
        (IO r -> IO ()) -> (Ptr a -> Ptr b -> Ptr c -> Ptr d -> Ptr e -> Ptr f -> Ptr g -> IO r) ->
        IO (a,b,c,d,e,f,g)
outParameters7 check fn =
        alloca $ \ a_return ->
        alloca $ \ b_return ->
        alloca $ \ c_return ->
        alloca $ \ d_return ->
        alloca $ \ e_return ->
        alloca $ \ f_return ->
        alloca $ \ g_return -> do
        check (fn a_return b_return c_return d_return e_return f_return g_return)
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
                Ptr Window -> Ptr Position -> Ptr Position -> Ptr Dimension ->
                Ptr Dimension -> Ptr Dimension -> Ptr CInt -> IO Status
