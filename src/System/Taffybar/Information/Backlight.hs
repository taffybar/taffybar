{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-----------------------------------------------------------------------------
-- |
-- Module      : System.Taffybar.Information.Backlight
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan A. Malison
-- Stability   : unstable
-- Portability : unportable
--
-- Simple helpers to read backlight status from /sys/class/backlight.
--
-----------------------------------------------------------------------------

module System.Taffybar.Information.Backlight
  ( BacklightInfo(..)
  , getBacklightDevices
  , getBacklightInfo
  , getBacklightInfoChan
  , getBacklightInfoChanWithInterval
  , getBacklightInfoState
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Control.Concurrent.STM.TChan
import Control.Exception (SomeException, bracket, catch, try)
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.STM (atomically)
import Data.List (sort, sortBy)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Ord (Down(..), comparing)
import Data.Proxy (Proxy(..))
import GHC.Conc (threadWaitRead)
import GHC.TypeLits (KnownSymbol, SomeSymbol(..), Symbol, someSymbolVal)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>))
import System.Log.Logger (Priority(..))
import System.Taffybar.Context (TaffyIO, getStateDefault)
import System.Taffybar.Information.Udev
  ( backlightMonitorFd
  , closeBacklightMonitor
  , drainBacklightMonitor
  , openBacklightMonitor
  )
import System.Taffybar.Util (logPrintF)
import System.Timeout (timeout)
import Text.Read (readMaybe)

backlightBasePath :: FilePath
backlightBasePath = "/sys/class/backlight"

defaultBacklightRefreshIntervalSeconds :: Double
defaultBacklightRefreshIntervalSeconds = 2

-- | Information about a backlight device.
data BacklightInfo = BacklightInfo
  { backlightDevice :: FilePath
  , backlightBrightness :: Int
  , backlightMaxBrightness :: Int
  , backlightPercent :: Int
  } deriving (Eq, Show)

-- | List all backlight device names under /sys/class/backlight.
getBacklightDevices :: IO [FilePath]
getBacklightDevices =
  catch (sort <$> listDirectory backlightBasePath) $ \(_ :: SomeException) ->
    return []

-- | Get backlight info for the provided device name, or (when no name is
-- provided) the "best" backlight device found under @/sys/class/backlight@.
--
-- The best device is the one with the highest @max_brightness@, which mirrors
-- Waybar's backlight selection logic.
getBacklightInfo :: Maybe FilePath -> IO (Maybe BacklightInfo)
getBacklightInfo deviceOverride = do
  device <- selectDevice deviceOverride
  case device of
    Nothing -> return Nothing
    Just dev -> readDevice dev

backlightLogPath :: String
backlightLogPath = "System.Taffybar.Information.Backlight"

backlightLogF :: Show t => Priority -> String -> t -> IO ()
backlightLogF = logPrintF backlightLogPath

newtype BacklightInfoChanVar (a :: Symbol)
  = BacklightInfoChanVar (TChan (Maybe BacklightInfo), MVar (Maybe BacklightInfo))

-- | Get a broadcast channel for backlight info for the provided device.
--
-- Like Waybar, this is event-driven when possible (udev netlink monitor) but
-- will still refresh at a regular interval as a fallback/sanity check.
getBacklightInfoChan :: Maybe FilePath -> TaffyIO (TChan (Maybe BacklightInfo))
getBacklightInfoChan deviceOverride =
  getBacklightInfoChanWithInterval deviceOverride defaultBacklightRefreshIntervalSeconds

-- | Like 'getBacklightInfoChan', but allows customizing the refresh interval
-- (in seconds).
--
-- Note: the first call for a given device key wins if multiple callers request
-- different intervals, because the underlying monitoring thread is cached in
-- 'Context' via 'getStateDefault'.
getBacklightInfoChanWithInterval :: Maybe FilePath -> Double -> TaffyIO (TChan (Maybe BacklightInfo))
getBacklightInfoChanWithInterval deviceOverride intervalSeconds =
  case someSymbolVal (fromMaybe "__default__" deviceOverride) of
    SomeSymbol (Proxy :: Proxy sym) ->
      getBacklightInfoChanFor @sym deviceOverride intervalSeconds

-- | Read the most recent backlight info state for the provided device.
--
-- See 'getBacklightInfoChan' for monitoring behavior.
getBacklightInfoState :: Maybe FilePath -> TaffyIO (Maybe BacklightInfo)
getBacklightInfoState deviceOverride =
  case someSymbolVal (fromMaybe "__default__" deviceOverride) of
    SomeSymbol (Proxy :: Proxy sym) -> getBacklightInfoStateFor @sym deviceOverride

getBacklightInfoChanFor
  :: forall (a :: Symbol)
   . KnownSymbol a
  => Maybe FilePath
  -> Double
  -> TaffyIO (TChan (Maybe BacklightInfo))
getBacklightInfoChanFor deviceOverride intervalSeconds = do
  BacklightInfoChanVar (chan, _) <-
    getBacklightInfoChanVarFor @a deviceOverride intervalSeconds
  pure chan

getBacklightInfoStateFor
  :: forall (a :: Symbol)
   . KnownSymbol a
  => Maybe FilePath
  -> TaffyIO (Maybe BacklightInfo)
getBacklightInfoStateFor deviceOverride = do
  BacklightInfoChanVar (_, var) <-
    getBacklightInfoChanVarFor @a deviceOverride defaultBacklightRefreshIntervalSeconds
  liftIO $ readMVar var

getBacklightInfoChanVarFor
  :: forall (a :: Symbol)
   . KnownSymbol a
  => Maybe FilePath
  -> Double
  -> TaffyIO (BacklightInfoChanVar a)
getBacklightInfoChanVarFor deviceOverride intervalSeconds =
  getStateDefault $ do
    liftIO $ do
      chan <- newBroadcastTChanIO
      var <- newMVar Nothing
      _ <- forkIO $ monitorBacklightInfo deviceOverride intervalSeconds chan var
      pure $ BacklightInfoChanVar (chan, var)

monitorBacklightInfo ::
  Maybe FilePath ->
  Double ->
  TChan (Maybe BacklightInfo) ->
  MVar (Maybe BacklightInfo) ->
  IO ()
monitorBacklightInfo deviceOverride intervalSeconds chan var = do
  let
    intervalMicros :: Int
    intervalMicros = max 1 (floor (intervalSeconds * 1000000))

    writeInfo info = do
      _ <- swapMVar var info
      atomically $ writeTChan chan info

    refresh = getBacklightInfo deviceOverride >>= writeInfo

    pollingFallback e = do
      -- Polling is a last resort, but we prefer it to a stuck widget.
      backlightLogF WARNING "Backlight udev monitor unavailable, falling back to polling: %s" e
      refresh
      forever $ do
        threadDelay intervalMicros
        refresh

  monitorResult <- try openBacklightMonitor
  case monitorResult of
    Left (e :: SomeException) -> pollingFallback e
    Right mon -> bracket (pure mon) closeBacklightMonitor $ \m -> do
      refresh
      forever $ do
        -- Wait for a udev event or time out for a periodic refresh (Waybar-style).
        mEvent <- timeout intervalMicros $ threadWaitRead (backlightMonitorFd m)
        -- Only read from the udev socket if we were woken due to readability.
        -- Otherwise udev_monitor_receive_device can block.
        case mEvent of
          Nothing -> pure ()
          Just () -> drainBacklightMonitor m
        refresh

selectDevice :: Maybe FilePath -> IO (Maybe FilePath)
selectDevice (Just device) = do
  let path = backlightBasePath </> device
  exists <- doesDirectoryExist path
  if exists then pure (Just device) else selectDevice Nothing
selectDevice Nothing = do
  devices <- getBacklightDevices
  candidates <- mapM (\d -> (d,) . fromMaybe 0 <$> readMaxBrightness d) devices
  pure $ fst <$> listToMaybe (sortBy (comparing (Down . snd)) candidates)

readDevice :: FilePath -> IO (Maybe BacklightInfo)
readDevice device = do
  let basePath = backlightBasePath </> device
  mActual <- readIntFile (basePath </> "actual_brightness")
  brightness <- case mActual of
    Just v -> pure (Just v)
    Nothing -> readIntFile (basePath </> "brightness")
  maxBrightness <- readIntFile (basePath </> "max_brightness")
  case (brightness, maxBrightness) of
    (Just current, Just maxVal) | maxVal > 0 -> do
      let percent = round $ (fromIntegral current * 100 :: Double) / fromIntegral maxVal
      return $ Just BacklightInfo
        { backlightDevice = device
        , backlightBrightness = current
        , backlightMaxBrightness = maxVal
        , backlightPercent = percent
        }
    _ -> return Nothing

readMaxBrightness :: FilePath -> IO (Maybe Int)
readMaxBrightness device =
  readIntFile (backlightBasePath </> device </> "max_brightness")

readIntFile :: FilePath -> IO (Maybe Int)
readIntFile path =
  catch (do
    contents <- readFile path
    case words contents of
      (val:_) -> return $ readMaybe val
      _ -> return Nothing
  ) $ \(_ :: SomeException) -> return Nothing
