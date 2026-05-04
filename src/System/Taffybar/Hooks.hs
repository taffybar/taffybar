-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      : System.Taffybar.Hooks
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan A. Malison
-- Stability   : unstable
-- Portability : unportable
--
-- This module provides various startup hooks that can be added to
-- 'TaffybarConfig'.
module System.Taffybar.Hooks
  ( module System.Taffybar.DBus,
    module System.Taffybar.Hooks,
    module System.Taffybar.LogLevels,
    ChromeTabImageData (..),
    getChromeTabImageDataChannel,
    getChromeTabImageDataTable,
    getX11WindowToChromeTabId,
    refreshBatteriesOnPropChange,
  )
where

import Control.Concurrent
import qualified Control.Concurrent.MVar as MV
import Control.Concurrent.STM.TChan
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.STM (atomically)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.Char (toLower)
import Data.List (isSuffixOf, nub)
import qualified Data.MultiMap as MM
import System.Directory (doesDirectoryExist)
import System.Environment (lookupEnv)
import System.Environment.XDG.DesktopEntry
import System.FSNotify (Event (eventIsDirectory, eventPath), EventIsDirectory (IsFile), startManager, watchDir)
import System.FilePath (splitSearchPath, (</>))
import System.Log.Logger
import System.Taffybar.Context
import System.Taffybar.DBus
import System.Taffybar.Information.Battery
import System.Taffybar.Information.Chrome
import System.Taffybar.Information.Network
import System.Taffybar.LogFormatter
import System.Taffybar.LogLevels

-- | The type of the channel that provides network information in taffybar.
newtype NetworkInfoChan
  = NetworkInfoChan (TChan [(String, (Rational, Rational))])

-- | Build a 'NetworkInfoChan' that refreshes at the provided interval.
buildNetworkInfoChan :: Double -> TaffyIO NetworkInfoChan
buildNetworkInfoChan interval = do
  chan <- liftIO newBroadcastTChanIO
  monitorNetworkInterfaces interval (void . atomically . writeTChan chan)
  return $ NetworkInfoChan chan

-- | Get the 'NetworkInfoChan' from 'Context', creating it if it does not exist.
getNetworkChan :: TaffyIO NetworkInfoChan
getNetworkChan = getStateDefault $ buildNetworkInfoChan 2.0

-- | Set the log formatter used in the taffybar process
setTaffyLogFormatter :: String -> IO ()
setTaffyLogFormatter loggerName = do
  handler <- taffyLogHandler
  updateGlobalLogger loggerName $ setHandlers [handler]

-- | Add 'refreshBatteriesOnPropChange' to the 'startupHook' of the
-- provided 'TaffybarConfig'. Use this if your system has issues with
-- the battery widget not updating or reporting the incorrect state.
--
-- This function 'withBatteryRefresh' is __not normally needed__
-- because the battery widget already subscribes to updates from
-- UPower, and UPower usually works correctly.
withBatteryRefresh :: TaffybarConfig -> TaffybarConfig
withBatteryRefresh = appendHook refreshBatteriesOnPropChange

-- | Load log levels from @~\/.config\/taffybar\/log-levels.yaml@ during
-- startup. The file should contain a YAML mapping from logger names to log
-- level strings. If the file does not exist, this is a no-op.
--
-- Example @log-levels.yaml@:
--
-- > System.Taffybar.DBus.Toggle: DEBUG
-- > Graphics.UI.GIGtkStrut: INFO
--
-- To use a custom path instead, call 'loadLogLevelsFromFile' directly.
withLogLevels :: TaffybarConfig -> TaffybarConfig
withLogLevels =
  appendHook $
    lift $
      defaultLogLevelsPath >>= loadLogLevelsFromFile

-- | Environment variable names used by 'withCSSPathEnvConfig'.
--
-- All fields are optional so callers can opt in to exactly the environment
-- surface they want to expose.
data CSSPathEnvConfig = CSSPathEnvConfig
  { -- | When set, replaces all resolved CSS paths with the path list from this
    -- environment variable.
    cssPathsEnvVar :: Maybe String,
    -- | When set to a truthy value, drops taffybar's vendored CSS paths.
    disableVendorCssEnvVar :: Maybe String,
    -- | When set to a truthy value, drops user CSS paths.
    disableUserCssEnvVar :: Maybe String
  }

-- | Default environment variable names for CSS path overrides.
defaultCSSPathEnvConfig :: CSSPathEnvConfig
defaultCSSPathEnvConfig =
  CSSPathEnvConfig
    { cssPathsEnvVar = Just "TAFFYBAR_CSS_PATHS",
      disableVendorCssEnvVar = Just "TAFFYBAR_DISABLE_VENDOR_CSS",
      disableUserCssEnvVar = Just "TAFFYBAR_DISABLE_USER_CSS"
    }

-- | Opt in to CSS path overrides from the environment.
--
-- This hook is intentionally not part of the default CSS loading behavior.
-- Apply it to a 'TaffybarConfig' when runtime environment overrides are wanted.
withEnvironmentCSSPaths :: TaffybarConfig -> TaffybarConfig
withEnvironmentCSSPaths = withCSSPathEnvConfig defaultCSSPathEnvConfig

-- | Opt in to CSS path overrides from caller-selected environment variables.
withCSSPathEnvConfig :: CSSPathEnvConfig -> TaffybarConfig -> TaffybarConfig
withCSSPathEnvConfig config =
  appendCSSPathTransform $ applyCSSPathEnvConfig config

applyCSSPathEnvConfig :: CSSPathEnvConfig -> CSSPaths -> IO CSSPaths
applyCSSPathEnvConfig
  CSSPathEnvConfig
    { cssPathsEnvVar = pathsVar,
      disableVendorCssEnvVar = disableVendorVar,
      disableUserCssEnvVar = disableUserVar
    }
  paths = do
    overridePaths <- lookupPathListEnv pathsVar
    disableVendor <- lookupFlagEnv disableVendorVar
    disableUser <- lookupFlagEnv disableUserVar
    let CSSPaths vendorPaths userPaths =
          case overridePaths of
            Just pathList -> paths {userCSSPaths = pathList}
            Nothing -> paths
    pure
      CSSPaths
        { vendorCSSPaths =
            if disableVendor
              then []
              else vendorPaths,
          userCSSPaths =
            if disableUser
              then []
              else userPaths
        }

lookupPathListEnv :: Maybe String -> IO (Maybe [FilePath])
lookupPathListEnv Nothing = pure Nothing
lookupPathListEnv (Just varName) =
  fmap parseCSSPathList <$> lookupEnv varName

parseCSSPathList :: String -> [FilePath]
parseCSSPathList =
  filter (not . null) . splitSearchPath

lookupFlagEnv :: Maybe String -> IO Bool
lookupFlagEnv Nothing = pure False
lookupFlagEnv (Just varName) = envFlag varName

envFlag :: String -> IO Bool
envFlag name = do
  value <- lookupEnv name
  pure $
    case fmap (map toLower) value of
      Just "1" -> True
      Just "true" -> True
      Just "yes" -> True
      Just "on" -> True
      _ -> False

newtype DesktopEntryCacheWatch
  = DesktopEntryCacheWatch (IO ())

-- | Load the 'DesktopEntry' cache from 'Context' state.
getDirectoryEntriesByClassName :: TaffyIO (MM.MultiMap String DesktopEntry)
getDirectoryEntriesByClassName =
  getStateDefault readDirectoryEntriesDefault

-- | Keep the 'DesktopEntry' cache fresh by watching XDG application directories
-- for desktop entry file updates.
updateDirectoryEntriesCache :: TaffyIO ()
updateDirectoryEntriesCache = do
  _ <- getStateDefault startDesktopEntryCacheWatch
  return ()

startDesktopEntryCacheWatch :: TaffyIO DesktopEntryCacheWatch
startDesktopEntryCacheWatch = do
  ctx <- ask
  appDirs <- lift getDesktopEntryApplicationDirs
  updateSignal <- lift MV.newEmptyMVar
  mgr <- lift startManager
  stopActions <-
    lift $
      forM appDirs $
        \dir ->
          watchDir mgr dir isDesktopEntryEvent $
            const $
              void $
                MV.tryPutMVar updateSignal ()
  _ <-
    lift $
      forkIO $
        forever $ do
          _ <- MV.takeMVar updateSignal
          threadDelay 200000
          flushUpdates updateSignal
          void $ runReaderT refreshDirectoryEntriesCache ctx
  void refreshDirectoryEntriesCache
  return $
    DesktopEntryCacheWatch $
      sequence_ stopActions

refreshDirectoryEntriesCache :: TaffyIO (MM.MultiMap String DesktopEntry)
refreshDirectoryEntriesCache = do
  entries <- readDirectoryEntriesDefault
  setState entries

flushUpdates :: MV.MVar () -> IO ()
flushUpdates updateSignal = do
  next <- MV.tryTakeMVar updateSignal
  case next of
    Nothing -> return ()
    Just _ -> flushUpdates updateSignal

isDesktopEntryEvent :: Event -> Bool
isDesktopEntryEvent event =
  eventIsDirectory event == IsFile && ".desktop" `isSuffixOf` eventPath event

getDesktopEntryApplicationDirs :: IO [FilePath]
getDesktopEntryApplicationDirs = do
  xdgDataDirs <- getXDGDataDirs
  filterM doesDirectoryExist $ map (</> "applications") $ nub xdgDataDirs

-- | Read 'DesktopEntry' values into a 'MM.Multimap', where they are indexed by
-- the class name specified in the 'DesktopEntry'.
readDirectoryEntriesDefault :: TaffyIO (MM.MultiMap String DesktopEntry)
readDirectoryEntriesDefault =
  lift $
    indexDesktopEntriesByClassName <$> getDirectoryEntriesDefault
