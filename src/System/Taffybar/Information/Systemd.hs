{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      : System.Taffybar.Information.Systemd
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan A. Malison
-- Stability   : unstable
-- Portability : unportable
--
-- This module provides information about systemd failed units using the
-- org.freedesktop.systemd1 DBus interface. It monitors both the system bus
-- (for system units) and the session bus (for user units).
module System.Taffybar.Information.Systemd
  ( SystemdState (..),
    SystemdInfo (..),
    getSystemdInfo,
    getSystemdInfoFromClients,
    getSystemdInfoChan,
    getSystemdInfoState,
    systemdUnknownInfo,
  )
where

import Control.Concurrent
import Control.Concurrent.STM.TChan
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.STM (atomically)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import DBus
import DBus.Client
import qualified DBus.TH as DBus
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word32)
import System.Log.Logger
import System.Taffybar.Context

-- | The state of a systemd instance (system or user).
data SystemdState
  = -- | All units are running properly
    SystemdRunning
  | -- | Some units have failed
    SystemdDegraded
  | -- | System is starting up
    SystemdStarting
  | -- | System is shutting down
    SystemdStopping
  | -- | System is in maintenance mode
    SystemdMaintenance
  | -- | System is initializing
    SystemdInitializing
  | -- | Unknown state
    SystemdOther Text
  | -- | systemd is not available on this bus
    SystemdUnavailable
  deriving (Eq, Show)

-- | Information about systemd failed units.
data SystemdInfo = SystemdInfo
  { -- | State of system systemd
    systemState :: SystemdState,
    -- | State of user systemd
    userState :: SystemdState,
    -- | Number of failed system units
    systemFailedCount :: Int,
    -- | Number of failed user units
    userFailedCount :: Int,
    -- | Total failed units (system + user)
    totalFailedCount :: Int
  }
  deriving (Eq, Show)

-- | Default info when systemd is unknown/unavailable.
systemdUnknownInfo :: SystemdInfo
systemdUnknownInfo =
  SystemdInfo
    { systemState = SystemdUnavailable,
      userState = SystemdUnavailable,
      systemFailedCount = 0,
      userFailedCount = 0,
      totalFailedCount = 0
    }

systemdLogPath :: String
systemdLogPath = "System.Taffybar.Information.Systemd"

-- | DBus constants for systemd.
systemdBusName :: BusName
systemdBusName = "org.freedesktop.systemd1"

systemdObjectPath :: ObjectPath
systemdObjectPath = "/org/freedesktop/systemd1"

systemdManagerInterface :: InterfaceName
systemdManagerInterface = "org.freedesktop.systemd1.Manager"

-- | Parse a SystemdState from a DBus string.
parseSystemdState :: Text -> SystemdState
parseSystemdState txt =
  case T.toLower txt of
    "running" -> SystemdRunning
    "degraded" -> SystemdDegraded
    "starting" -> SystemdStarting
    "stopping" -> SystemdStopping
    "maintenance" -> SystemdMaintenance
    "initializing" -> SystemdInitializing
    _ -> SystemdOther txt

-- | Newtype wrapper for the channel/var pair stored in Context.
newtype SystemdInfoChanVar = SystemdInfoChanVar (TChan SystemdInfo, MVar SystemdInfo)

-- | Get the current systemd info state.
getSystemdInfoState :: TaffyIO SystemdInfo
getSystemdInfoState = do
  SystemdInfoChanVar (_, theVar) <- getSystemdInfoChanVar
  lift $ readMVar theVar

-- | Get the broadcast channel for systemd info updates.
getSystemdInfoChan :: TaffyIO (TChan SystemdInfo)
getSystemdInfoChan = do
  SystemdInfoChanVar (chan, _) <- getSystemdInfoChanVar
  return chan

-- | Get or create the channel/var pair for systemd monitoring.
getSystemdInfoChanVar :: TaffyIO SystemdInfoChanVar
getSystemdInfoChanVar =
  getStateDefault $ SystemdInfoChanVar <$> monitorSystemdInfo

-- | The debounce delay in microseconds (1 second, like Waybar).
debounceDelayMicros :: Int
debounceDelayMicros = 1_000_000

-- | Start monitoring systemd on both system and session buses.
monitorSystemdInfo :: TaffyIO (TChan SystemdInfo, MVar SystemdInfo)
monitorSystemdInfo = do
  infoVar <- lift $ newMVar systemdUnknownInfo
  chan <- liftIO newBroadcastTChanIO
  debounceVar <- liftIO $ newMVar False

  taffyFork $ do
    ctx <- ask
    systemClient <- asks systemDBusClient
    sessionClient <- asks sessionDBusClient

    let doUpdate = updateSystemdInfo chan infoVar systemClient sessionClient
        debouncedUpdate = do
          pending <- liftIO $ swapMVar debounceVar True
          unless pending $ do
            liftIO $ threadDelay debounceDelayMicros
            liftIO $ void $ swapMVar debounceVar False
            doUpdate

        signalCallback _ _ _ _ = runReaderT debouncedUpdate ctx

    -- Register for PropertiesChanged on system bus
    _ <-
      lift $
        DBus.registerForPropertiesChanged
          systemClient
          matchAny
            { matchInterface = Just systemdManagerInterface,
              matchPath = Just systemdObjectPath
            }
          signalCallback

    -- Register for PropertiesChanged on session bus
    _ <-
      lift $
        DBus.registerForPropertiesChanged
          sessionClient
          matchAny
            { matchInterface = Just systemdManagerInterface,
              matchPath = Just systemdObjectPath
            }
          signalCallback

    -- Initial update
    doUpdate

  return (chan, infoVar)

-- | Update the systemd info by querying both buses.
updateSystemdInfo ::
  TChan SystemdInfo ->
  MVar SystemdInfo ->
  Client ->
  Client ->
  TaffyIO ()
updateSystemdInfo chan var systemClient sessionClient = do
  info <- liftIO $ getSystemdInfoFromClients systemClient sessionClient
  liftIO $ do
    _ <- swapMVar var info
    atomically $ writeTChan chan info

-- | Get systemd info using the clients from Context.
getSystemdInfo :: TaffyIO SystemdInfo
getSystemdInfo = do
  systemClient <- asks systemDBusClient
  sessionClient <- asks sessionDBusClient
  liftIO $ getSystemdInfoFromClients systemClient sessionClient

-- | Get systemd info from the provided DBus clients.
getSystemdInfoFromClients :: Client -> Client -> IO SystemdInfo
getSystemdInfoFromClients systemClient sessionClient = do
  (sysState, sysFailed) <- getSystemdStateAndFailed systemClient "system"
  (usrState, usrFailed) <- getSystemdStateAndFailed sessionClient "user"
  return
    SystemdInfo
      { systemState = sysState,
        userState = usrState,
        systemFailedCount = sysFailed,
        userFailedCount = usrFailed,
        totalFailedCount = sysFailed + usrFailed
      }

-- | Query a single systemd instance for its state and failed count.
getSystemdStateAndFailed :: Client -> String -> IO (SystemdState, Int)
getSystemdStateAndFailed client busType = do
  stateResult <- getSystemdProperty client "SystemState"
  case stateResult of
    Left err -> do
      logM systemdLogPath DEBUG $
        "Failed to get " ++ busType ++ " systemd state: " ++ show err
      return (SystemdUnavailable, 0)
    Right stateVariant -> do
      let state =
            maybe
              (SystemdOther "unknown")
              parseSystemdState
              (fromVariant stateVariant :: Maybe Text)
      -- Only query failed count if degraded
      if state == SystemdDegraded
        then do
          failedResult <- getSystemdProperty client "NFailedUnits"
          case failedResult of
            Left err -> do
              logM systemdLogPath WARNING $
                "Failed to get " ++ busType ++ " failed units count: " ++ show err
              return (state, 0)
            Right failedVariant -> do
              let failed = fromMaybe 0 (fromVariant failedVariant :: Maybe Word32)
              return (state, fromIntegral failed)
        else return (state, 0)

-- | Get a property from the systemd1.Manager interface.
getSystemdProperty :: Client -> MemberName -> IO (Either MethodError Variant)
getSystemdProperty client propName = do
  reply <-
    call client $
      ( methodCall
          systemdObjectPath
          "org.freedesktop.DBus.Properties"
          "Get"
      )
        { methodCallDestination = Just systemdBusName,
          methodCallBody = [toVariant systemdManagerInterface, toVariant propName]
        }
  case reply of
    Left err -> return $ Left err
    Right ret ->
      case methodReturnBody ret of
        [v] -> case fromVariant v of
          Just inner -> return $ Right inner
          Nothing ->
            return $
              Left $
                methodError
                  (methodReturnSerial ret)
                  (errorName_ "org.taffybar.Systemd.TypeMismatch")
        _ ->
          return $
            Left $
              methodError
                (methodReturnSerial ret)
                (errorName_ "org.taffybar.Systemd.InvalidResponse")
