{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      : System.Taffybar.Information.Inhibitor
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan A. Malison
-- Stability   : unstable
-- Portability : unportable
--
-- This module provides functions for managing idle/sleep inhibitors using the
-- systemd-logind DBus interface. The inhibitor is acquired by calling the
-- Inhibit method on org.freedesktop.login1.Manager, which returns a file
-- descriptor. The lock is held as long as the fd is open.
-----------------------------------------------------------------------------
module System.Taffybar.Information.Inhibitor
  ( -- * Types
    InhibitType(..)
  , InhibitorState(..)
  , InhibitorContext(..)
    -- * Inhibitor Management
  , getInhibitorContext
  , getInhibitorState
  , toggleInhibitor
  , getInhibitorChan
    -- * Utilities
  , inhibitTypeToString
  , inhibitTypesFromStrings
  ) where

import           Control.Concurrent
import           Control.Concurrent.STM.TChan
import           Control.Monad.IO.Class
import           Control.Monad.STM (atomically)
import           Control.Monad.Trans.Reader
import           DBus
import           DBus.Client
import           Data.List (intercalate)
import qualified Data.Text as T
import           System.Log.Logger
import           System.Posix.IO (closeFd)
import           System.Posix.Types (Fd)
import           System.Taffybar.Context

-- | Types of inhibitors supported by systemd-logind
data InhibitType
  = InhibitIdle
  | InhibitShutdown
  | InhibitSleep
  | InhibitHandlePowerKey
  | InhibitHandleSuspendKey
  | InhibitHandleHibernateKey
  | InhibitHandleLidSwitch
  deriving (Eq, Show, Ord, Enum, Bounded)

-- | Current state of the inhibitor
data InhibitorState = InhibitorState
  { inhibitorActive :: Bool
  , inhibitorTypes :: [InhibitType]
  } deriving (Eq, Show)

-- | Context for managing an inhibitor, stored in taffybar's state
data InhibitorContext = InhibitorContext
  { inhibitorChan :: TChan InhibitorState
  , inhibitorStateVar :: MVar InhibitorState
  , inhibitorFdVar :: MVar (Maybe Fd)
  , inhibitorConfig :: [InhibitType]
  }

inhibitorLogPath :: String
inhibitorLogPath = "System.Taffybar.Information.Inhibitor"

inhibitorLog :: MonadIO m => Priority -> String -> m ()
inhibitorLog priority = liftIO . logM inhibitorLogPath priority

-- | Convert an InhibitType to its string representation for DBus
inhibitTypeToString :: InhibitType -> String
inhibitTypeToString InhibitIdle = "idle"
inhibitTypeToString InhibitShutdown = "shutdown"
inhibitTypeToString InhibitSleep = "sleep"
inhibitTypeToString InhibitHandlePowerKey = "handle-power-key"
inhibitTypeToString InhibitHandleSuspendKey = "handle-suspend-key"
inhibitTypeToString InhibitHandleHibernateKey = "handle-hibernate-key"
inhibitTypeToString InhibitHandleLidSwitch = "handle-lid-switch"

-- | Parse a string to an InhibitType
inhibitTypeFromString :: String -> Maybe InhibitType
inhibitTypeFromString "idle" = Just InhibitIdle
inhibitTypeFromString "shutdown" = Just InhibitShutdown
inhibitTypeFromString "sleep" = Just InhibitSleep
inhibitTypeFromString "handle-power-key" = Just InhibitHandlePowerKey
inhibitTypeFromString "handle-suspend-key" = Just InhibitHandleSuspendKey
inhibitTypeFromString "handle-hibernate-key" = Just InhibitHandleHibernateKey
inhibitTypeFromString "handle-lid-switch" = Just InhibitHandleLidSwitch
inhibitTypeFromString _ = Nothing

-- | Parse a list of strings to InhibitTypes, ignoring invalid ones
inhibitTypesFromStrings :: [String] -> [InhibitType]
inhibitTypesFromStrings = foldr go []
  where
    go s acc = case inhibitTypeFromString s of
      Just t -> t : acc
      Nothing -> acc

-- | Convert a list of InhibitTypes to the colon-separated format for DBus
inhibitTypesToDBusString :: [InhibitType] -> String
inhibitTypesToDBusString = intercalate ":" . map inhibitTypeToString

-- | The DBus destination for systemd-logind
login1BusName :: BusName
login1BusName = "org.freedesktop.login1"

-- | The DBus object path for the login1 manager
login1ObjectPath :: ObjectPath
login1ObjectPath = "/org/freedesktop/login1"

-- | The DBus interface for the login1 manager
login1Interface :: InterfaceName
login1Interface = "org.freedesktop.login1.Manager"

-- | Acquire an inhibitor lock via DBus
-- The Inhibit method takes (what, who, why, mode) and returns a file descriptor
acquireInhibitor :: Client -> [InhibitType] -> IO (Either MethodError Fd)
acquireInhibitor client types = do
  let what = inhibitTypesToDBusString types
      who = "taffybar"
      why = "User requested inhibition"
      mode = "block" :: String
  inhibitorLog DEBUG $ "Acquiring inhibitor for: " ++ what
  reply <- call client (methodCall login1ObjectPath login1Interface "Inhibit")
    { methodCallDestination = Just login1BusName
    , methodCallBody = [ toVariant (T.pack what)
                       , toVariant (T.pack who)
                       , toVariant (T.pack why)
                       , toVariant (T.pack mode)
                       ]
    }
  case reply of
    Left err -> do
      inhibitorLog WARNING $ "Failed to acquire inhibitor: " ++ show err
      return $ Left err
    Right ret -> do
      case methodReturnBody ret of
        [v] -> case fromVariant v :: Maybe Fd of
          Just fd -> do
            inhibitorLog DEBUG $ "Acquired inhibitor fd: " ++ show fd
            return $ Right fd
          Nothing -> do
            inhibitorLog WARNING "Invalid fd type in response"
            return $ Left $ methodError (methodReturnSerial ret) $
              errorName_ "org.taffybar.InvalidResponse"
        _ -> do
          inhibitorLog WARNING "Unexpected response format"
          return $ Left $ methodError (methodReturnSerial ret) $
            errorName_ "org.taffybar.InvalidResponse"

-- | Release an inhibitor by closing its file descriptor
releaseInhibitor :: Fd -> IO ()
releaseInhibitor fd = do
  inhibitorLog DEBUG $ "Releasing inhibitor fd: " ++ show fd
  closeFd fd

-- | Get or create the InhibitorContext for the given inhibit types
getInhibitorContext :: [InhibitType] -> TaffyIO InhibitorContext
getInhibitorContext types = getStateDefault $ do
  inhibitorLog DEBUG "Initializing InhibitorContext"
  chan <- liftIO newBroadcastTChanIO
  stateVar <- liftIO $ newMVar InhibitorState
    { inhibitorActive = False
    , inhibitorTypes = types
    }
  fdVar <- liftIO $ newMVar Nothing
  return InhibitorContext
    { inhibitorChan = chan
    , inhibitorStateVar = stateVar
    , inhibitorFdVar = fdVar
    , inhibitorConfig = types
    }

-- | Get the current inhibitor state
getInhibitorState :: [InhibitType] -> TaffyIO InhibitorState
getInhibitorState types = do
  ctx <- getInhibitorContext types
  liftIO $ readMVar (inhibitorStateVar ctx)

-- | Get the broadcast channel for inhibitor state changes
getInhibitorChan :: [InhibitType] -> TaffyIO (TChan InhibitorState)
getInhibitorChan types = do
  ctx <- getInhibitorContext types
  return $ inhibitorChan ctx

-- | Toggle the inhibitor on or off
toggleInhibitor :: [InhibitType] -> TaffyIO ()
toggleInhibitor types = do
  ctx <- getInhibitorContext types
  client <- asks systemDBusClient
  liftIO $ modifyMVar_ (inhibitorFdVar ctx) $ \maybeFd -> do
    case maybeFd of
      Just fd -> do
        -- Currently active, release it
        releaseInhibitor fd
        let newState = InhibitorState
              { inhibitorActive = False
              , inhibitorTypes = types
              }
        _ <- swapMVar (inhibitorStateVar ctx) newState
        atomically $ writeTChan (inhibitorChan ctx) newState
        inhibitorLog DEBUG "Inhibitor deactivated"
        return Nothing
      Nothing -> do
        -- Currently inactive, acquire it
        result <- acquireInhibitor client types
        case result of
          Right fd -> do
            let newState = InhibitorState
                  { inhibitorActive = True
                  , inhibitorTypes = types
                  }
            _ <- swapMVar (inhibitorStateVar ctx) newState
            atomically $ writeTChan (inhibitorChan ctx) newState
            inhibitorLog DEBUG "Inhibitor activated"
            return (Just fd)
          Left _ -> do
            inhibitorLog WARNING "Failed to acquire inhibitor"
            return Nothing
