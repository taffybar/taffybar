{-# LANGUAGE OverloadedStrings #-}
module System.Log.DBus.Server where

import           Data.IORef
import           Data.Map (Map)
import qualified Data.Map as Map
import           DBus
import           DBus.Client
import qualified DBus.Introspection as I
import           System.Log.Logger
import           Text.Read

maybeToEither :: b -> Maybe a -> Either b a
maybeToEither = flip maybe Right . Left

-- | An opaque handle for the log server, which tracks loggers whose levels
-- have been explicitly set.
newtype LogServer = LogServer (IORef (Map String Priority))

-- | Create a new 'LogServer'.
newLogServer :: IO LogServer
newLogServer = LogServer <$> newIORef Map.empty

setLogLevelTracked :: LogServer -> String -> Priority -> IO ()
setLogLevelTracked (LogServer ref) logPrefix level = do
  getLogger logPrefix >>= saveGlobalLogger . setLevel level
  modifyIORef' ref (Map.insert logPrefix level)

setLogLevelFromPriorityStringTracked
  :: LogServer -> String -> String -> IO (Either Reply ())
setLogLevelFromPriorityStringTracked server logPrefix levelString =
  case readMaybe levelString of
    Just level -> Right <$> setLogLevelTracked server logPrefix level
    Nothing -> return $ Left (ReplyError errorInvalidParameters [])

-- | Get the log level of a specific logger. Returns the level as a string,
-- or an empty string if no level has been explicitly set on that logger.
getLogLevel :: String -> IO String
getLogLevel logPrefix = do
  logger <- getLogger logPrefix
  return $ maybe "" show (getLevel logger)

-- | Get all loggers whose levels have been explicitly set via 'SetLogLevel',
-- as a map from logger name to level string.
getConfiguredLogLevels :: LogServer -> IO (Map String String)
getConfiguredLogLevels (LogServer ref) =
  Map.map show <$> readIORef ref

-- | Build the D-Bus interface with tracking and introspection methods.
logInterfaceWithServer :: LogServer -> Interface
logInterfaceWithServer server = defaultInterface
  { interfaceName = "org.taffybar.LogServer"
  , interfaceMethods =
      [ autoMethod "SetLogLevel"
          (setLogLevelFromPriorityStringTracked server)
      , autoMethod "GetLogLevel" getLogLevel
      , autoMethod "GetConfiguredLogLevels"
          (getConfiguredLogLevels server)
      ]
  }

-- | The original interface with only 'SetLogLevel'. Kept for backward
-- compatibility.
logInterface :: Interface
logInterface = defaultInterface
  { interfaceName = "org.taffybar.LogServer"
  , interfaceMethods = [ autoMethod "SetLogLevel" setLogLevelFromPriorityString ]
  }

logPath :: ObjectPath
logPath = "/org/taffybar/LogServer"

-- | Start the log server with tracking and introspection. Returns a
-- 'LogServer' handle that can be used to register additional loggers.
startLogServerWithTracking :: Client -> IO LogServer
startLogServerWithTracking client = do
  server <- newLogServer
  export client logPath (logInterfaceWithServer server)
  return server

-- | Start the log server (original API, no tracking).
startLogServer :: Client -> IO ()
startLogServer client =
  export client logPath logInterface

-- | Introspection interface including all methods (SetLogLevel, GetLogLevel,
-- GetConfiguredLogLevels). Suitable for TH client generation.
logIntrospectionInterface :: I.Interface
logIntrospectionInterface = buildIntrospectionInterface $
  defaultInterface
    { interfaceName = "org.taffybar.LogServer"
    , interfaceMethods =
        [ autoMethod "SetLogLevel" setLogLevelFromPriorityString
        , autoMethod "GetLogLevel" getLogLevel
        , autoMethod "GetConfiguredLogLevels"
            (return Map.empty :: IO (Map String String))
        ]
    }

setLogLevelFromPriorityString :: String -> String -> IO (Either Reply ())
setLogLevelFromPriorityString logPrefix levelString =
  let maybePriority = readMaybe levelString
      getMaybeResult = sequenceA $ setLogLevel logPrefix <$> maybePriority
  in maybeToEither (ReplyError errorInvalidParameters []) <$> getMaybeResult

setLogLevel :: String -> Priority -> IO ()
setLogLevel logPrefix level =
  getLogger logPrefix >>= saveGlobalLogger . setLevel level
