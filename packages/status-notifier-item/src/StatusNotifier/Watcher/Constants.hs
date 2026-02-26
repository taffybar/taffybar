{-# LANGUAGE OverloadedStrings #-}
module StatusNotifier.Watcher.Constants where

import           DBus.Client
import           DBus.Generation
import           DBus.Internal.Types
import qualified DBus.Introspection as I
import           Data.Coerce
import           Data.String
import           StatusNotifier.Util
import           System.IO.Unsafe
import           System.Log.Logger
import           Text.Printf

statusNotifierWatcherString :: String
statusNotifierWatcherString = "StatusNotifierWatcher"

getWatcherInterfaceName :: String -> InterfaceName
getWatcherInterfaceName interfaceNamespace =
  fromString $ printf "%s.%s" interfaceNamespace statusNotifierWatcherString

data ItemEntry = ItemEntry
  { serviceName :: BusName
  , servicePath :: ObjectPath
  } deriving (Show, Eq)

data WatcherParams = WatcherParams
  { watcherNamespace :: String
  , watcherPath :: String
  , watcherStop :: IO ()
  , watcherDBusClient :: Maybe Client
  , watcherStateCachePath :: Maybe FilePath
  }

defaultWatcherParams :: WatcherParams
defaultWatcherParams =
  WatcherParams
  { watcherNamespace = "org.kde"
  , watcherStop = return ()
  , watcherPath = "/StatusNotifierWatcher"
  , watcherDBusClient = Nothing
  , watcherStateCachePath = Nothing
  }

defaultWatcherInterfaceName =
  getWatcherInterfaceName $ watcherNamespace defaultWatcherParams

serviceArg = I.SignalArg { I.signalArgName = "service"
                         , I.signalArgType = TypeString
                         }

watcherSignals = [ I.Signal { I.signalName = "StatusNotifierItemRegistered"
                            , I.signalArgs = [serviceArg]
                            }
                 , I.Signal { I.signalName = "StatusNotifierItemUnregistered"
                            , I.signalArgs = [serviceArg]
                            }
                 , I.Signal { I.signalName = "StatusNotifierHostRegistered"
                            , I.signalArgs = []
                            }
                 ]

watcherClientGenerationParams =
  defaultGenerationParams
  { genBusName = Just $ fromString $ coerce $ getWatcherInterfaceName
                 (watcherNamespace defaultWatcherParams)
  , genObjectPath = Just $ fromString $ watcherPath defaultWatcherParams
  , genTakeSignalErrorHandler = True
  }
