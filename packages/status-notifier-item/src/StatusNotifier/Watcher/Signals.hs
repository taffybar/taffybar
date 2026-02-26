{-# LANGUAGE TemplateHaskell #-}
module StatusNotifier.Watcher.Signals where

import DBus.Generation
import Language.Haskell.TH

import StatusNotifier.Watcher.Constants

-- The bus name is set to nothing here because sender comes through as the
-- unique name of the watcher, not the special bus name that it requests.
generateSignals watcherClientGenerationParams { genBusName = Nothing }
                defaultWatcherInterfaceName watcherSignals

printWatcherSignals =
  runQ (generateSignals watcherClientGenerationParams { genBusName = Nothing }
                defaultWatcherInterfaceName watcherSignals) >>=
       putStrLn . pprint
