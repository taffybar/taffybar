module Main where

import StatusNotifier.Watcher.Client
import DBus.Client
import Data.String

main = do
  client <- connectSession
  registeredItems <-
    getRegisteredSNIEntries
      client
  print registeredItems
  return ()
