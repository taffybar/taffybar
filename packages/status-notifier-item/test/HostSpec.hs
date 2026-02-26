{-# LANGUAGE OverloadedStrings #-}

module HostSpec (spec) where

import Control.Concurrent
  ( forkIO
  , newChan
  , newEmptyMVar
  , putMVar
  , readChan
  , takeMVar
  , threadDelay
  , tryPutMVar
  , writeChan
  )
import Control.Exception (finally)
import Control.Monad (replicateM, void)
import Data.List (sort)
import qualified Data.Map.Strict as Map
import DBus (busName_, objectPath_)
import DBus.Client
import qualified DBus.Internal.Message as M
import DBus.Internal.Types (ErrorName, Serial (..), errorName_)
import StatusNotifier.Host.Service hiding (startWatcher)
import qualified StatusNotifier.Watcher.Client as WatcherClient
import System.Log.Logger (Priority (..))
import System.Timeout (timeout)
import Test.Hspec

import TestSupport

spec :: Spec
spec = around withIsolatedSessionBus $ do
  describe "StatusNotifier.Host.Service integration" $ do
    it "receives ItemAdded then ItemRemoved for a registered item" $ \() -> do
      _watcher <- startWatcher
      hostClient <- connectSession
      Just host <- build defaultParams {dbusClient = Just hostClient, uniqueIdentifier = "host-a"}
      events <- newChan
      _ <- addUpdateHandler host (\ut info -> writeChan events (ut, itemServiceName info))

      itemClient <- connectSession
      cleanup <- registerSimpleItem itemClient "org.test.HostItemA" defaultPath "folder"

      added <- timeout 1500000 (readChan events)
      added `shouldBe` Just (ItemAdded, busName_ "org.test.HostItemA")

      cleanup

      removed <- timeout 1500000 (readChan events)
      removed `shouldBe` Just (ItemRemoved, busName_ "org.test.HostItemA")

    it "replays existing items to newly-added handlers" $ \() -> do
      _watcher <- startWatcher
      hostClient <- connectSession
      itemClient <- connectSession
      cleanup <- registerSimpleItem itemClient "org.test.HostItemB" defaultPath "browser"

      Just host <- build defaultParams {dbusClient = Just hostClient, uniqueIdentifier = "host-b"}
      events <- newChan
      _ <- addUpdateHandler host (\ut info -> writeChan events (ut, itemServiceName info))

      replayed <- timeout 1500000 (readChan events)
      replayed `shouldBe` Just (ItemAdded, busName_ "org.test.HostItemB")

      cleanup

    it "stops dispatching updates after removeUpdateHandler" $ \() -> do
      _watcher <- startWatcher
      hostClient <- connectSession
      Just host <- build defaultParams {dbusClient = Just hostClient, uniqueIdentifier = "host-c"}
      events <- newChan
      token <- addUpdateHandler host (\ut info -> writeChan events (ut, itemServiceName info))
      removeUpdateHandler host token

      itemClient <- connectSession
      cleanup <- registerSimpleItem itemClient "org.test.HostItemC" defaultPath "help-about"
      cleanup

      noEvent <- timeout 500000 (readChan events)
      noEvent `shouldBe` Nothing

    it "keeps itemInfoMap in sync across add/remove transitions" $ \() -> do
      _watcher <- startWatcher
      hostClient <- connectSession
      Just host <- build defaultParams {dbusClient = Just hostClient, uniqueIdentifier = "host-d"}
      itemClient <- connectSession
      cleanup <- registerSimpleItem itemClient "org.test.HostItemD" defaultPath "mail-send"

      addedMapReady <-
        waitFor 1500000 $ do
          current <- itemInfoMap host
          pure $ busName_ "org.test.HostItemD" `elem` Map.keys current
      addedMapReady `shouldBe` True

      cleanup

      removedMapReady <-
        waitFor 1500000 $ do
          current <- itemInfoMap host
          pure $ busName_ "org.test.HostItemD" `notElem` Map.keys current
      removedMapReady `shouldBe` True

    it "loads item icon names into itemInfoMap" $ \() -> do
      _watcher <- startWatcher
      hostClient <- connectSession
      Just host <- build defaultParams {dbusClient = Just hostClient, uniqueIdentifier = "host-e"}
      itemClient <- connectSession
      cleanup <- registerSimpleItem itemClient "org.test.HostItemE" defaultPath "drive-harddisk"
      (do
          populated <-
            waitFor 1500000 $ do
              current <- itemInfoMap host
              pure $ case Map.lookup (busName_ "org.test.HostItemE") current of
                Just info -> iconName info == "drive-harddisk"
                Nothing -> False
          populated `shouldBe` True
        ) `finally` cleanup

    it "replays all pre-existing items to newly-added handlers" $ \() -> do
      _watcher <- startWatcher
      hostClient <- connectSession
      itemClientA <- connectSession
      itemClientB <- connectSession
      cleanupA <- registerSimpleItem itemClientA "org.test.HostItemF" defaultPath "audio-volume-high"
      cleanupB <- registerSimpleItem itemClientB "org.test.HostItemG" defaultPath "audio-volume-low"
      (do
          Just host <- build defaultParams {dbusClient = Just hostClient, uniqueIdentifier = "host-f"}
          events <- newChan
          _ <- addUpdateHandler host (\ut info -> writeChan events (ut, itemServiceName info))
          replayed <- replicateM 2 (timeout 1500000 (readChan events))
          let addedNames = sort [name | Just (ItemAdded, name) <- replayed]
          addedNames
            `shouldBe` sort [busName_ "org.test.HostItemF", busName_ "org.test.HostItemG"]
        ) `finally` (cleanupA >> cleanupB)

    it "does not emit updates for forceUpdate on unknown services" $ \() -> do
      _watcher <- startWatcher
      hostClient <- connectSession
      Just host <- build defaultParams {dbusClient = Just hostClient, uniqueIdentifier = "host-g"}
      events <- newChan
      _ <- addUpdateHandler host (\ut info -> writeChan events (ut, itemServiceName info))
      forceUpdate host (busName_ "org.test.DoesNotExist")
      noEvent <- timeout 500000 (readChan events)
      noEvent `shouldBe` Nothing
      current <- itemInfoMap host
      Map.null current `shouldBe` True

    it "deduplicates an item that re-registers under a different bus name after watcher restart" $ \() -> do
      watcher1 <- startWatcher

      itemClient <- connectSession
      let iface =
            Interface
              { interfaceName = "org.kde.StatusNotifierItem"
              , interfaceMethods = []
              , interfaceProperties =
                  [ readOnlyProperty "IconName" (pure ("folder" :: String))
                  , readOnlyProperty "OverlayIconName" (pure ("" :: String))
                  , readOnlyProperty "ItemIsMenu" (pure False)
                  ]
              , interfaceSignals = []
              }
          defaultPath = "/StatusNotifierItem"
      export itemClient (objectPath_ defaultPath) iface
      _ <- requestName itemClient (busName_ "org.test.RestartRereg") []

      -- First register by path only; watcher will store the sender unique name.
      WatcherClient.registerStatusNotifierItem itemClient defaultPath
        `shouldReturn` Right ()

      hostClient <- connectSession
      Just host <- build defaultParams {dbusClient = Just hostClient, uniqueIdentifier = "host-h"}

      initialReady <-
        waitFor 1500000 $ do
          current <- itemInfoMap host
          pure $ Map.size current == 1
      initialReady `shouldBe` True
      initialMap <- itemInfoMap host
      let initialKeys = Map.keys initialMap
      length initialKeys `shouldBe` 1
      initialKey <-
        case initialKeys of
          [k] -> pure k
          _ -> expectationFailure "Expected exactly one initial item key" >> error "unreachable"

      -- Simulate watcher restart (lose all state).
      _ <- releaseName watcher1 (busName_ "org.kde.StatusNotifierWatcher")
      _watcher2 <- startWatcher

      -- Item re-registers, this time using its well-known name.
      WatcherClient.registerStatusNotifierItem itemClient "org.test.RestartRereg"
        `shouldReturn` Right ()

      -- Host should not keep both the old unique-name entry and the new
      -- well-known entry.
      deduped <-
        waitFor 1500000 $ do
          current <- itemInfoMap host
          pure $
            Map.size current == 1
              && Map.member (busName_ "org.test.RestartRereg") current
              && not (Map.member initialKey current)
      deduped `shouldBe` True

    it "removes stale items when watcher ownership changes and replacement watcher has no item" $ \() -> do
      watcher1 <- startWatcher

      hostClient <- connectSession
      Just host <- build defaultParams {dbusClient = Just hostClient, uniqueIdentifier = "host-i"}

      itemClient <- connectSession
      cleanup <- registerSimpleItem itemClient "org.test.HostWatcherOwnerChange" defaultPath "folder"

      added <-
        waitFor 1500000 $ do
          current <- itemInfoMap host
          pure $ Map.member (busName_ "org.test.HostWatcherOwnerChange") current
      added `shouldBe` True

      _ <- releaseName watcher1 (busName_ "org.kde.StatusNotifierWatcher")
      cleanup
      _watcher2 <- startWatcher

      removed <-
        waitFor 1500000 $ do
          current <- itemInfoMap host
          pure $ not $ Map.member (busName_ "org.test.HostWatcherOwnerChange") current
      removed `shouldBe` True

    it "does not emit duplicate ItemAdded when addUpdateHandler races with replay" $ \() -> do
      _watcher <- startWatcher
      hostClient <- connectSession
      Just host <- build defaultParams {dbusClient = Just hostClient, uniqueIdentifier = "host-j"}

      itemClient <- connectSession
      buildStarted <- newEmptyMVar
      continueBuild <- newEmptyMVar
      let itemName = "org.test.HostHandlerRace"
          iface =
            Interface
              { interfaceName = "org.kde.StatusNotifierItem"
              , interfaceMethods = []
              , interfaceProperties =
                  [ readOnlyProperty "IconName" $ do
                      void $ tryPutMVar buildStarted ()
                      takeMVar continueBuild
                      pure ("folder" :: String)
                  , readOnlyProperty "OverlayIconName" (pure ("" :: String))
                  , readOnlyProperty "ItemIsMenu" (pure False)
                  ]
              , interfaceSignals = []
              }
      export itemClient (objectPath_ defaultPath) iface
      _ <- requestName itemClient (busName_ itemName) []
      WatcherClient.registerStatusNotifierItem itemClient itemName
        `shouldReturn` Right ()

      -- Host is now blocked building the item while holding itemInfoMapVar.
      takeMVar buildStarted

      events <- newChan
      _ <- forkIO $ do
        threadDelay 50000
        putMVar continueBuild ()
      _ <- addUpdateHandler host (\ut info -> writeChan events (ut, itemServiceName info))

      first <- timeout 1500000 (readChan events)
      first `shouldBe` Just (ItemAdded, busName_ itemName)
      second <- timeout 300000 (readChan events)
      second `shouldBe` Nothing

      _ <- releaseName itemClient (busName_ itemName)
      pure ()

  describe "propertyUpdateFailureLogLevel" $ do
    it "returns DEBUG when there are successful updates" $ do
      let failures = [mkMethodError DBus.Client.errorFailed]
      propertyUpdateFailureLogLevel failures [()]
        `shouldBe` DEBUG

    it "returns DEBUG when all failures are UnknownMethod" $ do
      let failures = [mkMethodError DBus.Client.errorUnknownMethod]
      propertyUpdateFailureLogLevel failures ([] :: [()])
        `shouldBe` DEBUG

    it "returns DEBUG when all failures are InvalidArgs" $ do
      let failures = [mkMethodError errorInvalidArgs]
      propertyUpdateFailureLogLevel failures ([] :: [()])
        `shouldBe` DEBUG

    it "returns ERROR for unexpected failures with no successful updates" $ do
      let failures = [mkMethodError DBus.Client.errorFailed]
      propertyUpdateFailureLogLevel failures ([] :: [()])
        `shouldBe` ERROR

defaultPath :: String
defaultPath = "/StatusNotifierItem"

mkMethodError :: ErrorName -> M.MethodError
mkMethodError errName =
  M.MethodError
    { M.methodErrorName = errName
    , M.methodErrorSerial = Serial 0
    , M.methodErrorSender = Nothing
    , M.methodErrorDestination = Nothing
    , M.methodErrorBody = []
    }

errorInvalidArgs :: ErrorName
errorInvalidArgs = errorName_ "org.freedesktop.DBus.Error.InvalidArgs"
