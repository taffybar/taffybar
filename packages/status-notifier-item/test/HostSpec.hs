{-# LANGUAGE OverloadedStrings #-}

module HostSpec (spec) where

import Control.Concurrent
  ( forkIO,
    newChan,
    newEmptyMVar,
    newMVar,
    putMVar,
    readChan,
    readMVar,
    swapMVar,
    takeMVar,
    threadDelay,
    tryPutMVar,
    writeChan,
  )
import Control.Exception (finally)
import Control.Monad (join, replicateM, void)
import DBus (busName_, interfaceName_, memberName_, objectPath_, signal)
import DBus.Client
import qualified DBus.Internal.Message as M
import DBus.Internal.Types (ErrorName, Serial (..), errorName_)
import Data.List (sort)
import qualified Data.Map.Strict as Map
import StatusNotifier.Host.Service hiding (startWatcher)
import qualified StatusNotifier.Watcher.Client as WatcherClient
import System.Log.Logger (Priority (..))
import System.Timeout (timeout)
import Test.Hspec
import TestSupport

spec :: Spec
spec = around withIsolatedSessionBus $ do
  describe "logicalDuplicateKey" $ do
    it "normalizes generated Ayatana identities so duplicate app instances collapse" $ \() -> do
      let localSendItem token =
            ItemInfo
              { itemServiceName = busName_ ("org.test.LocalSend." <> token),
                itemServicePath = objectPath_ ("/org/ayatana/NotificationItem/" <> token),
                itemId = Just token,
                itemStatus = Just "Active",
                itemCategory = Just "ApplicationStatus",
                itemToolTip = Nothing,
                iconTitle = "localsend_app",
                iconName = "/nix/store/example-localsend/logo.png",
                overlayIconName = Nothing,
                iconThemePath = Nothing,
                iconPixmaps = [],
                overlayIconPixmaps = [],
                menuPath = Just $ objectPath_ ("/org/ayatana/NotificationItem/" <> token <> "/Menu"),
                itemIsMenu = True
              }
      logicalDuplicateKey (localSendItem "generatedA")
        `shouldBe` logicalDuplicateKey (localSendItem "generatedB")

    it "retains non-Ayatana item ids as part of logical identity" $ \() -> do
      let itemWithId itemIdValue =
            ItemInfo
              { itemServiceName = busName_ "org.test.RegularItem",
                itemServicePath = objectPath_ defaultPath,
                itemId = Just itemIdValue,
                itemStatus = Just "Active",
                itemCategory = Just "ApplicationStatus",
                itemToolTip = Nothing,
                iconTitle = "shared-title",
                iconName = "shared-icon",
                overlayIconName = Nothing,
                iconThemePath = Nothing,
                iconPixmaps = [],
                overlayIconPixmaps = [],
                menuPath = Nothing,
                itemIsMenu = False
              }
      logicalDuplicateKey (itemWithId "first")
        `shouldNotBe` logicalDuplicateKey (itemWithId "second")

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
      join $
        registerSimpleItem itemClient "org.test.HostItemC" defaultPath "help-about"

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
      ( do
          populated <-
            waitFor 1500000 $ do
              current <- itemInfoMap host
              pure $ case Map.lookup (busName_ "org.test.HostItemE") current of
                Just info -> iconName info == "drive-harddisk"
                Nothing -> False
          populated `shouldBe` True
        )
        `finally` cleanup

    it "replays all pre-existing items to newly-added handlers" $ \() -> do
      _watcher <- startWatcher
      hostClient <- connectSession
      itemClientA <- connectSession
      itemClientB <- connectSession
      cleanupA <- registerSimpleItem itemClientA "org.test.HostItemF" defaultPath "audio-volume-high"
      cleanupB <- registerSimpleItem itemClientB "org.test.HostItemG" defaultPath "audio-volume-low"
      ( do
          Just host <- build defaultParams {dbusClient = Just hostClient, uniqueIdentifier = "host-f"}
          events <- newChan
          _ <- addUpdateHandler host (\ut info -> writeChan events (ut, itemServiceName info))
          replayed <- replicateM 2 (timeout 1500000 (readChan events))
          let addedNames = sort [name | Just (ItemAdded, name) <- replayed]
          addedNames
            `shouldBe` sort [busName_ "org.test.HostItemF", busName_ "org.test.HostItemG"]
        )
        `finally` (cleanupA >> cleanupB)

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
              { interfaceName = "org.kde.StatusNotifierItem",
                interfaceMethods = [],
                interfaceProperties =
                  [ readOnlyProperty "IconName" (pure ("folder" :: String)),
                    readOnlyProperty "OverlayIconName" (pure ("" :: String)),
                    readOnlyProperty "ItemIsMenu" (pure False)
                  ],
                interfaceSignals = []
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

    it "deduplicates a logically identical item that appears under a new bus name" $ \() -> do
      _watcher <- startWatcher
      hostClient <- connectSession
      Just host <- build defaultParams {dbusClient = Just hostClient, uniqueIdentifier = "host-h2"}

      let duplicateIface =
            Interface
              { interfaceName = "org.kde.StatusNotifierItem",
                interfaceMethods = [],
                interfaceProperties =
                  [ readOnlyProperty "Id" (pure ("dup-item" :: String)),
                    readOnlyProperty "Title" (pure ("Duplicate Item" :: String)),
                    readOnlyProperty "Category" (pure ("ApplicationStatus" :: String)),
                    readOnlyProperty "IconName" (pure ("folder" :: String)),
                    readOnlyProperty "OverlayIconName" (pure ("" :: String)),
                    readOnlyProperty "ItemIsMenu" (pure False)
                  ],
                interfaceSignals = []
              }
          itemAName = "org.test.HostLogicalDuplicateA"
          itemBName = "org.test.HostLogicalDuplicateB"
      itemClientA <- connectSession
      export itemClientA (objectPath_ defaultPath) duplicateIface
      _ <- requestName itemClientA (busName_ itemAName) []
      WatcherClient.registerStatusNotifierItem itemClientA itemAName
        `shouldReturn` Right ()

      initialReady <-
        waitFor 1500000 $ do
          current <- itemInfoMap host
          pure $ Map.size current == 1 && Map.member (busName_ itemAName) current
      initialReady `shouldBe` True

      itemClientB <- connectSession
      export itemClientB (objectPath_ defaultPath) duplicateIface
      _ <- requestName itemClientB (busName_ itemBName) []
      WatcherClient.registerStatusNotifierItem itemClientB itemBName
        `shouldReturn` Right ()

      deduped <-
        waitFor 1500000 $ do
          current <- itemInfoMap host
          pure $
            Map.size current == 1
              && Map.member (busName_ itemBName) current
              && not (Map.member (busName_ itemAName) current)
      deduped `shouldBe` True

      _ <- releaseName itemClientA (busName_ itemAName)
      _ <- releaseName itemClientB (busName_ itemBName)
      pure ()

    it "keeps distinct items when shared ids differ by title or icon" $ \() -> do
      _watcher <- startWatcher
      hostClient <- connectSession
      Just host <- build defaultParams {dbusClient = Just hostClient, uniqueIdentifier = "host-h3"}

      let makeIface :: String -> String -> Interface
          makeIface title iconName =
            Interface
              { interfaceName = "org.kde.StatusNotifierItem",
                interfaceMethods = [],
                interfaceProperties =
                  [ readOnlyProperty "Id" (pure ("git-sync-rs" :: String)),
                    readOnlyProperty "Title" (pure title),
                    readOnlyProperty "Category" (pure ("ApplicationStatus" :: String)),
                    readOnlyProperty "IconName" (pure iconName),
                    readOnlyProperty "OverlayIconName" (pure ("" :: String)),
                    readOnlyProperty "ItemIsMenu" (pure True)
                  ],
                interfaceSignals = []
              }
          itemAName = "org.test.HostDistinctSharedA"
          itemBName = "org.test.HostDistinctSharedB"
      itemClientA <- connectSession
      export itemClientA (objectPath_ defaultPath) (makeIface "git-sync-rs - org" "text-org")
      _ <- requestName itemClientA (busName_ itemAName) []
      WatcherClient.registerStatusNotifierItem itemClientA itemAName
        `shouldReturn` Right ()

      itemClientB <- connectSession
      export itemClientB (objectPath_ defaultPath) (makeIface "git-sync-rs - .password-store" "password")
      _ <- requestName itemClientB (busName_ itemBName) []
      WatcherClient.registerStatusNotifierItem itemClientB itemBName
        `shouldReturn` Right ()

      bothPresent <-
        waitFor 1500000 $ do
          current <- itemInfoMap host
          pure $
            Map.size current == 2
              && Map.member (busName_ itemAName) current
              && Map.member (busName_ itemBName) current
      bothPresent `shouldBe` True

      _ <- releaseName itemClientA (busName_ itemAName)
      _ <- releaseName itemClientB (busName_ itemBName)
      pure ()

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
              { interfaceName = "org.kde.StatusNotifierItem",
                interfaceMethods = [],
                interfaceProperties =
                  [ readOnlyProperty "IconName" $ do
                      void $ tryPutMVar buildStarted ()
                      takeMVar continueBuild
                      pure ("folder" :: String),
                    readOnlyProperty "OverlayIconName" (pure ("" :: String)),
                    readOnlyProperty "ItemIsMenu" (pure False)
                  ],
                interfaceSignals = []
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

    it "suppresses property update signals when the item value is unchanged" $ \() -> do
      _watcher <- startWatcher
      hostClient <- connectSession
      Just host <- build defaultParams {dbusClient = Just hostClient, uniqueIdentifier = "host-k"}

      let itemName = "org.test.HostNoopTitleUpdate"
      titleVar <- newMVar ("Initial Title" :: String)
      itemClient <- connectSession
      let emitNewTitle =
            emit itemClient $
              signal
                (objectPath_ defaultPath)
                (interfaceName_ "org.kde.StatusNotifierItem")
                (memberName_ "NewTitle")
          iface =
            Interface
              { interfaceName = "org.kde.StatusNotifierItem",
                interfaceMethods = [],
                interfaceProperties =
                  [ readOnlyProperty "Title" (readMVar titleVar),
                    readOnlyProperty "IconName" (pure ("folder" :: String)),
                    readOnlyProperty "OverlayIconName" (pure ("" :: String)),
                    readOnlyProperty "ItemIsMenu" (pure False)
                  ],
                interfaceSignals = []
              }
      export itemClient (objectPath_ defaultPath) iface
      _ <- requestName itemClient (busName_ itemName) []
      WatcherClient.registerStatusNotifierItem itemClient itemName
        `shouldReturn` Right ()

      events <- newChan
      _ <- addUpdateHandler host (\ut info -> writeChan events (ut, iconTitle info))

      added <- timeout 1500000 (readChan events)
      added `shouldBe` Just (ItemAdded, "Initial Title")

      emitNewTitle
      unchanged <- timeout 300000 (readChan events)
      unchanged `shouldBe` Nothing

      _ <- swapMVar titleVar "Updated Title"
      emitNewTitle
      updated <- timeout 1500000 (readChan events)
      updated `shouldBe` Just (TitleUpdated, "Updated Title")

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
    { M.methodErrorName = errName,
      M.methodErrorSerial = Serial 0,
      M.methodErrorSender = Nothing,
      M.methodErrorDestination = Nothing,
      M.methodErrorBody = []
    }

errorInvalidArgs :: ErrorName
errorInvalidArgs = errorName_ "org.freedesktop.DBus.Error.InvalidArgs"
