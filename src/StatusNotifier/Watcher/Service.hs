{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module StatusNotifier.Watcher.Service where

import           Control.Arrow
import           Control.Concurrent.MVar
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           DBus
import           DBus.Client
import           DBus.Generation
import           DBus.Internal.Message as M
import           DBus.Internal.Types
import qualified DBus.Internal.Types as T
import qualified DBus.Introspection as I
import qualified DBus.TH as DBusTH
import           Data.Coerce
import           Data.Int
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.String
import qualified StatusNotifier.Item.Client as Item
import           StatusNotifier.Util
import           StatusNotifier.Watcher.Constants
import           StatusNotifier.Watcher.StateCache
import           StatusNotifier.Watcher.Signals
import           System.IO.Unsafe
import           System.Log.Logger
import           Text.Printf

buildWatcher WatcherParams
               { watcherNamespace = interfaceNamespace
               , watcherStop = stopWatcher
               , watcherPath = path
               , watcherDBusClient = mclient
               , watcherStateCachePath = maybeCachePath
               } = do
  let watcherInterfaceName = getWatcherInterfaceName interfaceNamespace
      logNamespace = "StatusNotifier.Watcher.Service"
      logInfo = logM logNamespace INFO
      logDebug = logM logNamespace DEBUG
      logError = logM logNamespace ERROR
      -- Default level is now INFO (watcher/Main.hs). Keep generic per-request
      -- logging at DEBUG to avoid spamming INFO for frequent property reads.
      mkLogCb cb msg = lift (logDebug (show msg)) >> cb msg
      mkLogMethod method = method { methodHandler = mkLogCb $ methodHandler method }
      mkLogProperty name fn =
        readOnlyProperty name $ logDebug (coerce name ++ " Called") >> fn

  client <- maybe connectSession return mclient
  cachePath <- maybe (defaultWatcherStateCachePath interfaceNamespace path)
                     pure
                     maybeCachePath

  notifierItems <- newMVar []
  notifierHosts <- newMVar []

  let itemIsRegistered item items =
        isJust $ find (== item) items

      persistWatcherState = do
        currentItems <- readMVar notifierItems
        currentHosts <- readMVar notifierHosts
        let toPersisted entry =
              PersistedItemEntry
                { persistedServiceName = coerce (serviceName entry)
                , persistedServicePath = coerce (servicePath entry)
                }
            persistedState =
              PersistedWatcherState
                { persistedItems = map toPersisted currentItems
                , persistedHosts = map toPersisted currentHosts
                }
        writePersistedWatcherState cachePath persistedState >>=
          either
            (\err ->
               logError $
                 printf "Failed to persist watcher state to %s: %s" cachePath err
            )
            (const $ return ())

      renderServiceName :: ItemEntry -> String
      renderServiceName ItemEntry { serviceName = busName
                                  , servicePath = path
                                  } =
        let bus = (coerce busName :: String)
            objPath = (coerce path :: String)
            defaultPath = (coerce Item.defaultPath :: String)
        in if objPath == defaultPath
           then bus
           else bus ++ objPath

      resolveOwner bus
        | ":" `isPrefixOf` (coerce bus :: String) = return (Just bus)
        | otherwise = do
            result <- DBusTH.getNameOwner client (coerce bus)
            return $ busName_ <$> either (const Nothing) Just result

      insertItemNoSignal owner item currentItems = do
        ownerPathMatches <-
          case owner of
            Nothing -> return []
            Just itemOwner ->
              filterM
                (\existingItem ->
                   if servicePath existingItem == servicePath item
                     then do
                       existingOwner <- resolveOwner (serviceName existingItem)
                       return $ existingOwner == Just itemOwner
                     else return False
                )
                currentItems
        if itemIsRegistered item currentItems
          then return (currentItems, False)
          else
            if null ownerPathMatches
              then return (item : currentItems, True)
              else
                let removeMatches =
                      foldl' (flip delete) currentItems ownerPathMatches
                in return (item : removeMatches, True)

      insertHostNoSignal host currentHosts =
        if itemIsRegistered host currentHosts
          then (currentHosts, False)
          else (host : currentHosts, True)

      parsePersistedItemEntry persistedEntry = do
        parsedBusName <- T.parseBusName (persistedServiceName persistedEntry)
        parsedPath <- T.parseObjectPath (persistedServicePath persistedEntry)
        return ItemEntry
          { serviceName = parsedBusName
          , servicePath = parsedPath
          }

      statusNotifierItemInterfaceName = fromString "org.kde.StatusNotifierItem"
      hasStatusNotifierItemInterface objectInfo =
        any ((== statusNotifierItemInterfaceName) . I.interfaceName) $
        I.objectInterfaces objectInfo

      validatePersistedItem persistedEntry =
        case parsePersistedItemEntry persistedEntry of
          Nothing -> do
            logError $ printf "Dropping unparsable cached item entry: %s" $
              show persistedEntry
            return Nothing
          Just item -> do
            owner <- resolveOwner (serviceName item)
            case owner of
              Nothing -> do
                logInfo $
                  printf "Dropping cached item %s because the bus name is no longer owned."
                    (renderServiceName item)
                return Nothing
              Just validOwner -> do
                objectResult <-
                  getInterfaceAt client (serviceName item) (servicePath item)
                case objectResult of
                  Right (Just objectInfo)
                    | hasStatusNotifierItemInterface objectInfo ->
                        return $ Just (item, validOwner)
                  _ -> do
                    logInfo $
                      printf "Dropping cached item %s because it no longer exposes org.kde.StatusNotifierItem."
                        (renderServiceName item)
                    return Nothing

      validatePersistedHost persistedEntry =
        case parsePersistedItemEntry persistedEntry of
          Nothing -> do
            logError $ printf "Dropping unparsable cached host entry: %s" $
              show persistedEntry
            return Nothing
          Just host -> do
            owner <- resolveOwner (serviceName host)
            if isNothing owner
              then do
                logInfo $
                  printf "Dropping cached host %s because the bus name is no longer owned."
                    (coerce (serviceName host) :: String)
                return Nothing
              else return $ Just host

      restoreWatcherStateFromCache = do
        stateResult <- readPersistedWatcherState cachePath
        case stateResult of
          Left err -> do
            logError $
              printf "Failed to read watcher cache state from %s: %s" cachePath err
            return ([], [])
          Right Nothing -> return ([], [])
          Right (Just persistedState) -> do
            validItems <- catMaybes <$> mapM validatePersistedItem (persistedItems persistedState)
            validHosts <- catMaybes <$> mapM validatePersistedHost (persistedHosts persistedState)

            restoredItems <-
              modifyMVar notifierItems $ \currentItems -> do
                (newItems, insertedItemsRev) <-
                  foldM
                    (\(accItems, accInserted) (item, itemOwner) -> do
                       (nextItems, inserted) <- insertItemNoSignal (Just itemOwner) item accItems
                       if inserted
                         then return (nextItems, item : accInserted)
                         else return (nextItems, accInserted)
                    )
                    (currentItems, [])
                    validItems
                return (newItems, reverse insertedItemsRev)

            restoredHosts <-
              modifyMVar notifierHosts $ \currentHosts -> do
                let insertHost (accHosts, accInserted) host =
                      let (nextHosts, inserted) = insertHostNoSignal host accHosts
                      in if inserted
                           then (nextHosts, host : accInserted)
                           else (nextHosts, accInserted)
                    (newHosts, insertedHostsRev) = foldl' insertHost (currentHosts, []) validHosts
                return (newHosts, reverse insertedHostsRev)

            persistWatcherState
            return (restoredItems, restoredHosts)

      registerStatusNotifierItem MethodCall
                                   { methodCallSender = sender }
                                 name = runExceptT $ do
        let parsedBusName = T.parseBusName name
            parseServiceError = makeErrorReply errorInvalidParameters $
              printf "the provided service %s could not be parsed \
                     \as a bus name or an object path." name
            senderMissingError = makeErrorReply errorInvalidParameters $
              "Unable to identify sender for registration."
            path = fromMaybe Item.defaultPath $ T.parseObjectPath name
            remapErrorName =
              left $ (`makeErrorReply` "Failed to verify ownership.") .
                   M.methodErrorName
        when (isNothing parsedBusName && isNothing (T.parseObjectPath name)) $
          throwE parseServiceError
        senderName <- ExceptT $ return $ maybeToEither senderMissingError sender
        busName <-
          case parsedBusName of
            Just providedBusName
              -- Unique bus names (starting with ':') are assigned by the D-Bus
              -- daemon and cannot be spoofed. Some applications (e.g. KDE's
              -- KStatusNotifierItem) register their SNI on a separate D-Bus
              -- connection, so the sender's unique name differs from the
              -- registered item's unique name even though they belong to the
              -- same process. Skip the ownership check for unique names.
              | ":" `isPrefixOf` (coerce providedBusName :: String) ->
                  return providedBusName
              | otherwise -> do
                  owner <- ExceptT $ remapErrorName <$>
                           DBusTH.getNameOwner client (coerce providedBusName)
                  unless (owner == coerce senderName) $
                    throwE $ makeErrorReply errorInvalidParameters $
                      printf "Sender %s does not own service %s."
                        (coerce senderName :: String)
                        name
                  return providedBusName
            Nothing -> return senderName
        let item = ItemEntry { serviceName = busName
                             , servicePath = path
                             }
        changed <- lift $ modifyMVar notifierItems $ \currentItems -> do
          (newItems, inserted) <- insertItemNoSignal (Just senderName) item currentItems
          return (newItems, inserted)
        lift $
          when changed $ do
            logInfo $ printf "Registered item %s." (renderServiceName item)
            emitStatusNotifierItemRegistered client $ renderServiceName item
            persistWatcherState

      registerStatusNotifierHost name =
        let item = ItemEntry { serviceName = busName_ name
                             , servicePath = "/StatusNotifierHost"
                             } in
        do
          changed <- modifyMVar notifierHosts $ \currentHosts ->
            let (newHosts, inserted) = insertHostNoSignal item currentHosts
            in return (newHosts, inserted)
          when changed $ do
            logInfo $ printf "Registered host %s." name
            emitStatusNotifierHostRegistered client
            persistWatcherState

      registeredStatusNotifierItems :: IO [String]
      registeredStatusNotifierItems =
        map renderServiceName <$> readMVar notifierItems

      registeredSNIEntries :: IO [(String, String)]
      registeredSNIEntries =
        map getTuple <$> readMVar notifierItems
          where getTuple (ItemEntry bname path) = (coerce bname, coerce path)

      objectPathForItem :: String -> IO (Either Reply String)
      objectPathForItem name =
        case splitServiceName name of
          (_, Just path) -> return $ Right path
          (bus, Nothing) ->
            maybeToEither notFoundError . fmap (coerce . servicePath) .
            find ((== busName_ bus) . serviceName) <$>
            readMVar notifierItems
        where notFoundError =
                makeErrorReply errorInvalidParameters $
                printf "Service %s is not registered." name

      isStatusNotifierHostRegistered = not . null <$> readMVar notifierHosts

      protocolVersion = return 0 :: IO Int32

      filterDeadService :: String -> MVar [ItemEntry] -> IO [ItemEntry]
      filterDeadService deadService mvar = modifyMVar mvar $
        return . partition ((/= busName_ deadService) . serviceName)

      handleNameOwnerChanged _ name oldOwner newOwner =
        when (newOwner == "") $ do
          removedItems <- filterDeadService name notifierItems
          unless (null removedItems) $ do
            logInfo $ printf "Unregistering item %s because it disappeared." name
            forM_ removedItems $ \item ->
              emitStatusNotifierItemUnregistered client $ renderServiceName item
          removedHosts <- filterDeadService name notifierHosts
          unless (null removedHosts) $
            logInfo $ printf "Unregistering host %s because it disappeared." name
          when (not (null removedItems) || not (null removedHosts)) $
            persistWatcherState
          return ()

      watcherMethods = map mkLogMethod
        [ autoMethodWithMsg "RegisterStatusNotifierItem"
          registerStatusNotifierItem
        , autoMethod "RegisterStatusNotifierHost"
          registerStatusNotifierHost
        , autoMethod "StopWatcher"
          stopWatcher
        , autoMethod "GetObjectPathForItemName"
          objectPathForItem
        ]

      watcherProperties =
        [ mkLogProperty "RegisteredStatusNotifierItems"
          registeredStatusNotifierItems
        , mkLogProperty "RegisteredSNIEntries"
          registeredSNIEntries
        , mkLogProperty "IsStatusNotifierHostRegistered"
          isStatusNotifierHostRegistered
        , mkLogProperty "ProtocolVersion"
          protocolVersion
        ]

      watcherInterface =
        Interface
        { interfaceName = watcherInterfaceName
        , interfaceMethods = watcherMethods
        , interfaceProperties = watcherProperties
        , interfaceSignals = watcherSignals
        }

      startWatcher = do
        nameRequestResult <- requestName client (coerce watcherInterfaceName) []
        case nameRequestResult of
          NamePrimaryOwner ->
            do
              _ <- DBusTH.registerForNameOwnerChanged client
                   matchAny handleNameOwnerChanged
              (restoredItems, restoredHosts) <- restoreWatcherStateFromCache
              export client (fromString path) watcherInterface
              logInfo $
                printf "Restored %d cached items and %d cached hosts."
                  (length restoredItems)
                  (length restoredHosts)
              mapM_ (emitStatusNotifierItemRegistered client . renderServiceName)
                restoredItems
              mapM_ (const $ emitStatusNotifierHostRegistered client) restoredHosts
          _ -> stopWatcher
        return nameRequestResult

  return (watcherInterface, startWatcher)

-- For Client generation
-- TODO: get rid of unsafePerformIO here by making function that takes mvars so
-- IO isn't needed to build watcher
{-# NOINLINE watcherInterface #-}
watcherInterface = buildIntrospectionInterface clientInterface
  where (clientInterface, _) =
          unsafePerformIO $ buildWatcher
          defaultWatcherParams { watcherDBusClient = Just undefined }
