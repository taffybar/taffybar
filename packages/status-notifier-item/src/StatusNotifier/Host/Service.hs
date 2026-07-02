{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module StatusNotifier.Host.Service where

import Control.Applicative
import Control.Arrow
import Control.Concurrent
import Control.Lens
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import DBus
import DBus.Client
import DBus.Generation
import qualified DBus.Internal.Message as M
import DBus.Internal.Types
import qualified DBus.TH as DTH
import qualified Data.ByteString as BS
import Data.Coerce
import Data.Either
import Data.Int
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.String
import Data.Typeable
import Data.Unique
import qualified StatusNotifier.Item.Client as I
import StatusNotifier.Util
import qualified StatusNotifier.Watcher.Client as W
import qualified StatusNotifier.Watcher.Constants as W
import qualified StatusNotifier.Watcher.Service as W
import qualified StatusNotifier.Watcher.Signals as W
import System.Log.Logger
import Text.Printf

statusNotifierHostString :: String
statusNotifierHostString = "StatusNotifierHost"

getBusName :: String -> String -> String
getBusName namespace =
  printf "%s.%s-%s" namespace statusNotifierHostString

data UpdateType
  = ItemAdded
  | ItemRemoved
  | IconUpdated
  | OverlayIconUpdated
  | StatusUpdated
  | TitleUpdated
  | ToolTipUpdated
  deriving (Eq, Show)

type UpdateHandler = UpdateType -> ItemInfo -> IO ()

data Params = Params
  { dbusClient :: Maybe Client,
    uniqueIdentifier :: String,
    namespace :: String,
    startWatcher :: Bool,
    matchSenderWhenNameOwnersUnmatched :: Bool
  }

hostLogger :: Priority -> String -> IO ()
hostLogger = logM "StatusNotifier.Host.Service"

defaultParams :: Params
defaultParams =
  Params
    { dbusClient = Nothing,
      uniqueIdentifier = "",
      namespace = "org.kde",
      startWatcher = False,
      matchSenderWhenNameOwnersUnmatched = True
    }

type ImageInfo = [(Int32, Int32, BS.ByteString)]

isExpectedPropertyUpdateFailure :: M.MethodError -> Bool
isExpectedPropertyUpdateFailure M.MethodError {M.methodErrorName = errName} =
  errName == errorUnknownMethod
    || errName == errorName_ "org.freedesktop.DBus.Error.InvalidArgs"

propertyUpdateFailureLogLevel :: [M.MethodError] -> [a] -> Priority
propertyUpdateFailureLogLevel failures updates
  | not (null updates) = DEBUG
  | all isExpectedPropertyUpdateFailure failures = DEBUG
  | otherwise = ERROR

data ItemInfo = ItemInfo
  { itemServiceName :: BusName,
    itemServicePath :: ObjectPath,
    itemId :: Maybe String,
    itemStatus :: Maybe String,
    itemCategory :: Maybe String,
    itemToolTip :: Maybe (String, ImageInfo, String, String),
    iconTitle :: String,
    iconName :: String,
    overlayIconName :: Maybe String,
    iconThemePath :: Maybe String,
    iconPixmaps :: ImageInfo,
    overlayIconPixmaps :: ImageInfo,
    menuPath :: Maybe ObjectPath,
    itemIsMenu :: Bool
  }
  deriving (Eq, Show)

data LogicalDuplicateKey = LogicalDuplicateKey
  { duplicateItemId :: Maybe String,
    duplicateIconTitle :: Maybe String,
    duplicateIconName :: Maybe String,
    duplicateCategory :: Maybe String,
    duplicateMenuPath :: Maybe ObjectPath,
    duplicatePath :: ObjectPath,
    duplicateIsMenu :: Bool
  }
  deriving (Eq, Show)

supressPixelData :: ItemInfo -> ItemInfo
supressPixelData info =
  info
    { iconPixmaps = map (\(w, h, _) -> (w, h, "")) $ iconPixmaps info,
      overlayIconPixmaps = map (\(w, h, _) -> (w, h, "")) $ overlayIconPixmaps info
    }

normalizeOptionalString :: String -> Maybe String
normalizeOptionalString value
  | null value = Nothing
  | otherwise = Just value

logicalDuplicateKey :: ItemInfo -> Maybe LogicalDuplicateKey
logicalDuplicateKey ItemInfo {..} =
  let key =
        LogicalDuplicateKey
          { duplicateItemId = itemId >>= normalizeOptionalString,
            duplicateIconTitle = normalizeOptionalString iconTitle,
            duplicateIconName = normalizeOptionalString iconName,
            duplicateCategory = itemCategory >>= normalizeOptionalString,
            duplicateMenuPath = menuPath,
            duplicatePath = itemServicePath,
            duplicateIsMenu = itemIsMenu
          }
      hasStableIdentity =
        isJust (duplicateItemId key)
          || isJust (duplicateIconTitle key)
          || isJust (duplicateIconName key)
   in if hasStableIdentity then Just key else Nothing

findUniqueMatchM :: (a -> IO Bool) -> [a] -> IO (Maybe a)
findUniqueMatchM predicate values = go values Nothing
  where
    go [] match = pure match
    go (value : rest) match = do
      matches <- predicate value
      case (matches, match) of
        (False, _) -> go rest match
        (True, Nothing) -> go rest (Just value)
        (True, Just _) -> pure Nothing

makeLensesWithLSuffix ''ItemInfo

convertPixmapsToHostByteOrder ::
  [(Int32, Int32, BS.ByteString)] -> [(Int32, Int32, BS.ByteString)]
convertPixmapsToHostByteOrder = map $ over _3 networkToSystemByteOrder

callFromInfo :: (BusName -> ObjectPath -> result) -> ItemInfo -> result
callFromInfo
  fn
  ItemInfo
    { itemServiceName = name,
      itemServicePath = path
    } = fn name path

data Host = Host
  { itemInfoMap :: IO (Map.Map BusName ItemInfo),
    addUpdateHandler :: UpdateHandler -> IO Unique,
    removeUpdateHandler :: Unique -> IO (),
    forceUpdate :: BusName -> IO ()
  }
  deriving (Typeable)

-- | How long to wait for a single item's properties when building its
-- 'ItemInfo' before treating the item as unresponsive.
itemBuildTimeout :: Int
itemBuildTimeout = 5 * 1000000

-- | How long to wait for the watcher to answer before giving up on a call. A
-- watcher stuck in its own startup can own the bus name without servicing
-- requests.
watcherCallTimeout :: Int
watcherCallTimeout = 5 * 1000000

build :: Params -> IO (Maybe Host)
build
  Params
    { dbusClient = mclient,
      namespace = namespaceString,
      uniqueIdentifier = uniqueID,
      startWatcher = shouldStartWatcher,
      matchSenderWhenNameOwnersUnmatched = doMatchUnmatchedSender
    } = do
    client <- maybe connectSession return mclient
    itemInfoMapVar <- newMVar Map.empty
    updateHandlersVar <- newMVar ([] :: [(Unique, UpdateHandler)])
    let busName = getBusName namespaceString uniqueID

        logError = hostLogger ERROR
        logErrorWithMessage message err = logError message >> logError (show err)

        logInfo = hostLogger INFO
        logDebug = hostLogger DEBUG
        logErrorAndThen andThen e = logError (show e) >> andThen

        doUpdateForHandler utype uinfo (unique, handler) = do
          -- This is extremely chatty under normal operation; keep it at DEBUG.
          logDebug
            ( printf
                "Sending update (iconPixmaps suppressed): %s %s, for handler %s"
                (show utype)
                (show $ supressPixelData uinfo)
                (show $ hashUnique unique)
            )
          forkIO $ handler utype uinfo

        doUpdate utype uinfo =
          readMVar updateHandlersVar >>= mapM_ (doUpdateForHandler utype uinfo)

        addUpdateHandler_ handler = do
          unique <- newUnique
          modifyMVar_ itemInfoMapVar $ \itemInfoMap -> do
            logInfo $
              printf
                "Registering update handler %s with %d replay items: %s"
                (show $ hashUnique unique)
                (Map.size itemInfoMap)
                (show $ Map.keys itemInfoMap)
            -- Register and replay under the item map lock so a concurrent add
            -- cannot be delivered both live and via replay.
            modifyMVar_ updateHandlersVar (return . ((unique, handler) :))
            let doUpdateForInfo info = doUpdateForHandler ItemAdded info (unique, handler)
            mapM_ doUpdateForInfo itemInfoMap
            return itemInfoMap
          return unique

        removeUpdateHandler_ unique =
          modifyMVar_ updateHandlersVar $ \handlers -> do
            let newHandlers = filter ((/= unique) . fst) handlers
            logInfo $
              printf
                "Removing update handler %s; remaining handlers: %d"
                (show $ hashUnique unique)
                (length newHandlers)
            return newHandlers

        getPixmaps getter a1 a2 a3 =
          fmap convertPixmapsToHostByteOrder <$> getter a1 a2 a3

        getMaybe fn a b c = right Just <$> fn a b c

        parseServiceName name =
          let (bus, maybePath) = splitServiceName name
           in (busName_ bus, objectPath_ <$> maybePath)

        -- Bound the full item property fetch: a wedged item process that owns
        -- its bus name but never services its connection would otherwise
        -- block the host (and with it the entire tray build) forever.
        buildItemInfo name =
          callWithTimeout itemBuildTimeout $ buildItemInfoUnbounded name

        buildItemInfoUnbounded name = runExceptT $ do
          let (itemBusName, maybePath) = parseServiceName name
          itemPath <- case maybePath of
            Just parsedPath -> return parsedPath
            Nothing ->
              objectPath_
                <$> ExceptT
                  (W.getObjectPathForItemName client (coerce itemBusName))
          let doGetDef def fn =
                ExceptT $ exemptAll def <$> fn client itemBusName itemPath
          pixmaps <- doGetDef [] $ getPixmaps I.getIconPixmap
          iName <- doGetDef name I.getIconName
          overlayPixmap <- doGetDef [] $ getPixmaps I.getOverlayIconPixmap
          overlayIName <- doGetDef Nothing $ getMaybe I.getOverlayIconName
          themePath <- doGetDef Nothing $ getMaybe I.getIconThemePath
          menu <- doGetDef Nothing $ getMaybe I.getMenu
          title <- doGetDef "" I.getTitle
          tooltip <- doGetDef Nothing $ getMaybe I.getToolTip
          idString <- doGetDef Nothing $ getMaybe I.getId
          status <- doGetDef Nothing $ getMaybe I.getStatus
          category <- doGetDef Nothing $ getMaybe I.getCategory
          itemIsMenu <- doGetDef True I.getItemIsMenu
          return
            ItemInfo
              { itemServiceName = itemBusName,
                itemId = idString,
                itemStatus = status,
                itemCategory = category,
                itemServicePath = itemPath,
                itemToolTip = tooltip,
                iconPixmaps = pixmaps,
                iconThemePath = themePath,
                iconName = iName,
                iconTitle = title,
                menuPath = menu,
                overlayIconName = overlayIName,
                overlayIconPixmaps = overlayPixmap,
                itemIsMenu = itemIsMenu
              }

        createAll serviceNames = do
          (errors, itemInfos) <-
            partitionEithers <$> mapM buildItemInfo serviceNames
          mapM_ (logErrorWithMessage "Error in item building at startup:") errors
          return itemInfos

        registerWithPairs =
          mapM (uncurry clientSignalRegister)
          where
            logUnableToCallSignal dbusSignal =
              hostLogger ERROR $
                printf "Unable to call handler with %s" $
                  show dbusSignal
            clientSignalRegister signalRegisterFn handler =
              signalRegisterFn client matchAny handler logUnableToCallSignal

        -- Resolve a bus name to its unique name owner when it is well-known.
        -- For unique names (":1.42"), the owner is the name itself.
        resolveOwner :: BusName -> IO (Maybe BusName)
        resolveOwner bus =
          case (coerce bus :: String) of
            ':' : _ -> pure (Just bus)
            _ ->
              either (const Nothing) (Just . busName_)
                <$> DTH.getNameOwner client (coerce bus)

        handleItemAdded serviceName =
          modifyMVar_ itemInfoMapVar $ \itemInfoMap ->
            buildItemInfo serviceName
              >>= either
                (logErrorAndThen $ return itemInfoMap)
                (addItemInfo itemInfoMap)
          where
            addItemInfo
              currentMap
              itemInfo@ItemInfo
                { itemServiceName = newName,
                  itemServicePath = newPath
                } =
                if Map.member newName currentMap
                  then do
                    logDebug $
                      printf
                        "Ignoring duplicate ItemAdded for %s because the exact service is already tracked."
                        (coerce newName :: String)
                    return currentMap
                  else do
                    -- When the watcher restarts, some items may re-register
                    -- under a different bus name (e.g. switching between
                    -- unique name and a well-known name). Detect that by
                    -- matching on the item's unique name owner, and replace
                    -- the existing entry rather than creating a duplicate.
                    newOwner <- resolveOwner newName
                    let matchesOwnerAndPath (existingName, existingInfo) = do
                          existingOwner <- resolveOwner existingName
                          pure $
                            existingName /= newName
                              && existingOwner == newOwner
                              && itemServicePath existingInfo == newPath
                        matchesLogicalDuplicate (existingName, existingInfo) =
                          pure $
                            existingName /= newName
                              && logicalDuplicateKey existingInfo == logicalDuplicateKey itemInfo
                        addFresh = do
                          doUpdate ItemAdded itemInfo
                          pure (Map.insert newName itemInfo currentMap)
                        replaceExisting :: BusName -> ItemInfo -> String -> IO (Map.Map BusName ItemInfo)
                        replaceExisting existingName existingInfo reason = do
                          logInfo $
                            printf
                              "Replacing tracked item %s with %s for %s"
                              (coerce existingName :: String)
                              (coerce newName :: String)
                              reason
                          doUpdate ItemRemoved existingInfo
                          doUpdate ItemAdded itemInfo
                          pure $
                            Map.insert newName itemInfo $
                              Map.delete existingName currentMap
                        tryLogicalDuplicateMatch =
                          case logicalDuplicateKey itemInfo of
                            Nothing -> addFresh
                            Just duplicateKey -> do
                              existing <- findUniqueMatchM matchesLogicalDuplicate (Map.toList currentMap)
                              case existing of
                                Just (existingName, existingInfo) ->
                                  replaceExisting
                                    existingName
                                    existingInfo
                                    (printf "logical duplicate key %s" (show duplicateKey))
                                Nothing -> addFresh
                    case newOwner of
                      Nothing -> tryLogicalDuplicateMatch
                      Just _ -> do
                        existing <- findM matchesOwnerAndPath (Map.toList currentMap)
                        case existing of
                          Nothing -> do
                            logDebug $
                              printf
                                "Tracking fresh item %s at %s"
                                (coerce newName :: String)
                                (coerce newPath :: String)
                            tryLogicalDuplicateMatch
                          Just (existingName, existingInfo) ->
                            replaceExisting
                              existingName
                              existingInfo
                              (printf "shared owner/path at %s" (coerce newPath :: String))

        getObjectPathForItemName name =
          maybe I.defaultPath itemServicePath . Map.lookup name
            <$> readMVar itemInfoMapVar

        handleItemRemoved serviceName =
          modifyMVar itemInfoMapVar doRemove
            >>= maybe logNonExistentRemoval (doUpdate ItemRemoved)
          where
            itemBusName = fst (parseServiceName serviceName)
            doRemove currentMap =
              return (Map.delete itemBusName currentMap, Map.lookup itemBusName currentMap)
            logNonExistentRemoval =
              -- This can happen due to watcher/host races (e.g. watcher restart).
              hostLogger DEBUG $
                printf "Attempt to remove unknown item %s" $
                  show itemBusName

        watcherRegistrationPairs =
          [ (W.registerForStatusNotifierItemRegistered, const handleItemAdded),
            (W.registerForStatusNotifierItemUnregistered, const handleItemRemoved)
          ]

        watcherServiceName = coerce $ W.getWatcherInterfaceName namespaceString

        synchronizeItemsWithWatcher = do
          let retryDelayMicros = 50000
              maxRetries :: Int
              maxRetries = 20
              fetchWatcherItems retries = do
                watcherItemsResult <-
                  callWithTimeout watcherCallTimeout $
                    W.getRegisteredStatusNotifierItems client
                case watcherItemsResult of
                  Right watcherItems -> return $ Right watcherItems
                  Left err ->
                    if retries <= 0
                      then return $ Left err
                      else
                        threadDelay retryDelayMicros
                          >> fetchWatcherItems (retries - 1)
          watcherItemsResult <- fetchWatcherItems maxRetries
          case watcherItemsResult of
            Left err ->
              logErrorWithMessage "Failed to synchronize host state with watcher:" err
            Right watcherServiceNames -> do
              currentMap <- readMVar itemInfoMapVar
              let watcherBusNames = map (fst . parseServiceName) watcherServiceNames
                  missingFromWatcher =
                    filter (`notElem` watcherBusNames) (Map.keys currentMap)
              mapM_ (handleItemRemoved . coerce) missingFromWatcher
              mapM_ handleItemAdded watcherServiceNames

        handleWatcherNameOwnerChanged _ name _ newOwner =
          when (name == watcherServiceName && newOwner /= "") $ do
            logInfo "Watcher owner changed; synchronizing item state."
            synchronizeItemsWithWatcher

        runProperty prop serviceName =
          getObjectPathForItemName serviceName >>= prop client serviceName

        logUnknownSender updateType dbusSignal =
          hostLogger DEBUG $
            printf
              "Got signal for update type: %s from unknown sender: %s"
              (show updateType)
              (show dbusSignal)

        identifySender
          M.Signal
            { M.signalSender = Just sender,
              M.signalPath = senderPath
            } = do
            infoMap <- readMVar itemInfoMapVar
            let identifySenderBySender = return (Map.lookup sender infoMap)
                identifySenderByNameOwner =
                  let matchByOwner info = do
                        ownerResult <-
                          DTH.getNameOwner
                            client
                            (coerce $ itemServiceName info)
                        return $ case ownerResult of
                          Right owner -> owner == coerce sender
                          Left _ -> False
                   in findM matchByOwner (Map.elems infoMap)
                identifySenderById =
                  fmap join $
                    identifySenderById_ >>= logIdentifyByIdResult
                logIdentifyByIdResult
                  (Left M.MethodError {M.methodErrorName = errName})
                    | errName == errorUnknownMethod =
                        hostLogger
                          DEBUG
                          ( printf
                              "Item does not support getId: %s"
                              (show errName)
                          )
                          >> return Nothing
                logIdentifyByIdResult result =
                  logEitherError hostLogger "Failed to identify sender" result
                identifySenderById_ = runExceptT $ do
                  senderId <- ExceptT $ I.getId client sender senderPath
                  let matchesSender info =
                        if itemId info == Just senderId
                          then do
                            senderNameOwner <- DTH.getNameOwner client (coerce sender)
                            infoNameOwner <- DTH.getNameOwner client (coerce $ itemServiceName info)
                            let warningMsg =
                                  "Matched sender id: %s, but name owners do not \
                                  \ match: %s %s. Considered match: %s."
                                warningText =
                                  printf
                                    warningMsg
                                    (show senderId)
                                    (show senderNameOwner)
                                    (show infoNameOwner)
                            when (senderNameOwner /= infoNameOwner) $
                              hostLogger DEBUG warningText
                            return doMatchUnmatchedSender
                          else return False
                  lift $ findM matchesSender (Map.elems infoMap)
            identifySenderBySender <||> identifySenderByNameOwner <||> identifySenderById
            where
              a <||> b = runMaybeT $ MaybeT a <|> MaybeT b
        identifySender _ = return Nothing

        updateItemByLensAndProp valueLens prop itemBusName = runExceptT $ do
          newValue <- ExceptT (runProperty prop itemBusName)
          let modify infoMap =
                case Map.lookup itemBusName infoMap of
                  Nothing -> return (infoMap, Nothing)
                  Just oldInfo ->
                    let newInfo = set valueLens newValue oldInfo
                     in if newInfo == oldInfo
                          then return (infoMap, Just Nothing)
                          else
                            let newMap = Map.insert itemBusName newInfo infoMap
                             in return (newMap, Just $ Just newInfo)
          ExceptT $
            maybeToEither (methodError (Serial 0) errorFailed)
              <$> modifyMVar itemInfoMapVar modify

        logErrorsHandler valueLens updateType prop =
          runUpdaters [updateItemByLensAndProp valueLens prop] updateType

        -- Run all the provided updaters with the expectation that at least one
        -- will succeed.
        runUpdatersForService updaters updateType serviceName = do
          updateResults <- mapM ($ serviceName) updaters
          let (failures, successes) = partitionEithers updateResults
              logLevel = propertyUpdateFailureLogLevel failures successes
          mapM_ (doUpdate updateType) $ catMaybes successes
          unless (null failures) $
            hostLogger logLevel $
              printf "Property update failures %s" $
                show failures

        runUpdaters updaters updateType dbusSignal =
          identifySender dbusSignal >>= maybe runForAll (runUpdateForService . itemServiceName)
          where
            runUpdateForService = runUpdatersForService updaters updateType
            runForAll =
              logUnknownSender updateType dbusSignal
                >> readMVar itemInfoMapVar
                >>= mapM_ runUpdateForService . Map.keys

        updateIconPixmaps =
          updateItemByLensAndProp iconPixmapsL $ getPixmaps I.getIconPixmap

        updateIconName =
          updateItemByLensAndProp iconNameL I.getIconName

        updateIconTheme =
          updateItemByLensAndProp iconThemePathL getThemePathDefault

        updateFromIconThemeFromSignal dbusSignal =
          identifySender dbusSignal >>= traverse (updateIconTheme . itemServiceName)

        handleNewIcon dbusSignal = do
          -- XXX: This avoids the case where the theme path is updated before the
          -- icon name is updated when both signals are sent simultaneously
          _ <- updateFromIconThemeFromSignal dbusSignal
          runUpdaters
            [updateIconPixmaps, updateIconName]
            IconUpdated
            dbusSignal

        updateOverlayIconName =
          updateItemByLensAndProp overlayIconNameL $
            getMaybe I.getOverlayIconName

        updateOverlayIconPixmaps =
          updateItemByLensAndProp overlayIconPixmapsL $
            getPixmaps I.getOverlayIconPixmap

        handleNewOverlayIcon dbusSignal = do
          _ <- updateFromIconThemeFromSignal dbusSignal
          runUpdaters
            [updateOverlayIconPixmaps, updateOverlayIconName]
            OverlayIconUpdated
            dbusSignal

        getThemePathDefault dbusClient itemBusName objectPath =
          right Just <$> I.getIconThemePath dbusClient itemBusName objectPath

        handleNewTitle =
          logErrorsHandler iconTitleL TitleUpdated I.getTitle

        handleNewTooltip =
          logErrorsHandler itemToolTipL ToolTipUpdated $ getMaybe I.getToolTip

        handleNewStatus =
          logErrorsHandler itemStatusL StatusUpdated $ getMaybe I.getStatus

        clientRegistrationPairs =
          [ (I.registerForNewIcon, handleNewIcon),
            (I.registerForNewIconThemePath, handleNewIcon),
            (I.registerForNewOverlayIcon, handleNewOverlayIcon),
            (I.registerForNewTitle, handleNewTitle),
            (I.registerForNewToolTip, handleNewTooltip),
            (I.registerForNewStatus, handleNewStatus)
          ]

        initializeItemInfoMap = modifyMVar itemInfoMapVar $ \itemInfoMap -> do
          -- All initialization is done inside this modifyMVar to avoid race
          -- conditions with the itemInfoMapVar.
          clientSignalHandlers <- registerWithPairs clientRegistrationPairs
          watcherSignalHandlers <- registerWithPairs watcherRegistrationPairs
          watcherOwnerChangedHandler <-
            DTH.registerForNameOwnerChanged client matchAny handleWatcherNameOwnerChanged
          let unregisterAll =
                mapM_ (removeMatch client) $
                  watcherOwnerChangedHandler : clientSignalHandlers ++ watcherSignalHandlers
              shutdownHost = do
                logInfo "Shutting down StatusNotifierHost"
                unregisterAll
                _ <- releaseName client (fromString busName)
                return ()
              logErrorAndShutdown err =
                logError (show err) >> shutdownHost >> return (Map.empty, False)
              finishInitialization serviceNames = do
                itemInfos <- createAll serviceNames
                let newMap = Map.fromList $ map (itemServiceName &&& id) itemInfos
                    resultMap = Map.union itemInfoMap newMap
                callWithTimeout watcherCallTimeout (W.registerStatusNotifierHost client busName)
                  >>= either logErrorAndShutdown (const $ return (resultMap, True))
          callWithTimeout watcherCallTimeout (W.getRegisteredStatusNotifierItems client)
            >>= either logErrorAndShutdown finishInitialization

        startWatcherIfNeeded = do
          let watcherName = maybe "" coerce $ genBusName W.watcherClientGenerationParams
              startWatcher = do
                (_, doIt) <- W.buildWatcher W.defaultWatcherParams
                doIt
          res <- DTH.getNameOwner client watcherName
          case res of
            Right _ -> return ()
            Left _ -> void $ forkIO $ void startWatcher

    when shouldStartWatcher startWatcherIfNeeded
    nameRequestResult <- requestName client (fromString busName) []
    if nameRequestResult == NamePrimaryOwner
      then do
        initializationSuccess <- initializeItemInfoMap
        return $
          if initializationSuccess
            then
              Just
                Host
                  { itemInfoMap = readMVar itemInfoMapVar,
                    addUpdateHandler = addUpdateHandler_,
                    removeUpdateHandler = removeUpdateHandler_,
                    forceUpdate = handleItemAdded . coerce
                  }
            else Nothing
      else do
        logErrorWithMessage "Failed to obtain desired service name" nameRequestResult
        return Nothing
