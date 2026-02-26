{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module StatusNotifier.Host.Service where

import           Control.Applicative
import           Control.Arrow
import           Control.Concurrent
import           Control.Concurrent.MVar
import           Control.Lens
import           Control.Lens.Tuple
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Maybe
import           DBus
import           DBus.Client
import           DBus.Generation
import qualified DBus.Internal.Message as M
import           DBus.Internal.Types
import qualified DBus.TH as DTH
import qualified Data.ByteString as BS
import           Data.Coerce
import           Data.Either
import           Data.Int
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.String
import           Data.Typeable
import           Data.Unique
import           Data.Word
import           System.Log.Logger
import           Text.Printf

import qualified StatusNotifier.Item.Client as I
import           StatusNotifier.Util
import qualified StatusNotifier.Watcher.Client as W
import qualified StatusNotifier.Watcher.Constants as W
import qualified StatusNotifier.Watcher.Signals as W
import qualified StatusNotifier.Watcher.Service as W

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
  | ToolTipUpdated deriving (Eq, Show)

type UpdateHandler = UpdateType -> ItemInfo -> IO ()

data Params = Params
  { dbusClient :: Maybe Client
  , uniqueIdentifier :: String
  , namespace :: String
  , startWatcher :: Bool
  , matchSenderWhenNameOwnersUnmatched :: Bool
  }

hostLogger = logM "StatusNotifier.Host.Service"

defaultParams = Params
  { dbusClient = Nothing
  , uniqueIdentifier = ""
  , namespace = "org.kde"
  , startWatcher = False
  , matchSenderWhenNameOwnersUnmatched = True
  }

type ImageInfo = [(Int32, Int32, BS.ByteString)]

isExpectedPropertyUpdateFailure :: M.MethodError -> Bool
isExpectedPropertyUpdateFailure M.MethodError { M.methodErrorName = errName } =
  errName == errorUnknownMethod ||
  errName == errorName_ "org.freedesktop.DBus.Error.InvalidArgs"

propertyUpdateFailureLogLevel :: [M.MethodError] -> [a] -> Priority
propertyUpdateFailureLogLevel failures updates
  | not (null updates) = DEBUG
  | all isExpectedPropertyUpdateFailure failures = DEBUG
  | otherwise = ERROR

data ItemInfo = ItemInfo
  { itemServiceName :: BusName
  , itemServicePath :: ObjectPath
  , itemId :: Maybe String
  , itemStatus :: Maybe String
  , itemCategory :: Maybe String
  , itemToolTip :: Maybe (String, ImageInfo, String, String)
  , iconTitle :: String
  , iconName :: String
  , overlayIconName :: Maybe String
  , iconThemePath :: Maybe String
  , iconPixmaps :: ImageInfo
  , overlayIconPixmaps :: ImageInfo
  , menuPath :: Maybe ObjectPath
  , itemIsMenu :: Bool
  } deriving (Eq, Show)

supressPixelData info =
  info { iconPixmaps = map (\(w, h, _) -> (w, h, "")) $ iconPixmaps info }

makeLensesWithLSuffix ''ItemInfo

convertPixmapsToHostByteOrder ::
  [(Int32, Int32, BS.ByteString)] -> [(Int32, Int32, BS.ByteString)]
convertPixmapsToHostByteOrder = map $ over _3 networkToSystemByteOrder

callFromInfo fn ItemInfo { itemServiceName = name
                         , itemServicePath = path
                         } = fn name path

data Host = Host
  { itemInfoMap :: IO (Map.Map BusName ItemInfo)
  , addUpdateHandler :: UpdateHandler -> IO Unique
  , removeUpdateHandler :: Unique -> IO ()
  , forceUpdate :: BusName -> IO ()
  } deriving Typeable

build :: Params -> IO (Maybe Host)
build Params { dbusClient = mclient
             , namespace = namespaceString
             , uniqueIdentifier = uniqueID
             , startWatcher = shouldStartWatcher
             , matchSenderWhenNameOwnersUnmatched = doMatchUnmatchedSender
             } = do
  client <- maybe connectSession return mclient
  itemInfoMapVar <- newMVar Map.empty
  updateHandlersVar <- newMVar ([] :: [(Unique, UpdateHandler)])
  let busName = getBusName namespaceString uniqueID

      logError = hostLogger ERROR
      logErrorWithMessage message error = logError message >> logError (show error)

      logInfo = hostLogger INFO
      logDebug = hostLogger DEBUG
      logErrorAndThen andThen e = logError (show e) >> andThen

      doUpdateForHandler utype uinfo (unique, handler) = do
        -- This is extremely chatty under normal operation; keep it at DEBUG.
        logDebug (printf "Sending update (iconPixmaps suppressed): %s %s, for handler %s"
                          (show utype)
                          (show $ supressPixelData uinfo)
                          (show $ hashUnique unique))
        forkIO $ handler utype uinfo

      doUpdate utype uinfo =
        readMVar updateHandlersVar >>= mapM_ (doUpdateForHandler utype uinfo)

      addHandler handler = do
        unique <- newUnique
        modifyMVar_ itemInfoMapVar $ \itemInfoMap -> do
          -- Register and replay under the item map lock so a concurrent add
          -- cannot be delivered both live and via replay.
          modifyMVar_ updateHandlersVar (return . ((unique, handler):))
          let doUpdateForInfo info = doUpdateForHandler ItemAdded info (unique, handler)
          mapM_ doUpdateForInfo itemInfoMap
          return itemInfoMap
        return unique

      removeHandler unique =
        modifyMVar_ updateHandlersVar (return . filter ((/= unique) . fst))

      getPixmaps getter a1 a2 a3 =
        fmap convertPixmapsToHostByteOrder <$> getter a1 a2 a3

      getMaybe fn a b c = right Just <$> fn a b c

      parseServiceName name =
        let (bus, maybePath) = splitServiceName name
        in (busName_ bus, objectPath_ <$> maybePath)

      buildItemInfo name = runExceptT $ do
        let (busName, maybePath) = parseServiceName name
        path <- case maybePath of
          Just parsedPath -> return parsedPath
          Nothing -> objectPath_ <$> ExceptT
            (W.getObjectPathForItemName client (coerce busName))
        let doGetDef def fn =
              ExceptT $ exemptAll def <$> fn client busName path
            doGet fn = ExceptT $ fn client busName path
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
        return ItemInfo
                 { itemServiceName = busName
                 , itemId = idString
                 , itemStatus = status
                 , itemCategory = category
                 , itemServicePath = path
                 , itemToolTip = tooltip
                 , iconPixmaps = pixmaps
                 , iconThemePath = themePath
                 , iconName = iName
                 , iconTitle = title
                 , menuPath = menu
                 , overlayIconName = overlayIName
                 , overlayIconPixmaps = overlayPixmap
                 , itemIsMenu = itemIsMenu
                 }

      createAll serviceNames = do
        (errors, itemInfos) <-
          partitionEithers <$> mapM buildItemInfo serviceNames
        mapM_ (logErrorWithMessage "Error in item building at startup:") errors
        return itemInfos

      registerWithPairs =
        mapM (uncurry clientSignalRegister)
        where logUnableToCallSignal signal =
                hostLogger ERROR $ printf "Unable to call handler with %s" $
                     show signal
              clientSignalRegister signalRegisterFn handler =
                signalRegisterFn client matchAny handler logUnableToCallSignal

      -- Resolve a bus name to its unique name owner when it is well-known.
      -- For unique names (":1.42"), the owner is the name itself.
      resolveOwner :: BusName -> IO (Maybe BusName)
      resolveOwner bus =
        case (coerce bus :: String) of
          ':' : _ -> pure (Just bus)
          _ ->
            either (const Nothing) (Just . busName_) <$>
              DTH.getNameOwner client (coerce bus)

      handleItemAdded serviceName =
        modifyMVar_ itemInfoMapVar $ \itemInfoMap ->
          buildItemInfo serviceName >>=
          either (logErrorAndThen $ return itemInfoMap)
                 (addItemInfo itemInfoMap)
          where addItemInfo map itemInfo@ItemInfo{ itemServiceName = newName
                                                 , itemServicePath = newPath
                                                 } =
                  if Map.member newName map
                  then return map
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
                        addFresh = do
                          doUpdate ItemAdded itemInfo
                          pure (Map.insert newName itemInfo map)
                    case newOwner of
                      Nothing -> addFresh
                      Just _ -> do
                        existing <- findM matchesOwnerAndPath (Map.toList map)
                        case existing of
                          Nothing -> addFresh
                          Just (existingName, existingInfo) -> do
                            doUpdate ItemRemoved existingInfo
                            doUpdate ItemAdded itemInfo
                            pure $
                              Map.insert newName itemInfo $
                                Map.delete existingName map

      getObjectPathForItemName name =
        maybe I.defaultPath itemServicePath . Map.lookup name <$>
        readMVar itemInfoMapVar

      handleItemRemoved serviceName =
        modifyMVar itemInfoMapVar doRemove >>=
        maybe logNonExistentRemoval (doUpdate ItemRemoved)
        where
          busName = fst (parseServiceName serviceName)
          doRemove currentMap =
            return (Map.delete busName currentMap, Map.lookup busName currentMap)
          logNonExistentRemoval =
            -- This can happen due to watcher/host races (e.g. watcher restart).
            hostLogger DEBUG $ printf "Attempt to remove unknown item %s" $
                       show busName

      watcherRegistrationPairs =
        [ (W.registerForStatusNotifierItemRegistered, const handleItemAdded)
        , (W.registerForStatusNotifierItemUnregistered, const handleItemRemoved)
        ]

      watcherServiceName = coerce $ W.getWatcherInterfaceName namespaceString

      synchronizeItemsWithWatcher = do
        let retryDelayMicros = 50000
            maxRetries = 20
            fetchWatcherItems retries = do
              watcherItemsResult <- W.getRegisteredStatusNotifierItems client
              case watcherItemsResult of
                Right watcherItems -> return $ Right watcherItems
                Left err ->
                  if retries <= 0
                    then return $ Left err
                    else threadDelay retryDelayMicros >>
                         fetchWatcherItems (retries - 1)
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

      getSender fn s@M.Signal { M.signalSender = Just sender} =
        -- Signal dumps are too noisy at INFO.
        logDebug (show s) >> fn sender
      getSender _ s = logError $ "Received signal with no sender: " ++ show s

      runProperty prop serviceName =
        getObjectPathForItemName serviceName >>= prop client serviceName

      logUnknownSender updateType signal =
        hostLogger DEBUG $
                   printf "Got signal for update type: %s from unknown sender: %s"
                   (show updateType) (show signal)

      identifySender M.Signal { M.signalSender = Just sender
                              , M.signalPath = senderPath
                              } = do
        infoMap <- readMVar itemInfoMapVar
        let identifySenderBySender = return (Map.lookup sender infoMap)
            identifySenderByNameOwner =
              let matchByOwner info = do
                    ownerResult <- DTH.getNameOwner client
                                   (coerce $ itemServiceName info)
                    return $ case ownerResult of
                      Right owner -> owner == coerce sender
                      Left _ -> False
              in findM matchByOwner (Map.elems infoMap)
            identifySenderById = fmap join $
              identifySenderById_ >>= logIdentifyByIdResult
            logIdentifyByIdResult
              (Left M.MethodError { M.methodErrorName = errName })
              | errName == errorUnknownMethod =
                  hostLogger DEBUG
                    (printf "Item does not support getId: %s"
                      (show errName)) >>
                  return Nothing
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
                          warningText = printf warningMsg
                                        (show senderId)
                                        (show senderNameOwner)
                                        (show infoNameOwner)
                      when (senderNameOwner /= infoNameOwner) $
                           hostLogger DEBUG warningText
                      return doMatchUnmatchedSender
                    else return False
              lift $ findM matchesSender (Map.elems infoMap)
        identifySenderBySender <||> identifySenderByNameOwner <||> identifySenderById
        where a <||> b = runMaybeT $ MaybeT a <|> MaybeT b
      identifySender _ = return Nothing

      updateItemByLensAndProp lens prop busName = runExceptT $ do
        newValue <- ExceptT (runProperty prop busName)
        let modify infoMap =
              -- This noops when the value is not present
              let newMap = set (at busName . _Just . lens) newValue infoMap
              in return (newMap, Map.lookup busName newMap)
        ExceptT $ maybeToEither (methodError (Serial 0) errorFailed) <$>
                modifyMVar itemInfoMapVar modify

      logErrorsHandler lens updateType prop =
        runUpdaters [updateItemByLensAndProp lens prop] updateType

      -- Run all the provided updaters with the expectation that at least one
      -- will succeed.
      runUpdatersForService updaters updateType serviceName = do
        updateResults <- mapM ($ serviceName) updaters
        let (failures, updates) = partitionEithers updateResults
            logLevel = propertyUpdateFailureLogLevel failures updates
        mapM_ (doUpdate updateType) updates
        when (not $ null failures) $
             hostLogger logLevel $ printf "Property update failures %s" $
                        show failures

      runUpdaters updaters updateType signal =
        identifySender signal >>= maybe runForAll (runUpdateForService . itemServiceName)
        where runUpdateForService = runUpdatersForService updaters updateType
              runForAll = logUnknownSender updateType signal >>
                          readMVar itemInfoMapVar >>=
                          mapM_ runUpdateForService . Map.keys

      updateIconPixmaps =
        updateItemByLensAndProp iconPixmapsL $ getPixmaps I.getIconPixmap

      updateIconName =
        updateItemByLensAndProp iconNameL I.getIconName

      updateIconTheme =
        updateItemByLensAndProp iconThemePathL getThemePathDefault

      updateFromIconThemeFromSignal signal =
        identifySender signal >>= traverse (updateIconTheme . itemServiceName)

      handleNewIcon signal = do
        -- XXX: This avoids the case where the theme path is updated before the
        -- icon name is updated when both signals are sent simultaneously
        updateFromIconThemeFromSignal signal
        runUpdaters [updateIconPixmaps, updateIconName]
                    IconUpdated signal

      updateOverlayIconName =
        updateItemByLensAndProp overlayIconNameL $
                                getMaybe I.getOverlayIconName

      updateOverlayIconPixmaps =
        updateItemByLensAndProp overlayIconPixmapsL $
                                getPixmaps I.getOverlayIconPixmap

      handleNewOverlayIcon signal = do
        updateFromIconThemeFromSignal signal
        runUpdaters [updateOverlayIconPixmaps, updateOverlayIconName]
                    OverlayIconUpdated signal

      getThemePathDefault client busName objectPath =
        right Just <$> I.getIconThemePath client busName objectPath

      handleNewTitle =
        logErrorsHandler iconTitleL TitleUpdated I.getTitle

      handleNewTooltip =
        logErrorsHandler itemToolTipL ToolTipUpdated $ getMaybe I.getToolTip

      handleNewStatus =
        logErrorsHandler itemStatusL StatusUpdated $ getMaybe I.getStatus

      clientRegistrationPairs =
        [ (I.registerForNewIcon, handleNewIcon)
        , (I.registerForNewIconThemePath, handleNewIcon)
        , (I.registerForNewOverlayIcon, handleNewOverlayIcon)
        , (I.registerForNewTitle, handleNewTitle)
        , (I.registerForNewToolTip, handleNewTooltip)
        , (I.registerForNewStatus, handleNewStatus)
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
                releaseName client (fromString busName)
                return ()
            logErrorAndShutdown error =
              logError (show error) >> shutdownHost >> return (Map.empty, False)
            finishInitialization serviceNames = do
              itemInfos <- createAll serviceNames
              let newMap = Map.fromList $ map (itemServiceName &&& id) itemInfos
                  resultMap = Map.union itemInfoMap newMap
              W.registerStatusNotifierHost client busName >>=
               either logErrorAndShutdown (const $ return (resultMap, True))
        W.getRegisteredStatusNotifierItems client >>=
         either logErrorAndShutdown finishInitialization

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
    return $ if initializationSuccess
    then
      Just Host
      { itemInfoMap = readMVar itemInfoMapVar
      , addUpdateHandler = addHandler
      , removeUpdateHandler = removeHandler
      , forceUpdate = handleItemAdded . coerce
      }
    else Nothing
  else do
    logErrorWithMessage "Failed to obtain desired service name" nameRequestResult
    return Nothing
