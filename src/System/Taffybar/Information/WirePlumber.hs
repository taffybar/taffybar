{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      : System.Taffybar.Information.WirePlumber
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan A. Malison
-- Stability   : unstable
-- Portability : unportable
--
-- WirePlumber/PipeWire audio information using libwireplumber through
-- GObject introspection.
--
-- This module keeps a WirePlumber core and object manager open, reads the
-- current node state from cached PipeWire params, and refreshes when
-- WirePlumber emits object or param change signals.
module System.Taffybar.Information.WirePlumber
  ( WirePlumberInfo (..),
    NodeType (..),
    getWirePlumberInfo,
    getWirePlumberInfoChan,
    getWirePlumberInfoState,
    getWirePlumberInfoChanAndVar,
    getWirePlumberInfoChanFor,
    getWirePlumberInfoStateFor,
    toggleWirePlumberMute,
    adjustWirePlumberVolume,
    setWirePlumberVolume,
  )
where

import Control.Concurrent (forkOS, runInBoundThread)
import Control.Concurrent.MVar
import Control.Concurrent.STM.TChan
import Control.Exception (SomeException, finally, try)
import Control.Monad (forM, forM_, unless, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.STM (atomically)
import Data.Aeson (decodeStrict)
import Data.Aeson.Types (parseMaybe, withObject, (.:))
import qualified Data.ByteString.Char8 as B
import Data.Char (isDigit)
import qualified Data.GI.Base.BasicTypes as GI
import qualified Data.GI.Base.GValue as GValue
import qualified Data.GI.Base.GVariant as GVariant
import qualified Data.GI.Base.ManagedPtr as ManagedPtr
import Data.IORef
import Data.Maybe (catMaybes, fromMaybe, listToMaybe, mapMaybe)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Foreign
import Foreign.C.String
import Foreign.C.Types
import GHC.TypeLits (KnownSymbol, SomeSymbol (..), Symbol, someSymbolVal, symbolVal)
import qualified GI.GLib.Structs.MainContext as MainContext
import qualified GI.GLib.Structs.MainLoop as MainLoop
import qualified GI.GObject.Objects.Object as GObject
import qualified GI.Wp.Enums as Wp
import qualified GI.Wp.Flags as Wp
import qualified GI.Wp.Functions as Wp
import qualified GI.Wp.Interfaces.PipewireObject as PipewireObject
import qualified GI.Wp.Objects.Conf as Conf
import qualified GI.Wp.Objects.Core as Core
import qualified GI.Wp.Objects.Metadata as Metadata
import qualified GI.Wp.Objects.Node as Node
import qualified GI.Wp.Objects.ObjectManager as ObjectManager
import qualified GI.Wp.Structs.Iterator as Iterator
import qualified GI.Wp.Structs.ObjectInterest as ObjectInterest
import qualified GI.Wp.Structs.SpaPod as SpaPod
import System.IO.Unsafe (unsafePerformIO)
import System.Log.Logger (Priority (..))
import System.Taffybar.Context (TaffyIO, getStateDefault)
import System.Taffybar.Util (logPrintF, runCommand)

-- | Type of audio node.
data NodeType
  = -- | Output device (speakers, headphones)
    Sink
  | -- | Input device (microphone)
    Source
  deriving (Eq, Show)

-- | WirePlumber audio information for a node.
data WirePlumberInfo = WirePlumberInfo
  { -- | Volume level from 0.0 to 1.0 (can exceed 1.0 for amplification)
    wirePlumberVolume :: Double,
    -- | Whether the node is muted
    wirePlumberMuted :: Bool,
    -- | PipeWire node name
    wirePlumberNodeName :: Text
  }
  deriving (Eq, Show)

data WirePlumberSelector
  = DefaultNode NodeType
  | NodeName Text
  | NodeId Text
  deriving (Eq, Show)

data WirePlumberObjectRefs = WirePlumberObjectRefs
  { wirePlumberObjectRef :: GObject.Object,
    wirePlumberNodeRef :: Maybe Node.Node,
    wirePlumberMetadataRef :: Maybe Metadata.Metadata
  }

newtype WirePlumberInfoChanVar (a :: Symbol)
  = WirePlumberInfoChanVar (TChan (Maybe WirePlumberInfo), MVar (Maybe WirePlumberInfo))

wpLogPath :: String
wpLogPath = "System.Taffybar.Information.WirePlumber"

wpLogF :: (MonadIO m, Show t) => Priority -> String -> t -> m ()
wpLogF = logPrintF wpLogPath

foreign import ccall "wp_spa_pod_get_property"
  c_wp_spa_pod_get_property ::
    Ptr SpaPod.SpaPod ->
    Ptr CString ->
    Ptr (Ptr SpaPod.SpaPod) ->
    IO CInt

foreign import ccall "wp_metadata_find"
  c_wp_metadata_find ::
    Ptr Metadata.Metadata ->
    Word32 ->
    CString ->
    Ptr CString ->
    IO CString

wpInitialized :: MVar Bool
wpInitialized = unsafePerformIO $ newMVar False
{-# NOINLINE wpInitialized #-}

initWirePlumber :: IO ()
initWirePlumber =
  modifyMVar_ wpInitialized $ \initialized -> do
    unless initialized $
      Wp.init [Wp.InitFlagsAll]
    pure True

-- | Get a broadcast channel for WirePlumber info for the provided node spec.
--
-- The first call for a given node spec starts a monitoring thread that keeps a
-- WirePlumber connection open and refreshes on PipeWire object/param changes.
-- Subsequent calls return the already created channel.
getWirePlumberInfoChan :: String -> TaffyIO (TChan (Maybe WirePlumberInfo))
getWirePlumberInfoChan nodeSpec =
  case someSymbolVal nodeSpec of
    SomeSymbol (Proxy :: Proxy sym) -> getWirePlumberInfoChanFor @sym

-- | Read the current WirePlumber info state for the provided node spec.
getWirePlumberInfoState :: String -> TaffyIO (Maybe WirePlumberInfo)
getWirePlumberInfoState nodeSpec =
  case someSymbolVal nodeSpec of
    SomeSymbol (Proxy :: Proxy sym) -> getWirePlumberInfoStateFor @sym

-- | Get both the broadcast channel and current state MVar for WirePlumber info.
getWirePlumberInfoChanAndVar :: String -> TaffyIO (TChan (Maybe WirePlumberInfo), MVar (Maybe WirePlumberInfo))
getWirePlumberInfoChanAndVar nodeSpec =
  case someSymbolVal nodeSpec of
    SomeSymbol (Proxy :: Proxy sym) -> do
      WirePlumberInfoChanVar pair <- getWirePlumberInfoChanVarFor @sym
      pure pair

-- | Get a broadcast channel for WirePlumber info for a node spec given as a
-- type-level string.
getWirePlumberInfoChanFor :: forall a. (KnownSymbol a) => TaffyIO (TChan (Maybe WirePlumberInfo))
getWirePlumberInfoChanFor = do
  WirePlumberInfoChanVar (chan, _) <- getWirePlumberInfoChanVarFor @a
  pure chan

-- | Read the current WirePlumber info state for a node spec given as a
-- type-level string.
getWirePlumberInfoStateFor :: forall a. (KnownSymbol a) => TaffyIO (Maybe WirePlumberInfo)
getWirePlumberInfoStateFor = do
  WirePlumberInfoChanVar (_, var) <- getWirePlumberInfoChanVarFor @a
  liftIO $ readMVar var

getWirePlumberInfoChanVarFor :: forall a. (KnownSymbol a) => TaffyIO (WirePlumberInfoChanVar a)
getWirePlumberInfoChanVarFor =
  getStateDefault $ do
    let nodeSpec = symbolVal (Proxy @a)
    liftIO $ do
      chan <- newBroadcastTChanIO
      var <- newMVar Nothing
      _ <- forkOS $ monitorWirePlumberInfo nodeSpec chan var
      pure $ WirePlumberInfoChanVar (chan, var)

monitorWirePlumberInfo ::
  String ->
  TChan (Maybe WirePlumberInfo) ->
  MVar (Maybe WirePlumberInfo) ->
  IO ()
monitorWirePlumberInfo nodeSpec chan var = do
  result <- try $ do
    context <- MainContext.mainContextNew
    MainContext.mainContextPushThreadDefault (Just context)
    ( do
        initWirePlumber
        loop <- MainLoop.mainLoopNew (Just context) False
        core <- Core.coreNew (Just context) (Nothing :: Maybe Conf.Conf) Nothing
        manager <- buildObjectManager nodeSpec
        refreshLock <- newMVar ()
        connectedSignalRefs <- newIORef []
        connectedObjectRefs <- newIORef []

        let writeInfo info = do
              oldInfo <- swapMVar var info
              when (oldInfo /= info) $
                atomically $
                  writeTChan chan info

            refresh = withMVar refreshLock $ \_ ->
              readWirePlumberInfoFromRefs connectedObjectRefs nodeSpec >>= writeInfo

            rememberSignal ref = modifyIORef' connectedSignalRefs (ref :)

            rememberObject object maybeNode maybeMetadata =
              modifyIORef'
                connectedObjectRefs
                (WirePlumberObjectRefs object maybeNode maybeMetadata :)

            connectNode object = do
              maybeNode <- ManagedPtr.castTo Node.Node object
              forM_ maybeNode $ \node -> do
                mediaClass <- PipewireObject.pipewireObjectGetProperty node "media.class"
                when (mediaClass `elem` [Just "Audio/Sink", Just "Audio/Source"]) $ do
                  handler <-
                    PipewireObject.onPipewireObjectParamsChanged node $ \param ->
                      when (param == "Props") refresh
                  rememberSignal handler
              pure maybeNode

            connectMetadata object = do
              maybeMetadata <- ManagedPtr.castTo Metadata.Metadata object
              forM_ maybeMetadata $ \metadata -> do
                handler <-
                  Metadata.onMetadataChanged metadata $ \subject maybeKey _type _value ->
                    when
                      ( subject == 0
                          && maybe False (`elem` defaultMetadataKeys) maybeKey
                      )
                      refresh
                rememberSignal handler
              pure maybeMetadata

            connectObject object = do
              maybeNode <- connectNode object
              maybeMetadata <- connectMetadata object
              rememberObject object maybeNode maybeMetadata

        _ <- ObjectManager.onObjectManagerInstalled manager $ do
          objects <- getManagedObjects manager
          mapM_ connectObject objects
          refresh

        _ <- ObjectManager.onObjectManagerObjectAdded manager $ \object -> do
          connectObject object
          refresh

        _ <- ObjectManager.onObjectManagerObjectRemoved manager $ const refresh
        _ <- ObjectManager.onObjectManagerObjectsChanged manager refresh

        Core.coreInstallObjectManager core manager
        connected <- Core.coreConnect core
        unless connected $ fail "failed to connect WirePlumber core"
        MainLoop.mainLoopRun loop
        ManagedPtr.touchManagedPtr core
        ManagedPtr.touchManagedPtr manager
        ManagedPtr.touchManagedPtr loop
      )
      `finally` MainContext.mainContextPopThreadDefault (Just context)
  case result of
    Right () -> pure ()
    Left (e :: SomeException) -> do
      wpLogF WARNING "WirePlumber monitor failed: %s" e
      _ <- swapMVar var Nothing
      atomically $ writeTChan chan Nothing

buildObjectManager :: String -> IO ObjectManager.ObjectManager
buildObjectManager nodeSpec = do
  manager <- ObjectManager.objectManagerNew
  nodeType <- GI.glibType @Node.Node
  metadataType <- GI.glibType @Metadata.Metadata

  nodeInterest <- ObjectInterest.objectInterestNewType nodeType
  forM_ (selectorMediaClass $ parseNodeSpec nodeSpec) $ \mediaClass -> do
    mediaClassVariant <- GVariant.gvariantFromText mediaClass
    ObjectInterest.objectInterestAddConstraint
      nodeInterest
      Wp.ConstraintTypePwProperty
      "media.class"
      Wp.ConstraintVerbEquals
      (Just mediaClassVariant)
  ObjectManager.objectManagerAddInterestFull manager nodeInterest
  ObjectManager.objectManagerRequestObjectFeatures
    manager
    nodeType
    (fromIntegral $ fromEnum Wp.ProxyFeaturesPipewireObjectFeaturesAll)

  metadataInterest <- ObjectInterest.objectInterestNewType metadataType
  ObjectManager.objectManagerAddInterestFull manager metadataInterest
  ObjectManager.objectManagerRequestObjectFeatures
    manager
    metadataType
    (fromIntegral $ fromEnum Wp.ProxyFeaturesPipewireObjectFeaturesMinimal)

  pure manager

-- | Query volume and mute state for the provided node spec once.
getWirePlumberInfo :: String -> IO (Maybe WirePlumberInfo)
getWirePlumberInfo nodeSpec = runInBoundThread $ do
  result <- try $ do
    context <- MainContext.mainContextNew
    MainContext.mainContextPushThreadDefault (Just context)
    ( do
        initWirePlumber
        loop <- MainLoop.mainLoopNew (Just context) False
        core <- Core.coreNew (Just context) (Nothing :: Maybe Conf.Conf) Nothing
        manager <- buildObjectManager nodeSpec
        var <- newMVar Nothing

        _ <- ObjectManager.onObjectManagerInstalled manager $ do
          objects <- getManagedObjects manager
          refs <- mapM buildWirePlumberObjectRefs objects
          info <- readWirePlumberInfoFromObjectRefs refs nodeSpec
          _ <- swapMVar var info
          MainLoop.mainLoopQuit loop

        _ <- Core.coreTimeoutAdd core 5000 $ do
          MainLoop.mainLoopQuit loop
          pure False

        Core.coreInstallObjectManager core manager
        connected <- Core.coreConnect core
        unless connected $ fail "failed to connect WirePlumber core"
        MainLoop.mainLoopRun loop
        info <- readMVar var
        ManagedPtr.touchManagedPtr core
        ManagedPtr.touchManagedPtr manager
        ManagedPtr.touchManagedPtr loop
        pure info
      )
      `finally` MainContext.mainContextPopThreadDefault (Just context)
  case result of
    Right info -> pure info
    Left (e :: SomeException) -> do
      wpLogF WARNING "WirePlumber query failed: %s" e
      pure Nothing

readWirePlumberInfoFromRefs ::
  IORef [WirePlumberObjectRefs] ->
  String ->
  IO (Maybe WirePlumberInfo)
readWirePlumberInfoFromRefs refs nodeSpec =
  readIORef refs >>= flip readWirePlumberInfoFromObjectRefs nodeSpec

readWirePlumberInfoFromObjectRefs ::
  [WirePlumberObjectRefs] ->
  String ->
  IO (Maybe WirePlumberInfo)
readWirePlumberInfoFromObjectRefs refs nodeSpec =
  ( do
      let metadata = mapMaybe wirePlumberMetadataRef refs
          nodes = mapMaybe wirePlumberNodeRef refs
      selectedNode <- selectNode metadata nodes (parseNodeSpec nodeSpec)
      maybe (pure Nothing) (fmap Just . readNodeInfo) selectedNode
  )
    `finally` mapM_ touchWirePlumberObjectRefs refs

touchWirePlumberObjectRefs :: WirePlumberObjectRefs -> IO ()
touchWirePlumberObjectRefs refs = do
  ManagedPtr.touchManagedPtr $ wirePlumberObjectRef refs
  mapM_ ManagedPtr.touchManagedPtr $ wirePlumberNodeRef refs
  mapM_ ManagedPtr.touchManagedPtr $ wirePlumberMetadataRef refs

buildWirePlumberObjectRefs :: GObject.Object -> IO WirePlumberObjectRefs
buildWirePlumberObjectRefs object =
  WirePlumberObjectRefs object
    <$> objectToNode object
    <*> objectToMetadata object

getManagedObjects :: ObjectManager.ObjectManager -> IO [GObject.Object]
getManagedObjects manager = do
  iterator <- ObjectManager.objectManagerNewIterator manager
  catMaybes <$> iteratorValues @(Maybe GObject.Object) iterator

objectToNode :: GObject.Object -> IO (Maybe Node.Node)
objectToNode = ManagedPtr.castTo Node.Node

objectToMetadata :: GObject.Object -> IO (Maybe Metadata.Metadata)
objectToMetadata = ManagedPtr.castTo Metadata.Metadata

selectNode ::
  [Metadata.Metadata] ->
  [Node.Node] ->
  WirePlumberSelector ->
  IO (Maybe Node.Node)
selectNode metadata nodes selector =
  case selector of
    DefaultNode nodeType -> do
      maybeDefaultName <- readDefaultNodeName metadata nodeType
      case maybeDefaultName of
        Just defaultName -> findM (nodeNameMatches defaultName) =<< filterNodesByType nodeType nodes
        Nothing -> listToMaybe <$> filterNodesByType nodeType nodes
    NodeName name -> findM (nodeNameMatches name) nodes
    NodeId nodeId -> findM (nodeIdMatches nodeId) nodes

filterNodesByType :: NodeType -> [Node.Node] -> IO [Node.Node]
filterNodesByType nodeType =
  filterMIO $ \node ->
    (== Just (nodeTypeMediaClass nodeType))
      <$> PipewireObject.pipewireObjectGetProperty node "media.class"

nodeNameMatches :: Text -> Node.Node -> IO Bool
nodeNameMatches expected node =
  (== Just expected) <$> PipewireObject.pipewireObjectGetProperty node "node.name"

nodeIdMatches :: Text -> Node.Node -> IO Bool
nodeIdMatches expected node =
  (== Just expected) <$> PipewireObject.pipewireObjectGetProperty node "object.id"

readDefaultNodeName :: [Metadata.Metadata] -> NodeType -> IO (Maybe Text)
readDefaultNodeName metadata nodeType = do
  names <-
    forM metadata $ \m -> do
      result <- metadataFindNoFree m 0 (defaultMetadataKey nodeType)
      pure $ result >>= parseDefaultNodeName . fst
  pure $ listToMaybe $ catMaybes names

readNodeInfo :: Node.Node -> IO WirePlumberInfo
readNodeInfo node = do
  nodeName <- fromMaybe "" <$> PipewireObject.pipewireObjectGetProperty node "node.name"
  props <- readNodeProps node
  maybeRawVolume <-
    maybe (pure Nothing) podArrayChildNumber $
      lookup "channelVolumes" props
  maybeMuted <-
    maybe (pure Nothing) podBool $
      lookup "mute" props
  let volume = pipeWireRawVolumeToLinear $ fromMaybe 0 maybeRawVolume
      muted = fromMaybe False maybeMuted
  pure $
    WirePlumberInfo
      { wirePlumberVolume = volume,
        wirePlumberMuted = muted,
        wirePlumberNodeName = nodeName
      }

-- | Read the "Props" params of a node as key/value pairs.
--
-- The returned pods are deep copies (see 'spaPodGetPropertyNoFree'), so they
-- stay valid after the params iterators and their pods are garbage collected.
readNodeProps :: Node.Node -> IO [(Text, SpaPod.SpaPod)]
readNodeProps node = do
  maybeParams <- PipewireObject.pipewireObjectEnumParamsSync node "Props" Nothing
  case maybeParams of
    Nothing -> pure []
    Just params -> do
      pods <- catMaybes <$> iteratorValues @(Maybe SpaPod.SpaPod) params
      keyValues <-
        fmap concat $
          forM pods $ \pod -> do
            iterator <- SpaPod.spaPodNewIterator pod
            props <- catMaybes <$> iteratorValues @(Maybe SpaPod.SpaPod) iterator
            propKeyValues <-
              fmap catMaybes $
                forM props $ \propPod -> do
                  (ok, key, value) <- spaPodGetPropertyNoFree propPod
                  pure $ if ok then Just (key, value) else Nothing
            -- The property pods yielded by the iterator wrap data owned by
            -- the parent pod, so keep the parent (and the iterator, which
            -- holds a reference to it) alive until all of them were read.
            mapM_ ManagedPtr.touchManagedPtr props
            ManagedPtr.touchManagedPtr iterator
            ManagedPtr.touchManagedPtr pod
            pure propKeyValues
      mapM_ ManagedPtr.touchManagedPtr pods
      ManagedPtr.touchManagedPtr params
      pure keyValues

spaPodGetPropertyNoFree :: SpaPod.SpaPod -> IO (Bool, Text, SpaPod.SpaPod)
spaPodGetPropertyNoFree pod =
  ManagedPtr.withManagedPtr pod $ \podPtr ->
    alloca $ \keyPtr ->
      alloca $ \valuePtr -> do
        result <- c_wp_spa_pod_get_property podPtr keyPtr valuePtr
        keyPtrValue <- peek keyPtr
        valuePtrValue <- peek valuePtr
        key <-
          if keyPtrValue == nullPtr
            then pure ""
            else T.pack <$> peekCString keyPtrValue
        wrapped <- ManagedPtr.wrapBoxed SpaPod.SpaPod valuePtrValue
        -- wp_spa_pod_get_property returns a wrap pod whose data borrows from
        -- the property pod, so deep copy it before it can outlive its parent.
        value <- SpaPod.spaPodCopy wrapped
        pure (result /= 0, key, value)

metadataFindNoFree :: Metadata.Metadata -> Word32 -> Text -> IO (Maybe (Text, Text))
metadataFindNoFree metadata subject key =
  ManagedPtr.withManagedPtr metadata $ \metadataPtr ->
    B.useAsCString (T.encodeUtf8 key) $ \keyPtr ->
      alloca $ \typePtr -> do
        valuePtr <- c_wp_metadata_find metadataPtr subject keyPtr typePtr
        if valuePtr == nullPtr
          then pure Nothing
          else do
            typePtrValue <- peek typePtr
            value <- T.pack <$> peekCString valuePtr
            type_ <-
              if typePtrValue == nullPtr
                then pure ""
                else T.pack <$> peekCString typePtrValue
            pure $ Just (value, type_)

podArrayChildNumber :: SpaPod.SpaPod -> IO (Maybe Double)
podArrayChildNumber pod = do
  isArray <- SpaPod.spaPodIsArray pod
  result <-
    if isArray
      then SpaPod.spaPodGetArrayChild pod >>= podNumber
      else podNumber pod
  -- wp_spa_pod_get_array_child returns a wrap pod whose data borrows from
  -- the array pod, so keep the array pod alive while the child is read.
  ManagedPtr.touchManagedPtr pod
  pure result

podNumber :: SpaPod.SpaPod -> IO (Maybe Double)
podNumber pod = do
  isFloat <- SpaPod.spaPodIsFloat pod
  isDouble <- SpaPod.spaPodIsDouble pod
  if isFloat
    then do
      (ok, value) <- SpaPod.spaPodGetFloat pod
      pure $ if ok then Just (realToFrac value) else Nothing
    else
      if isDouble
        then do
          (ok, value) <- SpaPod.spaPodGetDouble pod
          pure $ if ok then Just value else Nothing
        else pure Nothing

podBool :: SpaPod.SpaPod -> IO (Maybe Bool)
podBool pod = do
  isBool <- SpaPod.spaPodIsBoolean pod
  if isBool
    then do
      (ok, value) <- SpaPod.spaPodGetBoolean pod
      pure $ if ok then Just value else Nothing
    else pure Nothing

parseDefaultNodeName :: Text -> Maybe Text
parseDefaultNodeName value =
  decodeStrict (T.encodeUtf8 value) >>= parseMaybe parseName
  where
    parseName = withObject "WirePlumber default audio target" (.: "name")

pipeWireRawVolumeToLinear :: Double -> Double
pipeWireRawVolumeToLinear rawVolume = rawVolume ** (1 / 3 :: Double)

parseNodeSpec :: String -> WirePlumberSelector
parseNodeSpec "" = DefaultNode Sink
parseNodeSpec "@DEFAULT_AUDIO_SINK@" = DefaultNode Sink
parseNodeSpec "@DEFAULT_SINK@" = DefaultNode Sink
parseNodeSpec "@DEFAULT_AUDIO_SOURCE@" = DefaultNode Source
parseNodeSpec "@DEFAULT_SOURCE@" = DefaultNode Source
parseNodeSpec spec
  | all isDigit spec = NodeId $ T.pack spec
  | otherwise = NodeName $ T.pack spec

selectorMediaClass :: WirePlumberSelector -> Maybe Text
selectorMediaClass (DefaultNode nodeType) = Just $ nodeTypeMediaClass nodeType
selectorMediaClass _ = Nothing

nodeTypeMediaClass :: NodeType -> Text
nodeTypeMediaClass Sink = "Audio/Sink"
nodeTypeMediaClass Source = "Audio/Source"

defaultMetadataKey :: NodeType -> Text
defaultMetadataKey Sink = "default.audio.sink"
defaultMetadataKey Source = "default.audio.source"

defaultMetadataKeys :: [Text]
defaultMetadataKeys =
  [ "default.audio.sink",
    "default.audio.source",
    "default.configured.audio.sink",
    "default.configured.audio.source"
  ]

findM :: (a -> IO Bool) -> [a] -> IO (Maybe a)
findM _ [] = pure Nothing
findM predicate (x : xs) = do
  matches <- predicate x
  if matches then pure $ Just x else findM predicate xs

filterMIO :: (a -> IO Bool) -> [a] -> IO [a]
filterMIO predicate = fmap (map fst . filter snd) . mapM (\x -> (x,) <$> predicate x)

iteratorValues :: (GValue.IsGValue a) => Iterator.Iterator -> IO [a]
iteratorValues iterator = do
  (hasNext, value) <- Iterator.iteratorNext iterator
  if hasNext
    then do
      item <- GValue.fromGValue value
      rest <- iteratorValues iterator
      pure (item : rest)
    else pure []

-- | Toggle mute for the provided node spec. Returns True on success.
toggleWirePlumberMute :: String -> IO Bool
toggleWirePlumberMute nodeSpec = do
  let node = if null nodeSpec then "@DEFAULT_AUDIO_SINK@" else nodeSpec
  result <- runCommand "wpctl" ["set-mute", node, "toggle"]
  case result of
    Left err -> do
      wpLogF WARNING "wpctl set-mute toggle failed: %s" err
      return False
    Right _ -> return True

-- | Adjust volume by the provided percentage delta. Returns True on success.
--
-- Positive values increase volume, negative values decrease it.
-- The delta is clamped to prevent going below 0%.
adjustWirePlumberVolume :: String -> Int -> IO Bool
adjustWirePlumberVolume nodeSpec deltaPercent = do
  let node = if null nodeSpec then "@DEFAULT_AUDIO_SINK@" else nodeSpec
      sign = if deltaPercent >= 0 then "+" else "-"
      absVal = abs deltaPercent
      arg = show absVal ++ "%" ++ sign
  result <- runCommand "wpctl" ["set-volume", node, arg, "--limit", "1.5"]
  case result of
    Left err -> do
      wpLogF WARNING "wpctl set-volume failed: %s" err
      return False
    Right _ -> return True

-- | Set volume to an absolute percentage value. Returns True on success.
setWirePlumberVolume :: String -> Int -> IO Bool
setWirePlumberVolume nodeSpec percent = do
  let node = if null nodeSpec then "@DEFAULT_AUDIO_SINK@" else nodeSpec
      arg = show (max 0 percent) ++ "%"
  result <- runCommand "wpctl" ["set-volume", node, arg, "--limit", "1.5"]
  case result of
    Left err -> do
      wpLogF WARNING "wpctl set-volume failed: %s" err
      return False
    Right _ -> return True
