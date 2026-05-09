{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Module      : System.Taffybar.Widget.SNITray.PrioritizedCollapsible
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan A. Malison
-- Stability   : unstable
-- Portability : unportable
--
-- Prioritized, collapsible StatusNotifierItem tray with editable per-icon
-- priorities and persisted priority state.
module System.Taffybar.Widget.SNITray.PrioritizedCollapsible
  ( module System.Taffybar.Widget.SNITray.PrioritizedCollapsible,
  )
where

import Control.Applicative ((<|>))
import Control.Monad (forM_, guard, unless, void, when)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import qualified DBus as D
import qualified DBus.Client as DBus
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as AKey
import qualified Data.Aeson.KeyMap as AKeyMap
import Data.Aeson.Types (Parser, parseMaybe)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Char (isAlphaNum, isDigit, toLower)
import Data.Foldable (traverse_)
import Data.IORef
import Data.Int (Int32, Int64)
import Data.List (intercalate, isPrefixOf, sortOn, stripPrefix)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, listToMaybe, mapMaybe, maybeToList)
import Data.Ord (Down (..))
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Unique (hashUnique)
import Data.Word (Word32)
import qualified Data.Yaml as Y
import qualified GI.GLib as GLib
import qualified GI.Gdk as Gdk
import qualified GI.Gtk as Gtk
import Graphics.UI.GIGtkStrut (StrutAlignment (Beginning))
import qualified StatusNotifier.Host.Service as H
import StatusNotifier.Tray
import qualified StatusNotifier.Tray as Tray
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Environment.XDG.BaseDir (getUserConfigFile)
import System.FilePath (isRelative, replaceExtension, takeBaseName, takeDirectory, takeExtension)
import System.Log.Logger (Priority (DEBUG, INFO), logM)
import System.Taffybar.Context
import System.Taffybar.Widget.SNITray
  ( CollapsibleSNITrayParams (..),
    SNITrayConfig (..),
    defaultCollapsibleSNITrayParams,
    getTrayHost,
  )
import System.Taffybar.Widget.Util
import Text.Printf
import Text.Read (readMaybe)

prioritizedTrayLog :: Priority -> String -> IO ()
prioritizedTrayLog = logM "System.Taffybar.Widget.SNITray.PrioritizedCollapsible"

type SNIPriorityMap = M.Map String Int

data SNIPriorityEntry = SNIPriorityEntry
  { sniPriorityEntryKey :: String,
    sniPriorityEntryPriority :: Int
  }

data SNIPriorityFile = SNIPriorityFile
  { sniPriorityFilePriorities :: SNIPriorityMap,
    sniPriorityFileMaxVisibleIcons :: Maybe Int,
    -- Nothing: no persisted override (use config default)
    -- Just Nothing: explicitly no threshold
    -- Just (Just n): explicit threshold n
    sniPriorityFileVisibilityThreshold :: Maybe (Maybe Int)
  }

instance A.FromJSON SNIPriorityEntry where
  parseJSON = A.withObject "SNIPriorityEntry" $ \obj ->
    SNIPriorityEntry
      <$> obj A..: "key"
      <*> obj A..: "priority"

instance A.ToJSON SNIPriorityEntry where
  toJSON SNIPriorityEntry {..} =
    A.object
      [ "key" A..= sniPriorityEntryKey,
        "priority" A..= sniPriorityEntryPriority
      ]

parsePrioritiesValue :: A.Value -> Parser SNIPriorityMap
parsePrioritiesValue value =
  parseEntries value <|> A.parseJSON value
  where
    parseEntries v = do
      entries <- A.parseJSON v :: Parser [SNIPriorityEntry]
      let toPair SNIPriorityEntry {..} =
            (sniPriorityEntryKey, sniPriorityEntryPriority)
      return $ M.fromList (map toPair entries)

instance A.FromJSON SNIPriorityFile where
  parseJSON = A.withObject "SNIPriorityFile" $ \obj -> do
    maybePriorities <- obj A..:? "priorities"
    priorities <- maybe (return M.empty) parsePrioritiesValue maybePriorities
    maxVisibleIcons <- obj A..:? "max_visible_icons"
    let thresholdKey = AKey.fromString "visibility_threshold"
    visibilityThreshold <-
      if AKeyMap.member thresholdKey obj
        then Just <$> obj A..:? "visibility_threshold"
        else return Nothing
    return $
      SNIPriorityFile
        { sniPriorityFilePriorities = priorities,
          sniPriorityFileMaxVisibleIcons = maxVisibleIcons,
          sniPriorityFileVisibilityThreshold = visibilityThreshold
        }

instance A.ToJSON SNIPriorityFile where
  toJSON SNIPriorityFile {..} =
    let sortedEntries =
          map
            (uncurry SNIPriorityEntry)
            (sortOn (\(key, priority) -> (Down priority, key)) (M.toList sniPriorityFilePriorities))
     in A.object
          ( [ "format_version" A..= (2 :: Int),
              "priorities" A..= sortedEntries
            ]
              <> maybe [] (\value -> ["max_visible_icons" A..= value]) sniPriorityFileMaxVisibleIcons
              <> maybe [] (\value -> ["visibility_threshold" A..= value]) sniPriorityFileVisibilityThreshold
          )

-- | Configuration for a collapsible tray with editable icon priorities.
data PrioritizedCollapsibleSNITrayParams = PrioritizedCollapsibleSNITrayParams
  { -- | Base collapsible tray parameters.
    prioritizedCollapsibleSNITrayParams :: CollapsibleSNITrayParams,
    -- | Path for priority state persistence.
    --
    -- Relative paths are resolved under @~/.config/taffybar/@.
    prioritizedCollapsibleSNITrayPriorityStateFile :: FilePath,
    -- | Minimum priority value.
    prioritizedCollapsibleSNITrayPriorityMin :: Int,
    -- | Maximum priority value.
    prioritizedCollapsibleSNITrayPriorityMax :: Int,
    -- | Default priority assigned to unmapped icons.
    prioritizedCollapsibleSNITrayDefaultPriority :: Int,
    -- | Hide icons with priority below this value.
    --
    -- This only applies while collapsed; expanded mode always shows all icons.
    prioritizedCollapsibleSNITrayVisibilityThreshold :: Maybe Int,
    -- | Whether priority edit mode starts enabled.
    prioritizedCollapsibleSNITrayStartPriorityEditMode :: Bool,
    -- | Always show the expand/collapse toggle button.
    prioritizedCollapsibleSNITrayAlwaysShowExpandToggle :: Bool,
    -- | Temporarily expand collapsed tray icons while the pointer is over the tray.
    prioritizedCollapsibleSNITrayHoverExpand :: Bool,
    -- | Delay before hover expansion starts.
    prioritizedCollapsibleSNITrayHoverExpandDelayMs :: Word32,
    -- | Delay before hover expansion collapses again.
    prioritizedCollapsibleSNITrayHoverCollapseDelayMs :: Word32,
    -- | Duration for the clipped tray extent animation while hover expanding/collapsing.
    prioritizedCollapsibleSNITrayHoverAnimationDurationMs :: Word32,
    -- | Label renderer for the priority-edit-mode toggle.
    --
    -- Argument: @editing@.
    prioritizedCollapsibleSNITrayPriorityModeLabel :: Bool -> T.Text
  }

defaultPrioritizedCollapsibleSNITrayPriorityModeLabel :: Bool -> T.Text
defaultPrioritizedCollapsibleSNITrayPriorityModeLabel editing
  | editing = "P*"
  | otherwise = "P"

-- | Default params for 'PrioritizedCollapsibleSNITrayParams'.
defaultPrioritizedCollapsibleSNITrayParams :: PrioritizedCollapsibleSNITrayParams
defaultPrioritizedCollapsibleSNITrayParams =
  PrioritizedCollapsibleSNITrayParams
    { prioritizedCollapsibleSNITrayParams = defaultCollapsibleSNITrayParams,
      prioritizedCollapsibleSNITrayPriorityStateFile = "sni-priorities.yaml",
      prioritizedCollapsibleSNITrayPriorityMin = -5,
      prioritizedCollapsibleSNITrayPriorityMax = 5,
      prioritizedCollapsibleSNITrayDefaultPriority = 0,
      prioritizedCollapsibleSNITrayVisibilityThreshold = Nothing,
      prioritizedCollapsibleSNITrayStartPriorityEditMode = False,
      prioritizedCollapsibleSNITrayAlwaysShowExpandToggle = True,
      prioritizedCollapsibleSNITrayHoverExpand = False,
      prioritizedCollapsibleSNITrayHoverExpandDelayMs = 120,
      prioritizedCollapsibleSNITrayHoverCollapseDelayMs = 500,
      prioritizedCollapsibleSNITrayHoverAnimationDurationMs = 180,
      prioritizedCollapsibleSNITrayPriorityModeLabel = defaultPrioritizedCollapsibleSNITrayPriorityModeLabel
    }

clampPriorityInRange :: Int -> Int -> Int -> Int
clampPriorityInRange priorityMin priorityMax =
  max priorityMin . min priorityMax

resolveSNIPriorityStateFile :: FilePath -> IO FilePath
resolveSNIPriorityStateFile path
  | isRelative path = getUserConfigFile "taffybar" path
  | otherwise = return path

parseLegacySNIPriorityMap :: BS.ByteString -> Maybe SNIPriorityMap
parseLegacySNIPriorityMap content =
  M.fromList <$> (readMaybe (BS8.unpack content) :: Maybe [(String, Int)])

parseLegacySNIPriorityFile :: BS.ByteString -> Maybe SNIPriorityFile
parseLegacySNIPriorityFile content =
  legacyFile <$> parseLegacySNIPriorityMap content
  where
    legacyFile priorities =
      SNIPriorityFile
        { sniPriorityFilePriorities = priorities,
          sniPriorityFileMaxVisibleIcons = Nothing,
          sniPriorityFileVisibilityThreshold = Nothing
        }

parseSNIPriorityFile :: BS.ByteString -> Maybe SNIPriorityFile
parseSNIPriorityFile content =
  parseYamlState <|> parseYamlPriorityOnly <|> parseLegacySNIPriorityFile content
  where
    parseYamlState =
      either
        (const Nothing)
        Just
        (Y.decodeEither' content :: Either Y.ParseException SNIPriorityFile)
    parseYamlPriorityOnly = do
      yamlValue <- either (const Nothing) Just (Y.decodeEither' content :: Either Y.ParseException A.Value)
      priorities <- parseMaybe parsePrioritiesValue yamlValue
      return $
        SNIPriorityFile
          { sniPriorityFilePriorities = priorities,
            sniPriorityFileMaxVisibleIcons = Nothing,
            sniPriorityFileVisibilityThreshold = Nothing
          }

loadSNIPriorityFileFromPath :: FilePath -> IO (Maybe SNIPriorityFile)
loadSNIPriorityFileFromPath path = do
  exists <- doesFileExist path
  if not exists
    then return Nothing
    else parseSNIPriorityFile <$> BS.readFile path

legacyPriorityStateFallbackPath :: FilePath -> Maybe FilePath
legacyPriorityStateFallbackPath path
  | takeExtension path == ".yaml" = Just (replaceExtension path "dat")
  | otherwise = Nothing

loadSNIPriorityFileFromFile :: FilePath -> IO SNIPriorityFile
loadSNIPriorityFileFromFile path = do
  loaded <- loadSNIPriorityFileFromPath path
  case loaded of
    Just priorityFile -> return priorityFile
    Nothing ->
      case legacyPriorityStateFallbackPath path of
        Nothing -> return emptyPriorityFile
        Just legacyPath -> do
          fallbackLoaded <- loadSNIPriorityFileFromPath legacyPath
          case fallbackLoaded of
            Nothing -> return emptyPriorityFile
            Just priorityFile -> do
              -- One-time migration path for existing show/read .dat files.
              persistSNIPriorityFileToFile path priorityFile
              return priorityFile

emptyPriorityFile :: SNIPriorityFile
emptyPriorityFile =
  SNIPriorityFile
    { sniPriorityFilePriorities = M.empty,
      sniPriorityFileMaxVisibleIcons = Nothing,
      sniPriorityFileVisibilityThreshold = Nothing
    }

persistSNIPriorityFileToFile :: FilePath -> SNIPriorityFile -> IO ()
persistSNIPriorityFileToFile path priorityFile = do
  createDirectoryIfMissing True (takeDirectory path)
  BS.writeFile path (Y.encode priorityFile)

nonEmptyString :: String -> Maybe String
nonEmptyString value
  | null value = Nothing
  | otherwise = Just value

itemStableIdentity :: H.ItemInfo -> String
itemStableIdentity info =
  show (H.itemServiceName info) <> "|" <> show (H.itemServicePath info)

itemStableIdentityKey :: H.ItemInfo -> String
itemStableIdentityKey info = "item-identity:" ++ itemStableIdentity info

itemDBusAddress :: H.ItemInfo -> String
itemDBusAddress info =
  D.formatBusName (H.itemServiceName info) <> "|" <> D.formatObjectPath (H.itemServicePath info)

orderingComponent :: Maybe String -> (Bool, String)
orderingComponent value =
  case value >>= nonEmptyString of
    Nothing -> (True, "")
    Just text -> (False, map toLower text)

hasNumericSuffix :: String -> String -> Bool
hasNumericSuffix prefix value =
  case stripPrefix prefix value of
    Just suffix -> not (null suffix) && all isDigit suffix
    Nothing -> False

unstableItemIdPrefixes :: [String]
unstableItemIdPrefixes =
  ["chrome_status_icon_", "systray_", "statusnotifieritem-", "statusnotifieritem_"]

sharedItemIdPrefixes :: [String]
sharedItemIdPrefixes = ["chrome_status_icon_", "systray_"]

unstableServiceNamePrefixes :: [String]
unstableServiceNamePrefixes = ["org.kde.StatusNotifierItem-"]

matchingItemIdPrefix :: [String] -> String -> Maybe String
matchingItemIdPrefix prefixes itemId =
  let lowerItemId = map toLower itemId
      matchingPrefixes =
        filter (`hasNumericSuffix` lowerItemId) prefixes
   in case matchingPrefixes of
        prefix : _ -> Just (take (length prefix) itemId)
        [] -> Nothing

unstableItemIdPrefix :: String -> Maybe String
unstableItemIdPrefix = matchingItemIdPrefix unstableItemIdPrefixes

sharedItemIdPrefix :: String -> Maybe String
sharedItemIdPrefix = matchingItemIdPrefix sharedItemIdPrefixes

isLikelyUnstableItemId :: String -> Bool
isLikelyUnstableItemId = isJust . unstableItemIdPrefix

stableItemIdKey :: H.ItemInfo -> Maybe String
stableItemIdKey info = do
  itemId <- H.itemId info >>= nonEmptyString
  guard (not (isLikelyUnstableItemId itemId))
  return ("item-id:" ++ itemId)

unstableItemIdKey :: H.ItemInfo -> Maybe String
unstableItemIdKey info = do
  itemId <- H.itemId info >>= nonEmptyString
  guard (isLikelyUnstableItemId itemId)
  return ("item-id:" ++ itemId)

itemIdOrderingToken :: H.ItemInfo -> Maybe String
itemIdOrderingToken info = do
  itemId <- H.itemId info >>= nonEmptyString
  return $ fromMaybe itemId (unstableItemIdPrefix itemId)

itemServiceNameOrderingToken :: H.ItemInfo -> Maybe String
itemServiceNameOrderingToken info =
  let serviceName = D.formatBusName (H.itemServiceName info)
   in if ":" `isPrefixOf` serviceName || any (`isPrefixOf` serviceName) unstableServiceNamePrefixes
        then Nothing
        else Just serviceName

itemOrderingKey :: Maybe String -> H.ItemInfo -> [(Bool, String)]
itemOrderingKey maybeProcessKey info =
  map
    orderingComponent
    [ maybeProcessKey,
      itemIdOrderingToken info,
      nonEmptyString (H.iconName info),
      nonEmptyString (H.iconTitle info),
      H.itemCategory info >>= nonEmptyString,
      itemServiceNameOrderingToken info,
      show <$> H.menuPath info,
      Just (show (H.itemServicePath info)),
      Just (itemDBusAddress info)
    ]

sharedItemIdPrefixIconNameKey :: H.ItemInfo -> Maybe String
sharedItemIdPrefixIconNameKey info = do
  itemId <- H.itemId info >>= nonEmptyString
  prefix <- sharedItemIdPrefix itemId
  iconName <- nonEmptyString (H.iconName info)
  return ("item-id-prefix+icon-name:" ++ prefix ++ "|" ++ iconName)

sharedItemIdPrefixIconTitleKey :: H.ItemInfo -> Maybe String
sharedItemIdPrefixIconTitleKey info = do
  itemId <- H.itemId info >>= nonEmptyString
  prefix <- sharedItemIdPrefix itemId
  iconTitle <- nonEmptyString (H.iconTitle info)
  return ("item-id-prefix+icon-title:" ++ prefix ++ "|" ++ iconTitle)

isLikelySharedItemIdForInfo :: H.ItemInfo -> Bool
isLikelySharedItemIdForInfo info =
  case H.itemId info >>= nonEmptyString of
    Nothing -> False
    Just itemId -> isJust (sharedItemIdPrefix itemId)

normalizeProcessToken :: String -> String
normalizeProcessToken =
  dropWhile (== '-') . reverse . dropWhile (== '-') . reverse . map normalizeChar
  where
    normalizeChar c
      | isAlphaNum c = toLower c
      | otherwise = '-'

processIdentityTokenFromArg :: String -> Maybe String
processIdentityTokenFromArg arg
  | takeExtension arg == ".asar" =
      nonEmptyString (takeBaseName (takeDirectory arg))
  | otherwise = Nothing

processIdentityTokenFromCmdline :: [String] -> Maybe String
processIdentityTokenFromCmdline [] = Nothing
processIdentityTokenFromCmdline (exeArg : args) =
  let argToken = listToMaybe (mapMaybe processIdentityTokenFromArg args)
      exeToken = nonEmptyString (takeBaseName exeArg)
      normalized = normalizeProcessToken <$> (argToken <|> exeToken)
   in normalized >>= nonEmptyString

readProcessCommandLine :: Word32 -> IO [String]
readProcessCommandLine pid = do
  let cmdlinePath = "/proc/" ++ show pid ++ "/cmdline"
  exists <- doesFileExist cmdlinePath
  if not exists
    then return []
    else do
      bytes <- BS.readFile cmdlinePath
      return $ filter (not . null) (map BS8.unpack (BS8.split '\0' bytes))

dbusDaemonName :: D.BusName
dbusDaemonName = D.busName_ "org.freedesktop.DBus"

dbusDaemonPath :: D.ObjectPath
dbusDaemonPath = D.objectPath_ "/org/freedesktop/DBus"

getConnectionUnixProcessID :: DBus.Client -> D.BusName -> IO (Maybe Word32)
getConnectionUnixProcessID client busName = do
  let method =
        (D.methodCall dbusDaemonPath "org.freedesktop.DBus" "GetConnectionUnixProcessID")
          { D.methodCallDestination = Just dbusDaemonName,
            D.methodCallBody = [D.toVariant (D.formatBusName busName)]
          }
  result <- DBus.call client method
  case result of
    Left _ -> return Nothing
    Right reply ->
      case D.methodReturnBody reply of
        [pidVariant] -> return (D.fromVariant pidVariant)
        _ -> return Nothing

processDisambiguationKeyForItem :: DBus.Client -> H.ItemInfo -> IO (Maybe String)
processDisambiguationKeyForItem client info = do
  maybePid <- getConnectionUnixProcessID client (H.itemServiceName info)
  case maybePid of
    Nothing -> return Nothing
    Just pid -> do
      cmdline <- readProcessCommandLine pid
      return $ ("process:" ++) <$> processIdentityTokenFromCmdline cmdline

processDisambiguationKeysForItems :: DBus.Client -> [H.ItemInfo] -> IO (M.Map String String)
processDisambiguationKeysForItems client infos = do
  pairs <- mapM withKey infos
  return $ M.fromList (catMaybes pairs)
  where
    withKey info = do
      processKey <- processDisambiguationKeyForItem client info
      return $ fmap (itemStableIdentity info,) processKey

dedupe :: (Ord a) => [a] -> [a]
dedupe = go Set.empty
  where
    go _ [] = []
    go seen (x : xs)
      | x `Set.member` seen = go seen xs
      | otherwise = x : go (Set.insert x seen) xs

priorityLookupKeyCandidates :: Maybe String -> H.ItemInfo -> [String]
priorityLookupKeyCandidates maybeProcessKey info =
  dedupe $
    concat
      [ maybeToList maybeProcessKey,
        map ("icon-name:" ++) (maybeToList (nonEmptyString (H.iconName info))),
        maybeToList (stableItemIdKey info),
        map ("icon-title:" ++) (maybeToList (nonEmptyString (H.iconTitle info))),
        maybeToList (sharedItemIdPrefixIconNameKey info),
        maybeToList (sharedItemIdPrefixIconTitleKey info),
        [itemStableIdentityKey info],
        maybeToList (unstableItemIdKey info)
      ]

priorityEditableKeyCandidates :: Maybe String -> H.ItemInfo -> [String]
priorityEditableKeyCandidates maybeProcessKey info =
  dedupe $
    concat
      [ maybeToList maybeProcessKey,
        map ("icon-name:" ++) (maybeToList (nonEmptyString (H.iconName info))),
        maybeToList (stableItemIdKey info),
        map ("icon-title:" ++) (maybeToList (nonEmptyString (H.iconTitle info))),
        maybeToList (sharedItemIdPrefixIconNameKey info),
        maybeToList (sharedItemIdPrefixIconTitleKey info),
        [itemStableIdentityKey info],
        maybeToList (unstableItemIdKey info)
      ]

priorityKeyFromItem :: Maybe String -> H.ItemInfo -> Maybe String
priorityKeyFromItem maybeProcessKey =
  listToMaybe . priorityEditableKeyCandidates maybeProcessKey

itemPriorityFromMap ::
  Int ->
  Int ->
  Int ->
  SNIPriorityMap ->
  Maybe String ->
  H.ItemInfo ->
  Int
itemPriorityFromMap priorityMin priorityMax defaultPriority priorities maybeProcessKey info =
  let clampPriority = clampPriorityInRange priorityMin priorityMax
      matchedPriority =
        listToMaybe $
          mapMaybe (`M.lookup` priorities) (priorityLookupKeyCandidates maybeProcessKey info)
   in clampPriority (fromMaybe defaultPriority matchedPriority)

formatMaybe :: Maybe String -> String
formatMaybe = fromMaybe "-"

describeItemForOrder ::
  (H.ItemInfo -> Int) ->
  (H.ItemInfo -> Maybe String) ->
  H.ItemInfo ->
  String
describeItemForOrder priorityForInfo processKeyForInfo info =
  printf
    "p=%d process=%s itemId=%s icon=%s title=%s category=%s service=%s path=%s"
    (priorityForInfo info)
    (formatMaybe $ processKeyForInfo info)
    (formatMaybe $ H.itemId info)
    (H.iconName info)
    (H.iconTitle info)
    (formatMaybe $ H.itemCategory info)
    (D.formatBusName $ H.itemServiceName info)
    (D.formatObjectPath $ H.itemServicePath info)

describeOrderedInfos ::
  (H.ItemInfo -> Int) ->
  (H.ItemInfo -> Maybe String) ->
  [H.ItemInfo] ->
  String
describeOrderedInfos priorityForInfo processKeyForInfo infos =
  intercalate
    " | "
    [ printf
        "%d:{%s}"
        index
        (describeItemForOrder priorityForInfo processKeyForInfo info)
    | (index, info) <- zip [0 :: Int ..] infos
    ]

sortedInfosByPriority ::
  Bool ->
  Int ->
  Int ->
  Int ->
  SNIPriorityMap ->
  (H.ItemInfo -> Maybe String) ->
  [H.ItemInfo] ->
  [H.ItemInfo]
sortedInfosByPriority highPriorityFirstInTrayOrder priorityMin priorityMax defaultPriority priorities processKeyForInfo infos =
  let itemPriority info =
        itemPriorityFromMap
          priorityMin
          priorityMax
          defaultPriority
          priorities
          (processKeyForInfo info)
          info
      prioritySortKey info =
        if highPriorityFirstInTrayOrder
          then negate (itemPriority info)
          else itemPriority info
   in sortOn
        (\info -> (prioritySortKey info, itemOrderingKey (processKeyForInfo info) info))
        infos

lookupExplicitPriority ::
  SNIPriorityMap ->
  Maybe String ->
  H.ItemInfo ->
  Maybe Int
lookupExplicitPriority priorities maybeProcessKey info =
  listToMaybe $ mapMaybe (`M.lookup` priorities) (priorityLookupKeyCandidates maybeProcessKey info)

setExplicitPriorityForItem ::
  IORef SNIPriorityMap ->
  (SNIPriorityMap -> IO ()) ->
  Maybe String ->
  H.ItemInfo ->
  Maybe Int ->
  IO ()
setExplicitPriorityForItem prioritiesRef afterUpdate maybeProcessKey info newPriority =
  case priorityKeyFromItem maybeProcessKey info of
    Nothing -> return ()
    Just primaryKey -> do
      let editableKeys = priorityEditableKeyCandidates maybeProcessKey info
      modifyIORef' prioritiesRef $ \priorities ->
        case newPriority of
          Nothing -> foldr M.delete priorities editableKeys
          Just priority -> M.insert primaryKey priority priorities
      readIORef prioritiesRef >>= afterUpdate

menuIconSize :: Int32
menuIconSize = fromIntegral $ fromEnum Gtk.IconSizeMenu

showPriorityEditMenu ::
  Gtk.Widget ->
  Int ->
  Int ->
  Int ->
  Maybe Int ->
  (Maybe Int -> IO ()) ->
  IO ()
showPriorityEditMenu anchor priorityMin priorityMax defaultPriority currentExplicit onSelection = do
  currentEvent <- Gtk.getCurrentEvent
  menu <- Gtk.menuNew
  Gtk.menuAttachToWidget menu anchor Nothing

  let options = [priorityMax, priorityMax - 1 .. priorityMin]
  forM_ options $ \priority -> do
    let prefix =
          if currentExplicit == Just priority
            then "\x2713 " :: T.Text
            else "   "
        labelText = prefix <> "Set priority " <> T.pack (show priority)
    item <- Gtk.menuItemNewWithLabel labelText
    void $ Gtk.onMenuItemActivate item $ onSelection (Just priority)
    Gtk.menuShellAppend menu item

  sep <- Gtk.separatorMenuItemNew
  Gtk.menuShellAppend menu sep

  let clearPrefix =
        if isNothing currentExplicit
          then "\x2713 " :: T.Text
          else "   "
      clearLabel =
        clearPrefix
          <> "Clear override (default "
          <> T.pack (show defaultPriority)
          <> ")"
  clearItem <- Gtk.menuItemNewWithLabel clearLabel
  void $ Gtk.onMenuItemActivate clearItem $ onSelection Nothing
  Gtk.menuShellAppend menu clearItem

  void $
    Gtk.onWidgetHide menu $
      void $
        GLib.idleAdd GLib.PRIORITY_LOW $ do
          Gtk.widgetDestroy menu
          return False

  Gtk.widgetShowAll menu
  Gtk.menuPopupAtPointer menu currentEvent

showPriorityControlsMenu ::
  Gtk.EventBox ->
  Int ->
  Int ->
  Bool ->
  (Bool -> T.Text) ->
  IORef Bool ->
  IORef Bool ->
  IORef Int ->
  IORef Int ->
  IORef (Maybe Int) ->
  IO () ->
  IO () ->
  IO ()
showPriorityControlsMenu
  anchor
  priorityMin
  priorityMax
  alwaysShowExpandControl
  priorityModeLabel
  expandedRef
  priorityEditModeRef
  hiddenCountRef
  maxVisibleRef
  thresholdRef
  onControlStateChanged
  onSettingsChanged = do
    currentEvent <- Gtk.getCurrentEvent
    currentExpanded <- readIORef expandedRef
    currentPriorityEditMode <- readIORef priorityEditModeRef
    currentHiddenCount <- readIORef hiddenCountRef
    currentMaxVisible <- readIORef maxVisibleRef
    currentThreshold <- readIORef thresholdRef

    menu <- Gtk.menuNew
    Gtk.menuAttachToWidget menu anchor Nothing

    let showExpandControl =
          alwaysShowExpandControl || currentExpanded || currentHiddenCount > 0
    when showExpandControl $ do
      let expandLabel =
            if currentExpanded
              then "Allow tray icon hiding" :: T.Text
              else "Show all tray icons"
      expandItem <- Gtk.menuItemNewWithLabel expandLabel
      void $ Gtk.onMenuItemActivate expandItem $ do
        modifyIORef' expandedRef not
        onControlStateChanged
      Gtk.menuShellAppend menu expandItem

    let priorityModePrefix =
          if currentPriorityEditMode
            then "\x2713 " :: T.Text
            else "   "
        priorityModeItemLabel =
          priorityModePrefix
            <> "Priority edit mode: "
            <> priorityModeLabel currentPriorityEditMode
    priorityModeItem <- Gtk.menuItemNewWithLabel priorityModeItemLabel
    void $ Gtk.onMenuItemActivate priorityModeItem $ do
      modifyIORef' priorityEditModeRef not
      onControlStateChanged
    Gtk.menuShellAppend menu priorityModeItem

    controlsSep <- Gtk.separatorMenuItemNew
    Gtk.menuShellAppend menu controlsSep

    maxVisibleItem <- Gtk.menuItemNewWithLabel ("Max visible (collapsed)" :: T.Text)
    maxVisibleMenu <- Gtk.menuNew
    Gtk.menuItemSetSubmenu maxVisibleItem (Just maxVisibleMenu)
    let maxVisibleOptions = [0 .. 20]
    forM_ maxVisibleOptions $ \option -> do
      let optionLabel =
            if option <= 0
              then "No limit" :: T.Text
              else T.pack (show option)
          prefix =
            if option == currentMaxVisible
              then "\x2713 " :: T.Text
              else "   "
      item <- Gtk.menuItemNewWithLabel (prefix <> optionLabel)
      void $ Gtk.onMenuItemActivate item $ do
        writeIORef maxVisibleRef option
        onSettingsChanged
      Gtk.menuShellAppend maxVisibleMenu item
    Gtk.menuShellAppend menu maxVisibleItem

    thresholdItem <- Gtk.menuItemNewWithLabel ("Priority threshold" :: T.Text)
    thresholdMenu <- Gtk.menuNew
    Gtk.menuItemSetSubmenu thresholdItem (Just thresholdMenu)
    let thresholdOptions = Nothing : map Just [priorityMin .. priorityMax]
    forM_ thresholdOptions $ \option -> do
      let optionLabel =
            case option of
              Nothing -> "No threshold" :: T.Text
              Just value -> ">= " <> T.pack (show value)
          prefix =
            if option == currentThreshold
              then "\x2713 " :: T.Text
              else "   "
      item <- Gtk.menuItemNewWithLabel (prefix <> optionLabel)
      void $ Gtk.onMenuItemActivate item $ do
        writeIORef thresholdRef option
        onSettingsChanged
      Gtk.menuShellAppend thresholdMenu item
    Gtk.menuShellAppend menu thresholdItem

    void $
      Gtk.onWidgetHide menu $
        void $
          GLib.idleAdd GLib.PRIORITY_LOW $ do
            Gtk.widgetDestroy menu
            return False

    Gtk.widgetShowAll menu
    Gtk.menuPopupAtPointer menu currentEvent

-- | Build a collapsible StatusNotifierItem tray with priority editing controls
-- and persisted priority state.
sniTrayPrioritizedCollapsibleNew :: TaffyIO Gtk.Widget
sniTrayPrioritizedCollapsibleNew =
  sniTrayPrioritizedCollapsibleNewFromParams defaultPrioritizedCollapsibleSNITrayParams

-- | Build a prioritized collapsible tray from custom params.
sniTrayPrioritizedCollapsibleNewFromParams ::
  PrioritizedCollapsibleSNITrayParams -> TaffyIO Gtk.Widget
sniTrayPrioritizedCollapsibleNewFromParams params =
  getTrayHost False >>= sniTrayPrioritizedCollapsibleNewFromHostParams params

-- | Build a prioritized collapsible tray from custom params and a host.
sniTrayPrioritizedCollapsibleNewFromHostParams ::
  PrioritizedCollapsibleSNITrayParams -> H.Host -> TaffyIO Gtk.Widget
sniTrayPrioritizedCollapsibleNewFromHostParams PrioritizedCollapsibleSNITrayParams {..} host = do
  client <- asks sessionDBusClient
  lift $ do
    let CollapsibleSNITrayParams {..} = prioritizedCollapsibleSNITrayParams
        SNITrayConfig {..} = collapsibleSNITrayConfig
        rawPriorityMin = prioritizedCollapsibleSNITrayPriorityMin
        rawPriorityMax = prioritizedCollapsibleSNITrayPriorityMax
        priorityMin = min rawPriorityMin rawPriorityMax
        priorityMax = max rawPriorityMin rawPriorityMax
        clampPriority = clampPriorityInRange priorityMin priorityMax
        defaultPriority = clampPriority prioritizedCollapsibleSNITrayDefaultPriority
        visibilityThreshold = clampPriority <$> prioritizedCollapsibleSNITrayVisibilityThreshold
        trayOrientation' = trayOrientation sniTrayTrayParams
        highPriorityFirstInTrayOrder = True

    statePath <- resolveSNIPriorityStateFile prioritizedCollapsibleSNITrayPriorityStateFile
    persistedPriorityFile <- loadSNIPriorityFileFromFile statePath
    let persistedPriorities =
          M.map clampPriority (sniPriorityFilePriorities persistedPriorityFile)
        initialMaxVisibleIcons =
          max
            0
            ( fromMaybe
                collapsibleSNITrayMaxVisibleIcons
                (sniPriorityFileMaxVisibleIcons persistedPriorityFile)
            )
        initialVisibilityThreshold =
          case sniPriorityFileVisibilityThreshold persistedPriorityFile of
            Nothing -> visibilityThreshold
            Just persistedThreshold -> clampPriority <$> persistedThreshold
    prioritiesRef <- newIORef persistedPriorities
    expandedRef <- newIORef collapsibleSNITrayStartExpanded
    hoverExpandedRef <- newIORef False
    hoverInsideRef <- newIORef False
    hoverSerialRef <- newIORef (0 :: Int)
    animationSerialRef <- newIORef (0 :: Int)
    animationActiveRef <- newIORef False
    priorityEditModeRef <- newIORef prioritizedCollapsibleSNITrayStartPriorityEditMode
    maxVisibleIconsRef <- newIORef initialMaxVisibleIcons
    visibilityThresholdRef <- newIORef initialVisibilityThreshold
    hiddenCountRef <- newIORef 0
    orderedInfosRef <- newIORef ([] :: [H.ItemInfo])
    processDisambiguationKeysRef <- newIORef (M.empty :: M.Map String String)
    trayRef <- newIORef Nothing
    updateHandlerRef <- newIORef Nothing

    outer <- Gtk.boxNew trayOrientation' 0
    _ <- widgetSetClassGI outer "sni-tray-collapsible"
    _ <- widgetSetClassGI outer "sni-tray-prioritized-collapsible"
    outerEventBox <- Gtk.eventBoxNew
    _ <- widgetSetClassGI outerEventBox "sni-tray-hover-expand-target"
    Gtk.containerAdd outerEventBox outer
    outerWidget <- Gtk.toWidget outerEventBox

    trayContainer <- Gtk.boxNew trayOrientation' 0
    _ <- widgetSetClassGI trayContainer "sni-tray-collapsible-container"
    trayClipper <-
      Gtk.scrolledWindowNew
        (Nothing :: Maybe Gtk.Adjustment)
        (Nothing :: Maybe Gtk.Adjustment)
    Gtk.scrolledWindowSetPolicy trayClipper Gtk.PolicyTypeNever Gtk.PolicyTypeNever
    Gtk.scrolledWindowSetShadowType trayClipper Gtk.ShadowTypeNone
    Gtk.scrolledWindowSetOverlayScrolling trayClipper False
    Gtk.scrolledWindowSetPropagateNaturalWidth trayClipper False
    Gtk.scrolledWindowSetPropagateNaturalHeight trayClipper False
    _ <- widgetSetClassGI trayClipper "sni-tray-collapsible-clipper"

    overflowCountLabel <- Gtk.labelNew Nothing
    _ <- widgetSetClassGI overflowCountLabel "sni-tray-overflow-count-label"

    settingsIcon <- Gtk.imageNewFromIconName (Just "emblem-system-symbolic") menuIconSize
    settingsContent <- Gtk.boxNew trayOrientation' 3
    _ <- widgetSetClassGI settingsContent "sni-tray-settings-toggle-content"
    Gtk.boxPackStart settingsContent settingsIcon False False 0
    Gtk.boxPackStart settingsContent overflowCountLabel False False 0
    settingsToggle <- Gtk.eventBoxNew
    _ <- widgetSetClassGI settingsToggle "sni-tray-settings-toggle"
    Gtk.containerAdd settingsToggle settingsContent
    Gtk.widgetSetTooltipText settingsToggle (Just "Tray controls")

    Gtk.boxPackStart outer trayContainer False False 0
    Gtk.boxPackStart outer settingsToggle False False 0

    let persistCurrentState = do
          priorities <- readIORef prioritiesRef
          maxVisibleIcons <- readIORef maxVisibleIconsRef
          visibilityThresholdOverride <- readIORef visibilityThresholdRef
          persistSNIPriorityFileToFile
            statePath
            SNIPriorityFile
              { sniPriorityFilePriorities = priorities,
                sniPriorityFileMaxVisibleIcons = Just (max 0 maxVisibleIcons),
                sniPriorityFileVisibilityThreshold = Just visibilityThresholdOverride
              }

        processKeyForInfoFromMap processKeyMap info =
          M.lookup (itemStableIdentity info) processKeyMap

        updateOrderedInfos recomputeProcessKeys = do
          infoMap <- H.itemInfoMap host
          let infos = M.elems infoMap
          processKeyMap <-
            if recomputeProcessKeys
              then processDisambiguationKeysForItems client infos
              else readIORef processDisambiguationKeysRef
          when recomputeProcessKeys $
            writeIORef processDisambiguationKeysRef processKeyMap
          priorities <- readIORef prioritiesRef
          let priorityForInfo info =
                itemPriorityFromMap
                  priorityMin
                  priorityMax
                  defaultPriority
                  priorities
                  (processKeyForInfoFromMap processKeyMap info)
                  info
              orderedInfos =
                sortedInfosByPriority
                  highPriorityFirstInTrayOrder
                  priorityMin
                  priorityMax
                  defaultPriority
                  priorities
                  (processKeyForInfoFromMap processKeyMap)
                  infos
          writeIORef orderedInfosRef orderedInfos
          prioritizedTrayLog DEBUG $
            printf
              "Prioritized tray computed order recomputeProcessKeys=%s count=%d order=[%s]"
              (show recomputeProcessKeys)
              (length orderedInfos)
              (describeOrderedInfos priorityForInfo (processKeyForInfoFromMap processKeyMap) orderedInfos)
          return orderedInfos

        scheduleRefresh recomputeProcessKeys waitForExactChildCount updateType = do
          attemptsRef <- newIORef (0 :: Int)
          void $
            Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT $
              do
                orderedInfos <- updateOrderedInfos recomputeProcessKeys
                maybeTray <- readIORef trayRef
                case maybeTray of
                  Nothing -> return False
                  Just tray -> do
                    childCount <- length <$> Gtk.containerGetChildren tray
                    let expectedChildCount = length orderedInfos
                    attempts <- readIORef attemptsRef
                    if waitForExactChildCount && childCount /= expectedChildCount && attempts < 50
                      then do
                        when (attempts == 0 || attempts == 49) $
                          prioritizedTrayLog DEBUG $
                            printf
                              "Delaying prioritized tray refresh update=%s; attempt=%d childCount=%d expected=%d"
                              (show updateType)
                              attempts
                              childCount
                              expectedChildCount
                        writeIORef attemptsRef (attempts + 1)
                        return True
                      else do
                        void $ refreshTray tray
                        return False

        editPriorityForClick clickContext = do
          let clickedInfo = trayClickItemInfo clickContext
          priorities <- readIORef prioritiesRef
          processKeyMap <- readIORef processDisambiguationKeysRef
          let maybeProcessKey = processKeyForInfoFromMap processKeyMap clickedInfo
              currentExplicit =
                lookupExplicitPriority priorities maybeProcessKey clickedInfo
              updatePriority newPriority = do
                setExplicitPriorityForItem
                  prioritiesRef
                  (\_ -> persistCurrentState >> scheduleRefresh False False H.IconUpdated)
                  maybeProcessKey
                  clickedInfo
                  (fmap clampPriority newPriority)
          showPriorityEditMenu
            outerWidget
            priorityMin
            priorityMax
            defaultPriority
            currentExplicit
            updatePriority

        refreshPriorityModeToggle = do
          editing <- readIORef priorityEditModeRef
          if editing
            then do
              addClassIfMissing "sni-tray-editing" outer
            else do
              removeClassIfPresent "sni-tray-editing" outer

        getEffectiveExpanded = do
          expanded <- readIORef expandedRef
          hoverExpanded <- readIORef hoverExpandedRef
          return (expanded || hoverExpanded)

        computeNaturalVisibleCount totalCount = do
          effectiveExpanded <- getEffectiveExpanded
          priorities <- readIORef prioritiesRef
          maxVisibleIcons <- readIORef maxVisibleIconsRef
          thresholdValue <- readIORef visibilityThresholdRef
          orderedInfos <- readIORef orderedInfosRef
          processKeyMap <- readIORef processDisambiguationKeysRef
          let itemPriority info =
                itemPriorityFromMap
                  priorityMin
                  priorityMax
                  defaultPriority
                  priorities
                  (processKeyForInfoFromMap processKeyMap info)
                  info
              collapsedThresholdVisibleCount =
                case thresholdValue of
                  Nothing -> totalCount
                  Just threshold ->
                    min
                      totalCount
                      (length $ filter (\info -> itemPriority info >= threshold) orderedInfos)
              collapsedVisibleCount
                | maxVisibleIcons > 0 =
                    min collapsedThresholdVisibleCount maxVisibleIcons
                | otherwise = collapsedThresholdVisibleCount
          return $
            if effectiveExpanded
              then totalCount
              else collapsedVisibleCount

        childPreferredExtent child =
          case trayOrientation' of
            Gtk.OrientationVertical -> snd <$> Gtk.widgetGetPreferredHeight child
            _ -> snd <$> Gtk.widgetGetPreferredWidth child

        preferredTrayExtentForCount tray children visibleCount = do
          childExtents <- mapM childPreferredExtent (take visibleCount children)
          spacing <- Gtk.boxGetSpacing tray
          let spacingExtent =
                if visibleCount > 1
                  then fromIntegral (visibleCount - 1) * spacing
                  else 0
          return $ sum childExtents + spacingExtent

        setTrayClipperExtent extent =
          case trayOrientation' of
            Gtk.OrientationVertical ->
              Gtk.widgetSetSizeRequest trayClipper (-1) (fromIntegral extent)
            _ ->
              Gtk.widgetSetSizeRequest trayClipper (fromIntegral extent) (-1)

        allocatedTrayClipperExtent = do
          allocatedExtent <-
            case trayOrientation' of
              Gtk.OrientationVertical -> Gtk.widgetGetAllocatedHeight trayClipper
              _ -> Gtk.widgetGetAllocatedWidth trayClipper
          return $ max 0 allocatedExtent

        easeOutCubic :: Double -> Double
        easeOutCubic progress =
          1 - ((1 - progress) ^ (3 :: Int))

        animateTrayExtentTo tray targetVisibleCount = do
          modifyIORef' animationSerialRef (+ 1)
          animationSerial <- readIORef animationSerialRef
          writeIORef animationActiveRef True
          children <- Gtk.containerGetChildren tray
          forM_ children Gtk.widgetShow
          startExtent <- allocatedTrayClipperExtent
          targetExtent <- preferredTrayExtentForCount tray children targetVisibleCount
          let durationMs = prioritizedCollapsibleSNITrayHoverAnimationDurationMs
          if durationMs == 0 || startExtent == targetExtent
            then do
              setTrayClipperExtent targetExtent
              writeIORef animationActiveRef False
              void $ refreshTray tray
            else do
              startTime <- GLib.getMonotonicTime
              let durationUs = fromIntegral durationMs * 1000 :: Int64
                  extentAt now =
                    let elapsed = max 0 (now - startTime)
                        progress =
                          min
                            1
                            (fromIntegral elapsed / fromIntegral durationUs :: Double)
                        eased = easeOutCubic progress
                     in round $
                          fromIntegral startExtent
                            + (fromIntegral (targetExtent - startExtent) * eased :: Double)
              void $
                GLib.timeoutAdd GLib.PRIORITY_DEFAULT 16 $ do
                  currentSerial <- readIORef animationSerialRef
                  if currentSerial /= animationSerial
                    then return False
                    else do
                      now <- GLib.getMonotonicTime
                      setTrayClipperExtent (extentAt now)
                      if now - startTime >= durationUs
                        then do
                          setTrayClipperExtent targetExtent
                          writeIORef animationActiveRef False
                          void $ refreshTray tray
                          return False
                        else return True

        setHoverExpanded shouldExpand = do
          wasHoverExpanded <- readIORef hoverExpandedRef
          writeIORef hoverExpandedRef shouldExpand
          maybeTray <- readIORef trayRef
          case maybeTray of
            Nothing -> return ()
            Just tray ->
              when (wasHoverExpanded /= shouldExpand) $
                do
                  children <- Gtk.containerGetChildren tray
                  targetVisibleCount <- computeNaturalVisibleCount (length children)
                  animateTrayExtentTo tray targetVisibleCount

        scheduleHoverExpanded shouldExpand delayMs = do
          modifyIORef' hoverSerialRef (+ 1)
          hoverSerial <- readIORef hoverSerialRef
          void $
            GLib.timeoutAdd GLib.PRIORITY_DEFAULT delayMs $ do
              currentSerial <- readIORef hoverSerialRef
              hoverInside <- readIORef hoverInsideRef
              when
                ( currentSerial == hoverSerial
                    && hoverInside == shouldExpand
                )
                (setHoverExpanded shouldExpand)
              return False

        refreshTray tray = do
          effectiveExpanded <- getEffectiveExpanded
          animationActive <- readIORef animationActiveRef
          orderedInfos <- readIORef orderedInfosRef
          reorderTrayChildrenByIdentities tray (map itemStableIdentity orderedInfos)
          children <- Gtk.containerGetChildren tray

          naturalVisibleCount <- computeNaturalVisibleCount (length children)
          priorities <- readIORef prioritiesRef
          processKeyMap <- readIORef processDisambiguationKeysRef
          let totalCount = length children
              visibleCount =
                max 0 (min totalCount naturalVisibleCount)
              hiddenCount = max 0 (totalCount - visibleCount)
              hiddenCountText = T.pack (show hiddenCount)
              priorityForInfo info =
                itemPriorityFromMap
                  priorityMin
                  priorityMax
                  defaultPriority
                  priorities
                  (processKeyForInfoFromMap processKeyMap info)
                  info
              orderedInfoByIdentity =
                M.fromList [(itemStableIdentity info, info) | info <- orderedInfos]
              describeChildIdentity index maybeIdentity =
                case maybeIdentity >>= (`M.lookup` orderedInfoByIdentity) of
                  Just info ->
                    printf
                      "%d:{%s}"
                      index
                      (describeItemForOrder priorityForInfo (processKeyForInfoFromMap processKeyMap) info)
                  Nothing ->
                    printf
                      "%d:{identity=%s}"
                      index
                      (formatMaybe maybeIdentity)
          childIdentities <- mapM Tray.getTrayItemIdentity children
          prioritizedTrayLog DEBUG $
            printf
              "Prioritized tray rendered order expanded=%s animating=%s visible=%d hidden=%d total=%d children=[%s]"
              (show effectiveExpanded)
              (show animationActive)
              visibleCount
              hiddenCount
              totalCount
              (intercalate " | " (zipWith describeChildIdentity [0 :: Int ..] childIdentities))

          forM_ (zip [0 :: Int ..] children) $ \(childIndex, child) -> do
            let shouldShow = animationActive || childIndex < visibleCount
            isVisible <- Gtk.widgetGetVisible child
            when (isVisible /= shouldShow) $
              if shouldShow
                then Gtk.widgetShow child
                else Gtk.widgetHide child
          unless animationActive $ do
            visibleExtent <- preferredTrayExtentForCount tray children visibleCount
            setTrayClipperExtent visibleExtent
          writeIORef hiddenCountRef hiddenCount

          if hiddenCount > 0
            then do
              Gtk.labelSetText overflowCountLabel hiddenCountText
              Gtk.widgetShow overflowCountLabel
            else do
              Gtk.labelSetText overflowCountLabel ""
              Gtk.widgetHide overflowCountLabel

          if effectiveExpanded
            then addClassIfMissing "sni-tray-collapsible-expanded" outer
            else removeClassIfPresent "sni-tray-collapsible-expanded" outer

          hoverExpanded <- readIORef hoverExpandedRef
          if hoverExpanded
            then addClassIfMissing "sni-tray-hover-expanded" outer
            else removeClassIfPresent "sni-tray-hover-expanded" outer

          return hiddenCount

        refresh = do
          maybeTray <- readIORef trayRef
          case maybeTray of
            Nothing -> return 0
            Just tray -> refreshTray tray

        queueRefresh updateType _ =
          case updateType of
            H.ItemAdded -> scheduleRefresh True True updateType
            H.ItemRemoved -> scheduleRefresh True True updateType
            H.IconUpdated -> scheduleRefresh False False updateType
            H.TitleUpdated -> scheduleRefresh False False updateType
            H.OverlayIconUpdated -> return ()
            H.ToolTipUpdated -> return ()
            H.StatusUpdated -> return ()

        installUpdateHandler = do
          maybeHandlerId <- readIORef updateHandlerRef
          case maybeHandlerId of
            Just handlerId ->
              prioritizedTrayLog DEBUG $
                printf
                  "installUpdateHandler: handler already registered id=%d"
                  (hashUnique handlerId)
            Nothing -> do
              handlerId <- H.addUpdateHandler host queueRefresh
              prioritizedTrayLog INFO $
                printf
                  "Registered prioritized tray host update handler id=%d"
                  (hashUnique handlerId)
              writeIORef updateHandlerRef (Just handlerId)

        buildTrayForPrioritizedWrapper = do
          let baseHooks = trayEventHooks sniTrayTrayParams
              baseClickHook = trayClickHook baseHooks
              combinedClickHook clickContext = do
                editMode <- readIORef priorityEditModeRef
                if editMode
                  then do
                    editPriorityForClick clickContext
                    return ConsumeClick
                  else case baseClickHook of
                    Just clickHook -> clickHook clickContext
                    Nothing -> return UseDefaultClickAction
              trayParams =
                -- This wrapper owns ordering and visibility. Keep the inner
                -- tray as a widget factory with straightforward child packing.
                sniTrayTrayParams
                  { trayAlignment = Beginning,
                    trayPriorityConfig = Tray.defaultTrayPriorityConfig,
                    trayEventHooks =
                      baseHooks {trayClickHook = Just combinedClickHook},
                    trayShowNewIconsImmediately = False
                  }
          tray <- buildTray host client trayParams
          _ <- widgetSetClassGI tray "sni-tray"
          return tray

    _ <- Gtk.onWidgetButtonPressEvent settingsToggle $ \event -> do
      eventType <- Gdk.getEventButtonType event
      button <- Gdk.getEventButtonButton event
      if eventType == Gdk.EventTypeButtonPress && button == 1
        then do
          showPriorityControlsMenu
            settingsToggle
            priorityMin
            priorityMax
            prioritizedCollapsibleSNITrayAlwaysShowExpandToggle
            prioritizedCollapsibleSNITrayPriorityModeLabel
            expandedRef
            priorityEditModeRef
            hiddenCountRef
            maxVisibleIconsRef
            visibilityThresholdRef
            (refreshPriorityModeToggle >> void refresh)
            (persistCurrentState >> scheduleRefresh False False H.ToolTipUpdated)
          return True
        else return False

    when prioritizedCollapsibleSNITrayHoverExpand $ do
      Gtk.widgetAddEvents
        outerEventBox
        [ Gdk.EventMaskEnterNotifyMask,
          Gdk.EventMaskLeaveNotifyMask
        ]
      Gtk.widgetAddEvents settingsToggle [Gdk.EventMaskEnterNotifyMask]
      _ <- Gtk.onWidgetEnterNotifyEvent outerEventBox $ \_event -> do
        writeIORef hoverInsideRef True
        return False
      _ <- Gtk.onWidgetLeaveNotifyEvent outerEventBox $ \_event -> do
        writeIORef hoverInsideRef False
        scheduleHoverExpanded False prioritizedCollapsibleSNITrayHoverCollapseDelayMs
        return False
      _ <- Gtk.onWidgetEnterNotifyEvent settingsToggle $ \_event -> do
        writeIORef hoverInsideRef True
        scheduleHoverExpanded True prioritizedCollapsibleSNITrayHoverExpandDelayMs
        return False
      return ()

    _ <-
      Gtk.onWidgetDestroy outerEventBox $
        readIORef updateHandlerRef
          >>= traverse_
            ( \handlerId -> do
                prioritizedTrayLog INFO $
                  printf
                    "Removing prioritized tray host update handler id=%d"
                    (hashUnique handlerId)
                H.removeUpdateHandler host handlerId
            )

    _ <- updateOrderedInfos True
    tray <- buildTrayForPrioritizedWrapper
    Gtk.containerAdd trayClipper tray
    Gtk.boxPackStart trayContainer trayClipper False False 0
    writeIORef trayRef (Just tray)
    installUpdateHandler
    Gtk.widgetShow tray

    Gtk.widgetShowAll outerWidget
    refreshPriorityModeToggle
    _ <- refresh
    return outerWidget
