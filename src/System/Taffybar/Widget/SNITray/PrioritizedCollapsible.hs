{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

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

import Control.Monad (forM_, void)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Applicative ((<|>))
import qualified Data.Aeson as A
import Data.Aeson.Types (Parser)
import Data.IORef
import Data.Int (Int32)
import Data.List (sortOn)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe, maybeToList)
import Data.Ord (Down (..))
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified GI.GLib as GLib
import qualified GI.Gdk as Gdk
import qualified GI.Gtk as Gtk
import Graphics.UI.GIGtkStrut (StrutAlignment (End))
import qualified StatusNotifier.Host.Service as H
import StatusNotifier.Tray
import qualified StatusNotifier.Tray as Tray
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Environment.XDG.BaseDir (getUserConfigFile)
import System.FilePath (isRelative, replaceExtension, takeDirectory, takeExtension)
import System.Taffybar.Context
import System.Taffybar.Widget.SNITray
  ( CollapsibleSNITrayParams (..),
    SNITrayConfig (..),
    defaultCollapsibleSNITrayParams,
    getTrayHost,
  )
import System.Taffybar.Widget.Util
import Text.Read (readMaybe)
import qualified Data.Yaml as Y

type SNIPriorityMap = M.Map String Int

data SNIPriorityEntry = SNIPriorityEntry
  { sniPriorityEntryKey :: String,
    sniPriorityEntryPriority :: Int
  }

newtype SNIPriorityFile = SNIPriorityFile
  { sniPriorityFilePriorities :: SNIPriorityMap
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
    return $ SNIPriorityFile priorities

instance A.ToJSON SNIPriorityFile where
  toJSON (SNIPriorityFile priorities) =
    let sortedEntries =
          map
            (\(key, priority) -> SNIPriorityEntry key priority)
            (sortOn (\(key, priority) -> (Down priority, key)) (M.toList priorities))
     in
      A.object
        [ "format_version" A..= (1 :: Int),
          "priorities" A..= sortedEntries
        ]

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

parseSNIPriorityMap :: BS.ByteString -> Maybe SNIPriorityMap
parseSNIPriorityMap content =
  parseYaml <|> parseLegacySNIPriorityMap content
  where
    parseYaml =
      either
        (const Nothing)
        (Just . sniPriorityFilePriorities)
        (Y.decodeEither' content :: Either Y.ParseException SNIPriorityFile)

loadSNIPriorityMapFromPath :: FilePath -> IO (Maybe SNIPriorityMap)
loadSNIPriorityMapFromPath path = do
  exists <- doesFileExist path
  if not exists
    then return Nothing
    else parseSNIPriorityMap <$> BS.readFile path

legacyPriorityStateFallbackPath :: FilePath -> Maybe FilePath
legacyPriorityStateFallbackPath path
  | takeExtension path == ".yaml" = Just (replaceExtension path "dat")
  | otherwise = Nothing

loadSNIPriorityMapFromFile :: FilePath -> IO SNIPriorityMap
loadSNIPriorityMapFromFile path = do
  loaded <- loadSNIPriorityMapFromPath path
  case loaded of
    Just priorities -> return priorities
    Nothing ->
      case legacyPriorityStateFallbackPath path of
        Nothing -> return M.empty
        Just legacyPath -> do
          fallbackLoaded <- loadSNIPriorityMapFromPath legacyPath
          case fallbackLoaded of
            Nothing -> return M.empty
            Just priorities -> do
              -- One-time migration path for existing show/read .dat files.
              persistSNIPriorityMapToFile path priorities
              return priorities

persistSNIPriorityMapToFile :: FilePath -> SNIPriorityMap -> IO ()
persistSNIPriorityMapToFile path priorities = do
  createDirectoryIfMissing True (takeDirectory path)
  BS.writeFile path (Y.encode (SNIPriorityFile priorities))

nonEmptyString :: String -> Maybe String
nonEmptyString value
  | null value = Nothing
  | otherwise = Just value

priorityKeyCandidates :: H.ItemInfo -> [String]
priorityKeyCandidates info =
  concat
    [ map ("item-id:" ++) (maybeToList (H.itemId info >>= nonEmptyString)),
      map ("icon-name:" ++) (maybeToList (nonEmptyString (H.iconName info))),
      map ("icon-title:" ++) (maybeToList (nonEmptyString (H.iconTitle info)))
    ]

priorityKeyFromItem :: H.ItemInfo -> Maybe String
priorityKeyFromItem = listToMaybe . priorityKeyCandidates

itemPriorityFromMap ::
  Int ->
  Int ->
  Int ->
  SNIPriorityMap ->
  H.ItemInfo ->
  Int
itemPriorityFromMap priorityMin priorityMax defaultPriority priorities info =
  let clampPriority = clampPriorityInRange priorityMin priorityMax
      matchedPriority =
        listToMaybe $
          mapMaybe (`M.lookup` priorities) (priorityKeyCandidates info)
   in clampPriority (fromMaybe defaultPriority matchedPriority)

itemStableIdentity :: H.ItemInfo -> String
itemStableIdentity info =
  show (H.itemServiceName info) <> "|" <> show (H.itemServicePath info)

itemIdentityMatcher :: H.ItemInfo -> Tray.TrayItemMatcher
itemIdentityMatcher info =
  let stableIdentity = itemStableIdentity info
   in mkTrayItemMatcher
        ("priority:identity:" <> stableIdentity)
        (\candidate -> itemStableIdentity candidate == stableIdentity)

priorityMatchersFromMapAndItems ::
  Bool ->
  Int ->
  Int ->
  Int ->
  SNIPriorityMap ->
  [H.ItemInfo] ->
  [Tray.TrayItemMatcher]
priorityMatchersFromMapAndItems highPriorityFirstInMatcherOrder priorityMin priorityMax defaultPriority priorities infos =
  let sortedInfos =
        sortedInfosByPriority
          highPriorityFirstInMatcherOrder
          priorityMin
          priorityMax
          defaultPriority
          priorities
          infos
      fallbackMatcher = mkTrayItemMatcher "priority:identity:fallback" (const True)
   in map itemIdentityMatcher sortedInfos ++ [fallbackMatcher]

sortedInfosByPriority ::
  Bool ->
  Int ->
  Int ->
  Int ->
  SNIPriorityMap ->
  [H.ItemInfo] ->
  [H.ItemInfo]
sortedInfosByPriority highPriorityFirstInMatcherOrder priorityMin priorityMax defaultPriority priorities infos =
  let itemPriority info =
        itemPriorityFromMap priorityMin priorityMax defaultPriority priorities info
      prioritySortKey info =
        if highPriorityFirstInMatcherOrder
          then negate (itemPriority info)
          else itemPriority info
   in
    -- Matchers may need to be reversed to keep higher numeric priorities on the
    -- visual left when the tray is end-aligned.
    sortOn
      (\info -> (prioritySortKey info, itemStableIdentity info))
      infos

lookupExplicitPriority ::
  SNIPriorityMap ->
  H.ItemInfo ->
  Maybe Int
lookupExplicitPriority priorities info =
  listToMaybe $ mapMaybe (`M.lookup` priorities) (priorityKeyCandidates info)

setExplicitPriorityForItem ::
  IORef SNIPriorityMap ->
  (SNIPriorityMap -> IO ()) ->
  H.ItemInfo ->
  Maybe Int ->
  IO ()
setExplicitPriorityForItem prioritiesRef afterUpdate info newPriority =
  case priorityKeyFromItem info of
    Nothing -> return ()
    Just primaryKey -> do
      let candidateKeys = priorityKeyCandidates info
      modifyIORef' prioritiesRef $ \priorities ->
        let cleared = foldr M.delete priorities candidateKeys
         in case newPriority of
              Nothing -> cleared
              Just priority -> M.insert primaryKey priority cleared
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
        if currentExplicit == Nothing
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

showPrioritySettingsMenu ::
  Gtk.EventBox ->
  Int ->
  Int ->
  IORef Int ->
  IORef (Maybe Int) ->
  IO () ->
  IO ()
showPrioritySettingsMenu anchor priorityMin priorityMax maxVisibleRef thresholdRef onSettingsChanged = do
  currentEvent <- Gtk.getCurrentEvent
  currentMaxVisible <- readIORef maxVisibleRef
  currentThreshold <- readIORef thresholdRef

  menu <- Gtk.menuNew
  Gtk.menuAttachToWidget menu anchor Nothing

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
        highPriorityFirstInMatcherOrder =
          case trayAlignment sniTrayTrayParams of
            End -> False
            _ -> True

    statePath <- resolveSNIPriorityStateFile prioritizedCollapsibleSNITrayPriorityStateFile
    persistedPriorities <- loadSNIPriorityMapFromFile statePath
    prioritiesRef <- newIORef (M.map clampPriority persistedPriorities)
    expandedRef <- newIORef collapsibleSNITrayStartExpanded
    priorityEditModeRef <- newIORef prioritizedCollapsibleSNITrayStartPriorityEditMode
    maxVisibleIconsRef <- newIORef collapsibleSNITrayMaxVisibleIcons
    visibilityThresholdRef <- newIORef visibilityThreshold
    knownItemIdentitiesRef <- newIORef ([] :: [String])
    orderedInfosRef <- newIORef ([] :: [H.ItemInfo])
    trayRef <- newIORef Nothing
    rebuildTrayRef <- newIORef (return ())

    outer <- Gtk.boxNew trayOrientation' 0
    _ <- widgetSetClassGI outer "sni-tray-collapsible"
    _ <- widgetSetClassGI outer "sni-tray-prioritized-collapsible"
    outerWidget <- Gtk.toWidget outer

    trayContainer <- Gtk.boxNew trayOrientation' 0
    _ <- widgetSetClassGI trayContainer "sni-tray-collapsible-container"

    overflowCountLabel <- Gtk.labelNew Nothing
    _ <- widgetSetClassGI overflowCountLabel "sni-tray-overflow-count-label"

    expandIcon <- Gtk.imageNewFromIconName (Just "pan-down-symbolic") menuIconSize
    expandToggle <- Gtk.eventBoxNew
    _ <- widgetSetClassGI expandToggle "sni-tray-expand-toggle"
    Gtk.containerAdd expandToggle expandIcon

    priorityModeIcon <- Gtk.imageNewFromIconName (Just "document-edit-symbolic") menuIconSize
    priorityModeToggle <- Gtk.eventBoxNew
    _ <- widgetSetClassGI priorityModeToggle "sni-tray-edit-toggle"
    Gtk.containerAdd priorityModeToggle priorityModeIcon

    settingsIcon <- Gtk.imageNewFromIconName (Just "emblem-system-symbolic") menuIconSize
    settingsToggle <- Gtk.eventBoxNew
    _ <- widgetSetClassGI settingsToggle "sni-tray-settings-toggle"
    Gtk.containerAdd settingsToggle settingsIcon
    Gtk.widgetSetTooltipText settingsToggle (Just "Tray display settings")

    Gtk.boxPackStart outer trayContainer False False 0
    Gtk.boxPackStart outer overflowCountLabel False False 0
    Gtk.boxPackStart outer expandToggle False False 0
    Gtk.boxPackStart outer priorityModeToggle False False 0
    Gtk.boxPackStart outer settingsToggle False False 0

    let queueRebuild = do
          rebuild <- readIORef rebuildTrayRef
          void $
            Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT_IDLE $
              rebuild >> return False

        editPriorityForClick clickContext = do
          let clickedInfo = trayClickItemInfo clickContext
          priorities <- readIORef prioritiesRef
          let currentExplicit = lookupExplicitPriority priorities clickedInfo
              updatePriority newPriority = do
                setExplicitPriorityForItem
                  prioritiesRef
                  (\updatedPriorities -> persistSNIPriorityMapToFile statePath updatedPriorities >> queueRebuild)
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
          let tooltipText =
                if editing
                  then "Disable icon priority edit mode"
                  else "Enable icon priority edit mode"
          Gtk.widgetSetTooltipText
            priorityModeToggle
            (Just tooltipText)
          if editing
            then addClassIfMissing "sni-tray-edit-toggle-active" priorityModeToggle
            else removeClassIfPresent "sni-tray-edit-toggle-active" priorityModeToggle
          if editing
            then do
              addClassIfMissing "sni-tray-editing" outer
            else do
              removeClassIfPresent "sni-tray-editing" outer
          Gtk.widgetShowAll priorityModeToggle

        refresh = do
          maybeTray <- readIORef trayRef
          case maybeTray of
            Nothing -> return 0
            Just tray -> do
              children <- Gtk.containerGetChildren tray
              expanded <- readIORef expandedRef
              priorities <- readIORef prioritiesRef
              maxVisibleIcons <- readIORef maxVisibleIconsRef
              thresholdValue <- readIORef visibilityThresholdRef
              orderedInfos <- readIORef orderedInfosRef

              let itemPriority = itemPriorityFromMap priorityMin priorityMax defaultPriority priorities
                  totalCount = length children
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
                  visibleCount
                    | expanded = totalCount
                    | otherwise = collapsedVisibleCount
                  visibleChildren = take visibleCount children
                  hiddenChildren = drop visibleCount children
                  hiddenCount = length hiddenChildren
                  showExpandToggle =
                    prioritizedCollapsibleSNITrayAlwaysShowExpandToggle
                      || ( if expanded
                             then collapsibleSNITrayShowIndicatorWhenExpanded && hiddenCount > 0
                             else hiddenCount > 0
                         )
                  expandIconName =
                    if expanded
                      then "pan-up-symbolic"
                      else "pan-down-symbolic"
                  expandTooltip =
                    if expanded
                      then "Allow tray icon hiding"
                      else "Show all tray icons"
                  hiddenCountText = T.pack (show hiddenCount)

              mapM_ Gtk.widgetShow visibleChildren
              mapM_ Gtk.widgetHide hiddenChildren

              Gtk.imageSetFromIconName expandIcon (Just expandIconName) menuIconSize
              if showExpandToggle
                then do
                  Gtk.widgetSetTooltipText expandToggle (Just expandTooltip)
                  Gtk.widgetShowAll expandToggle
                else do
                  Gtk.widgetSetTooltipText expandToggle Nothing
                  Gtk.widgetHide expandToggle

              if hiddenCount > 0
                then do
                  Gtk.labelSetText overflowCountLabel hiddenCountText
                  Gtk.widgetShow overflowCountLabel
                else do
                  Gtk.labelSetText overflowCountLabel ""
                  Gtk.widgetHide overflowCountLabel

              if expanded
                then addClassIfMissing "sni-tray-collapsible-expanded" outer
                else removeClassIfPresent "sni-tray-collapsible-expanded" outer

              return hiddenCount

        buildTrayWithPriorities priorities infos = do
          let priorityConfig =
                sniTrayPriorityConfig
                  { trayPriorityMatchers =
                      priorityMatchersFromMapAndItems
                        highPriorityFirstInMatcherOrder
                        priorityMin
                        priorityMax
                        defaultPriority
                        priorities
                        infos
                  }
              baseHooks = trayEventHooks sniTrayTrayParams
              baseClickHook = trayClickHook baseHooks
              combinedClickHook clickContext = do
                editMode <- readIORef priorityEditModeRef
                if editMode
                  then do
                    editPriorityForClick clickContext
                    return ConsumeClick
                  else
                    case baseClickHook of
                      Just clickHook -> clickHook clickContext
                      Nothing -> return UseDefaultClickAction
              trayParams =
                sniTrayTrayParams
                  { trayEventHooks =
                      baseHooks {trayClickHook = Just combinedClickHook}
                  }
          tray <- buildTray host client trayParams {trayPriorityConfig = priorityConfig}
          _ <- widgetSetClassGI tray "sni-tray"
          return tray

        rebuildTray = do
          priorities <- readIORef prioritiesRef
          infoMap <- H.itemInfoMap host
          let infos = M.elems infoMap
              orderedInfos =
                sortedInfosByPriority
                  highPriorityFirstInMatcherOrder
                  priorityMin
                  priorityMax
                  defaultPriority
                  priorities
                  infos
              currentItemIdentities =
                sortOn id (map itemStableIdentity infos)
          tray <- buildTrayWithPriorities priorities orderedInfos
          oldTray <- readIORef trayRef
          forM_ oldTray $ \existingTray -> do
            Gtk.containerRemove trayContainer existingTray
            Gtk.widgetDestroy existingTray
          Gtk.boxPackStart trayContainer tray False False 0
          writeIORef trayRef (Just tray)
          writeIORef orderedInfosRef orderedInfos
          writeIORef knownItemIdentitiesRef currentItemIdentities
          Gtk.widgetShowAll trayContainer
          void refresh

    writeIORef rebuildTrayRef rebuildTray

    _ <- Gtk.onWidgetButtonPressEvent expandToggle $ \event -> do
      eventType <- Gdk.getEventButtonType event
      button <- Gdk.getEventButtonButton event
      if eventType == Gdk.EventTypeButtonPress && button == 1
        then do
          modifyIORef' expandedRef not
          void refresh
          return True
        else return False

    _ <- Gtk.onWidgetButtonPressEvent priorityModeToggle $ \event -> do
      eventType <- Gdk.getEventButtonType event
      button <- Gdk.getEventButtonButton event
      if eventType == Gdk.EventTypeButtonPress && button == 1
        then do
          modifyIORef' priorityEditModeRef not
          refreshPriorityModeToggle
          return True
        else return False

    _ <- Gtk.onWidgetButtonPressEvent settingsToggle $ \event -> do
      eventType <- Gdk.getEventButtonType event
      button <- Gdk.getEventButtonButton event
      if eventType == Gdk.EventTypeButtonPress && button == 1
        then do
          showPrioritySettingsMenu
            settingsToggle
            priorityMin
            priorityMax
            maxVisibleIconsRef
            visibilityThresholdRef
            (void refresh)
          return True
        else return False

    let queueRefresh updateType _ =
          void $
            Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT_IDLE $
              do
                case updateType of
                  H.ItemAdded -> do
                    infoMap <- H.itemInfoMap host
                    knownItemIdentities <- readIORef knownItemIdentitiesRef
                    let currentItemIdentities =
                          sortOn id (map itemStableIdentity (M.elems infoMap))
                    if currentItemIdentities /= knownItemIdentities
                      then do
                        rebuild <- readIORef rebuildTrayRef
                        rebuild
                      else void refresh
                  H.ItemRemoved -> do
                    infoMap <- H.itemInfoMap host
                    knownItemIdentities <- readIORef knownItemIdentitiesRef
                    let currentItemIdentities =
                          sortOn id (map itemStableIdentity (M.elems infoMap))
                    if currentItemIdentities /= knownItemIdentities
                      then do
                        rebuild <- readIORef rebuildTrayRef
                        rebuild
                      else void refresh
                  _ -> void refresh
                return False
    handlerId <- H.addUpdateHandler host queueRefresh
    _ <- Gtk.onWidgetDestroy outer $ H.removeUpdateHandler host handlerId

    rebuildTray
    refreshPriorityModeToggle
    _ <- refresh
    Gtk.widgetShowAll outer
    return outerWidget
