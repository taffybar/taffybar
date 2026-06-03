{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module StatusNotifier.Tray where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar as MV
import Control.Exception.Base
import Control.Exception.Enclosed (catchAny)
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import DBus.Client
import qualified DBus.Internal.Types as DBusTypes
import qualified DBusMenu
import qualified Data.ByteString as BS
import Data.Coerce
import Data.Foldable (traverse_)
import Data.GI.Base (unsafeCastTo)
import Data.GI.Base.GError
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Int
import Data.List
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Ord
import Data.Ratio
import qualified Data.Text as T
import Data.Unique (hashUnique, newUnique)
import Data.Word
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.StablePtr
  ( StablePtr,
    castPtrToStablePtr,
    castStablePtrToPtr,
    deRefStablePtr,
    freeStablePtr,
    newStablePtr,
  )
import qualified GI.DbusmenuGtk3.Objects.Menu as DM
import qualified GI.GLib as GLib
import GI.GLib.Structs.Bytes
import qualified GI.GObject as GObject
import qualified GI.Gdk as Gdk
import GI.Gdk.Enums
import GI.Gdk.Structs.EventScroll
import GI.GdkPixbuf.Enums
import GI.GdkPixbuf.Objects.Pixbuf as Gdk
import qualified GI.Gtk as Gtk
import GI.Gtk.Flags
import GI.Gtk.Objects.IconTheme
import Graphics.UI.GIGtkScalingImage
import Graphics.UI.GIGtkStrut
import StatusNotifier.Host.Service
import qualified StatusNotifier.Item.Client as IC
import qualified StatusNotifier.Tray.ContextMap as ContextMap
import System.Directory
import System.FilePath
import System.Log.Logger
import Text.Printf

trayLogger :: Priority -> String -> IO ()
trayLogger = logM "StatusNotifier.Tray"

dbusMenuLayoutPropNames :: [String]
dbusMenuLayoutPropNames =
  [ "type",
    "label",
    "visible",
    "enabled",
    "children-display",
    "toggle-type",
    "toggle-state"
  ]

-- | Optional post-processing hook for item icons. This is applied after scaling
-- and overlay composition.
type PixbufTransform = Gtk.Widget -> Pixbuf -> IO Pixbuf

logItemInfo :: ItemInfo -> String -> IO ()
logItemInfo info message =
  trayLogger INFO $
    printf
      "%s - %s pixmap count: %s"
      message
      (show $ info {iconPixmaps = []})
      (show $ length $ iconPixmaps info)

themeLoadFlags :: [IconLookupFlags]
themeLoadFlags = [IconLookupFlagsGenericFallback, IconLookupFlagsUseBuiltin]

getThemeWithOptionalSearchPath :: Maybe String -> IO IconTheme
getThemeWithOptionalSearchPath themePath = do
  theme <- iconThemeGetDefault
  forM_ (themePath >>= nonEmpty) $ \p -> do
    -- Respect the user's configured icon theme by using GTK's default theme
    -- object, but include any item-provided IconThemePath as an additional
    -- search path.
    --
    -- Some items provide IconThemePath pointing inside a theme dir, e.g.:
    --   .../Papirus/64x64/mimetypes
    -- GTK expects search paths that contain theme directories, so also try to
    -- append the parent directory of any ancestor that contains index.theme.
    pathsToAppend <- pathsForIconThemePath p
    existing <- iconThemeGetSearchPath theme
    forM_ pathsToAppend $ \p' ->
      unless (p' `elem` existing) $ iconThemeAppendSearchPath theme p'
  return theme
  where
    nonEmpty "" = Nothing
    nonEmpty x = Just x

pathsForIconThemePath :: FilePath -> IO [FilePath]
pathsForIconThemePath rawPath = do
  mThemeDir <- findAncestorWithIndexTheme 8 rawPath
  let base =
        case mThemeDir of
          Nothing -> []
          Just themeDir -> [takeDirectory themeDir]
  return $ nub $ base ++ [rawPath]
  where
    findAncestorWithIndexTheme :: Int -> FilePath -> IO (Maybe FilePath)
    findAncestorWithIndexTheme 0 _ = return Nothing
    findAncestorWithIndexTheme n p = do
      hasIndex <- doesFileExist (p </> "index.theme")
      if hasIndex
        then return (Just p)
        else do
          let parent = takeDirectory p
          if parent == p
            then return Nothing
            else findAncestorWithIndexTheme (n - 1) parent

catchGErrorsAsLeft :: IO a -> IO (Either GError a)
catchGErrorsAsLeft action = catch (Right <$> action) (return . Left)

catchGErrorsAsNothing :: IO a -> IO (Maybe a)
catchGErrorsAsNothing action = catchGErrorsAsLeft action >>= rightToJustLogLeft
  where
    rightToJustLogLeft (Right value) = return $ Just value
    rightToJustLogLeft (Left err) = do
      trayLogger WARNING $ printf "Encountered error: %s" $ show err
      return Nothing

safePixbufNewFromFile :: FilePath -> IO (Maybe Gdk.Pixbuf)
safePixbufNewFromFile =
  handleResult . catchGErrorsAsNothing . Gdk.pixbufNewFromFile
  where
#if MIN_VERSION_gi_gdkpixbuf(2,0,26)
    handleResult = fmap join
#else
    handleResult = id
#endif

getIconPixbufByName :: Int32 -> T.Text -> Maybe String -> IO (Maybe Pixbuf)
getIconPixbufByName size name themePath = do
  trayLogger DEBUG $ printf "Getting Pixbuf from name for %s" name
  themeForIcon <- getThemeWithOptionalSearchPath themePath

  let panelName = T.pack $ printf "%s-panel" name
  -- Avoid relying on iconThemeHasIcon: it can be overly strict when fallback
  -- loading is enabled. Just try to load and fall back if it fails.
  let tryLoad :: T.Text -> IO (Maybe Pixbuf)
      tryLoad themedName =
        catchAny
          (iconThemeLoadIcon themeForIcon themedName size themeLoadFlags)
          (const $ pure Nothing)

  themedPixbuf <- do
    pbPanel <- tryLoad panelName
    case pbPanel of
      Just _ -> return pbPanel
      Nothing -> tryLoad name

  case themedPixbuf of
    Just _ -> return themedPixbuf
    Nothing -> do
      trayLogger DEBUG $ printf "Trying to load icon %s as filepath" name
      -- Try to load the icon as a filepath
      let nameString = T.unpack name
      fileExists <- doesFileExist nameString
      maybeFile <-
        if fileExists
          then return $ Just nameString
          else join <$> traverse (getIconPathFromThemePath nameString) themePath
      join <$> traverse safePixbufNewFromFile maybeFile

getIconPathFromThemePath :: String -> String -> IO (Maybe String)
getIconPathFromThemePath name themePath =
  if name == ""
    then return Nothing
    else do
      trayLogger DEBUG $
        printf
          "Trying to load icon %s as filepath with theme path %s"
          name
          themePath
      pathExists <- doesDirectoryExist themePath
      if pathExists
        then do
          fileNames <- catchAny (listDirectory themePath) (const $ return [])
          trayLogger DEBUG $
            printf
              "Found files in theme path %s"
              (show fileNames)
          return $ (themePath </>) <$> find (isPrefixOf name) fileNames
        else return Nothing

getIconPixbufFromByteString :: Int32 -> Int32 -> BS.ByteString -> IO (Maybe Pixbuf)
getIconPixbufFromByteString width height byteString
  | width <= 0 || height <= 0 = do
      trayLogger WARNING $ printf "Invalid icon dimensions: %dx%d" width height
      return Nothing
  | BS.length byteString < expectedLength = do
      trayLogger WARNING $
        printf
          "Invalid icon payload length for dimensions %dx%d: got %d bytes, expected at least %d"
          width
          height
          (BS.length byteString)
          expectedLength
      return Nothing
  | otherwise = catchGErrorsAsNothing $ do
      trayLogger DEBUG "Getting Pixbuf from bytestring"
      bytes <- bytesNew $ Just byteString
      let bytesPerPixel = 4
          rowStride = width * bytesPerPixel
          sampleBits = 8
      pixbuf <- pixbufNewFromBytes bytes ColorspaceRgb True sampleBits width height rowStride
      -- The pixbuf returned by pixbufNewFromBytes is backed by immutable GBytes.
      -- Return a private copy so later overlay composition or caller transforms
      -- never write into SNI-owned byte storage.
      fromMaybe pixbuf <$> Gdk.pixbufCopy pixbuf
  where
    expectedLength =
      fromIntegral width * fromIntegral height * 4

data ItemContext = ItemContext
  { contextName :: DBusTypes.BusName,
    contextMenuPath :: Maybe DBusTypes.ObjectPath,
    contextIconWidget :: Gtk.Widget,
    contextSetIcon :: ItemInfo -> IO (),
    contextButton :: Gtk.EventBox
  }

data TrayImageSize = Expand | TrayImageSize Int32

data TrayClickAction = Activate | SecondaryActivate | PopupMenu deriving (Eq, Show)

data TrayClickContext = TrayClickContext
  { trayClickItemInfo :: ItemInfo,
    trayClickButton :: Word32,
    trayClickXRoot :: Int32,
    trayClickYRoot :: Int32,
    trayClickModifiers :: [Gdk.ModifierType],
    trayClickDefaultAction :: TrayClickAction
  }

data TrayClickDecision
  = UseDefaultClickAction
  | OverrideClickAction TrayClickAction
  | ConsumeClick
  deriving (Eq, Show)

type TrayClickHook = TrayClickContext -> IO TrayClickDecision

newtype TrayEventHooks = TrayEventHooks
  { trayClickHook :: Maybe TrayClickHook
  }

defaultTrayEventHooks :: TrayEventHooks
defaultTrayEventHooks = TrayEventHooks {trayClickHook = Nothing}

data MenuBackend = LibDBusMenu | HaskellDBusMenu deriving (Eq, Show)

data TrayItemMatcher = TrayItemMatcher
  { trayItemMatcherDescription :: String,
    trayItemMatcherPredicate :: ItemInfo -> Bool
  }

newtype TrayPriorityConfig = TrayPriorityConfig
  { trayPriorityMatchers :: [TrayItemMatcher]
  }

trayItemIdentityKey :: T.Text
trayItemIdentityKey = "status-notifier.tray.item-identity"

trayItemIdentity :: ItemInfo -> String
trayItemIdentity info =
  show (itemServiceName info) <> "|" <> show (itemServicePath info)

setTrayItemIdentity :: Gtk.EventBox -> ItemInfo -> IO ()
setTrayItemIdentity widget info = do
  sp <- newStablePtr (trayItemIdentity info)
  GObject.objectSetDataFull
    widget
    trayItemIdentityKey
    (castStablePtrToPtr sp :: Ptr ())
    (Just $ \p -> freeStablePtr (castPtrToStablePtr p :: StablePtr String))

getTrayItemIdentity :: Gtk.Widget -> IO (Maybe String)
getTrayItemIdentity widget = do
  p <- GObject.objectGetData widget trayItemIdentityKey
  if p == nullPtr
    then pure Nothing
    else Just <$> deRefStablePtr (castPtrToStablePtr p :: StablePtr String)

reorderTrayChildrenByIdentities :: Gtk.Box -> [String] -> IO ()
reorderTrayChildrenByIdentities trayBox orderedIdentities = do
  currentChildren <- Gtk.containerGetChildren trayBox
  let identityOrder = Map.fromList (zip orderedIdentities [0 :: Int ..])
      fallbackOrder = length orderedIdentities
  childRows <- forM (zip [0 :: Int ..] currentChildren) $ \(currentIndex, child) -> do
    identity <- getTrayItemIdentity child
    let desiredIndex =
          fromMaybe
            (fallbackOrder + currentIndex)
            (identity >>= (`Map.lookup` identityOrder))
    pure (desiredIndex, currentIndex, child)
  let sortedChildren =
        sortOn (\(desiredIndex, currentIndex, _) -> (desiredIndex, currentIndex)) childRows
      currentOrder = map (\(_, _, child) -> child) childRows
      desiredOrder = map (\(_, _, child) -> child) sortedChildren
  unless (currentOrder == desiredOrder) $
    forM_ (zip [0 :: Int ..] sortedChildren) $
      \(newIndex, (_, _, child)) ->
        Gtk.boxReorderChild trayBox child (fromIntegral newIndex)

defaultTrayPriorityConfig :: TrayPriorityConfig
defaultTrayPriorityConfig = TrayPriorityConfig {trayPriorityMatchers = []}

mkTrayItemMatcher :: String -> (ItemInfo -> Bool) -> TrayItemMatcher
mkTrayItemMatcher = TrayItemMatcher

trayMatchAny :: [TrayItemMatcher] -> TrayItemMatcher
trayMatchAny matchers = mkTrayItemMatcher "any" $ \info ->
  any (`trayItemMatcherPredicate` info) matchers

trayMatchAll :: [TrayItemMatcher] -> TrayItemMatcher
trayMatchAll matchers = mkTrayItemMatcher "all" $ \info ->
  all (`trayItemMatcherPredicate` info) matchers

trayMatchNot :: TrayItemMatcher -> TrayItemMatcher
trayMatchNot matcher =
  mkTrayItemMatcher ("not(" <> trayItemMatcherDescription matcher <> ")") $
    not . trayItemMatcherPredicate matcher

normalizeText :: T.Text -> T.Text
normalizeText = T.toCaseFold

containsCI :: T.Text -> T.Text -> Bool
containsCI needle haystack =
  normalizeText needle `T.isInfixOf` normalizeText haystack

equalsCI :: T.Text -> T.Text -> Bool
equalsCI left right = normalizeText left == normalizeText right

matchOnTextFields ::
  String ->
  (T.Text -> T.Text -> Bool) ->
  [ItemInfo -> Maybe T.Text] ->
  T.Text ->
  TrayItemMatcher
matchOnTextFields matcherName comparator fieldGetters target =
  mkTrayItemMatcher matcherName $ \info ->
    any
      (\fieldGetter -> maybe False (comparator target) (fieldGetter info))
      fieldGetters

serviceNameText :: ItemInfo -> T.Text
serviceNameText = T.pack . (coerce :: DBusTypes.BusName -> String) . itemServiceName

servicePathText :: ItemInfo -> T.Text
servicePathText = T.pack . (coerce :: DBusTypes.ObjectPath -> String) . itemServicePath

menuPathText :: ItemInfo -> Maybe T.Text
menuPathText = fmap (T.pack . (coerce :: DBusTypes.ObjectPath -> String)) . menuPath

itemIdText :: ItemInfo -> Maybe T.Text
itemIdText = fmap T.pack . itemId

itemCategoryText :: ItemInfo -> Maybe T.Text
itemCategoryText = fmap T.pack . itemCategory

itemStatusText :: ItemInfo -> Maybe T.Text
itemStatusText = fmap T.pack . itemStatus

iconNameText :: ItemInfo -> T.Text
iconNameText = T.pack . iconName

iconTitleText :: ItemInfo -> T.Text
iconTitleText = T.pack . iconTitle

tooltipTitleText :: ItemInfo -> Maybe T.Text
tooltipTitleText info = (\(_, _, titleText, _) -> T.pack titleText) <$> itemToolTip info

tooltipBodyText :: ItemInfo -> Maybe T.Text
tooltipBodyText info = (\(_, _, _, bodyText) -> T.pack bodyText) <$> itemToolTip info

trayMatchServiceNameContains :: T.Text -> TrayItemMatcher
trayMatchServiceNameContains =
  matchOnTextFields "service-name-contains" containsCI [Just . serviceNameText]

trayMatchServiceNameEquals :: T.Text -> TrayItemMatcher
trayMatchServiceNameEquals =
  matchOnTextFields "service-name-equals" equalsCI [Just . serviceNameText]

trayMatchServicePathContains :: T.Text -> TrayItemMatcher
trayMatchServicePathContains =
  matchOnTextFields "service-path-contains" containsCI [Just . servicePathText]

trayMatchServicePathEquals :: T.Text -> TrayItemMatcher
trayMatchServicePathEquals =
  matchOnTextFields "service-path-equals" equalsCI [Just . servicePathText]

trayMatchMenuPathContains :: T.Text -> TrayItemMatcher
trayMatchMenuPathContains =
  matchOnTextFields "menu-path-contains" containsCI [menuPathText]

trayMatchMenuPathEquals :: T.Text -> TrayItemMatcher
trayMatchMenuPathEquals =
  matchOnTextFields "menu-path-equals" equalsCI [menuPathText]

trayMatchItemIdContains :: T.Text -> TrayItemMatcher
trayMatchItemIdContains =
  matchOnTextFields "item-id-contains" containsCI [itemIdText]

trayMatchItemIdEquals :: T.Text -> TrayItemMatcher
trayMatchItemIdEquals =
  matchOnTextFields "item-id-equals" equalsCI [itemIdText]

trayMatchItemCategoryContains :: T.Text -> TrayItemMatcher
trayMatchItemCategoryContains =
  matchOnTextFields "item-category-contains" containsCI [itemCategoryText]

trayMatchItemCategoryEquals :: T.Text -> TrayItemMatcher
trayMatchItemCategoryEquals =
  matchOnTextFields "item-category-equals" equalsCI [itemCategoryText]

trayMatchStatusContains :: T.Text -> TrayItemMatcher
trayMatchStatusContains =
  matchOnTextFields "item-status-contains" containsCI [itemStatusText]

trayMatchStatusEquals :: T.Text -> TrayItemMatcher
trayMatchStatusEquals =
  matchOnTextFields "item-status-equals" equalsCI [itemStatusText]

trayMatchIconNameContains :: T.Text -> TrayItemMatcher
trayMatchIconNameContains =
  matchOnTextFields "icon-name-contains" containsCI [Just . iconNameText]

trayMatchIconNameEquals :: T.Text -> TrayItemMatcher
trayMatchIconNameEquals =
  matchOnTextFields "icon-name-equals" equalsCI [Just . iconNameText]

trayMatchIconTitleContains :: T.Text -> TrayItemMatcher
trayMatchIconTitleContains =
  matchOnTextFields "icon-title-contains" containsCI [Just . iconTitleText]

trayMatchIconTitleEquals :: T.Text -> TrayItemMatcher
trayMatchIconTitleEquals =
  matchOnTextFields "icon-title-equals" equalsCI [Just . iconTitleText]

trayMatchTooltipContains :: T.Text -> TrayItemMatcher
trayMatchTooltipContains =
  matchOnTextFields "tooltip-contains" containsCI [tooltipTitleText, tooltipBodyText]

trayMatchTooltipEquals :: T.Text -> TrayItemMatcher
trayMatchTooltipEquals =
  matchOnTextFields "tooltip-equals" equalsCI [tooltipTitleText, tooltipBodyText]

trayMatchAnyTextContains :: T.Text -> TrayItemMatcher
trayMatchAnyTextContains =
  matchOnTextFields
    "any-text-contains"
    containsCI
    [ Just . serviceNameText,
      Just . servicePathText,
      menuPathText,
      itemIdText,
      itemCategoryText,
      itemStatusText,
      Just . iconNameText,
      Just . iconTitleText,
      tooltipTitleText,
      tooltipBodyText
    ]

trayMatchIsMenu :: Bool -> TrayItemMatcher
trayMatchIsMenu expected =
  mkTrayItemMatcher "is-menu" $ \info -> itemIsMenu info == expected

-- | Controls whether to prefer application-provided pixmaps or themed icons
-- when both are present. Some items provide both.
data TrayIconPreference
  = PreferPixmaps
  | PreferThemedIcons
  deriving (Eq, Show, Read)

data TrayParams = TrayParams
  { trayOrientation :: Gtk.Orientation,
    trayImageSize :: TrayImageSize,
    trayIconExpand :: Bool,
    trayIconPreference :: TrayIconPreference,
    trayAlignment :: StrutAlignment,
    trayOverlayScale :: Rational,
    trayLeftClickAction :: TrayClickAction,
    trayMiddleClickAction :: TrayClickAction,
    trayRightClickAction :: TrayClickAction,
    trayMenuBackend :: MenuBackend,
    trayCenterIcons :: Bool,
    -- | Whether newly-added item widgets are shown immediately.
    -- Collapsible wrappers can disable this and decide visibility in their
    -- own refresh pass to avoid transient flashes of hidden items.
    trayShowNewIconsImmediately :: Bool,
    trayPriorityConfig :: TrayPriorityConfig,
    trayPixbufTransform :: Maybe PixbufTransform,
    trayEventHooks :: TrayEventHooks
  }

defaultTrayParams :: TrayParams
defaultTrayParams =
  TrayParams
    { trayOrientation = Gtk.OrientationHorizontal,
      trayImageSize = Expand,
      trayIconExpand = False,
      trayIconPreference = PreferPixmaps,
      trayAlignment = End,
      trayOverlayScale = 2 % 5,
      trayLeftClickAction = Activate,
      trayMiddleClickAction = SecondaryActivate,
      trayRightClickAction = PopupMenu,
      trayMenuBackend = HaskellDBusMenu,
      trayCenterIcons = False,
      trayShowNewIconsImmediately = True,
      trayPriorityConfig = defaultTrayPriorityConfig,
      trayPixbufTransform = Nothing,
      trayEventHooks = defaultTrayEventHooks
    }

buildTray :: Host -> Client -> TrayParams -> IO Gtk.Box
buildTray
  Host
    { itemInfoMap = getInfoMap,
      addUpdateHandler = addUHandler,
      removeUpdateHandler = removeUHandler
    }
  client
  TrayParams
    { trayOrientation = orientation,
      trayImageSize = imageSize,
      trayIconExpand = shouldExpand,
      trayIconPreference = iconPreference,
      trayAlignment = alignment,
      trayOverlayScale = overlayScale,
      trayLeftClickAction = leftClickAction,
      trayMiddleClickAction = middleClickAction,
      trayRightClickAction = rightClickAction,
      trayMenuBackend = menuBackend,
      trayCenterIcons = centerIcons,
      trayShowNewIconsImmediately = showNewIconsImmediately,
      trayPriorityConfig =
        TrayPriorityConfig
          { trayPriorityMatchers = priorityMatchers
          },
      trayPixbufTransform = mTransform,
      trayEventHooks =
        TrayEventHooks
          { trayClickHook = mClickHook
          }
    } =
    do
      trayInstance <- hashUnique <$> newUnique
      trayLogger INFO $ printf "Building tray instance=%d" trayInstance

      trayBox <- Gtk.boxNew orientation 0
      when centerIcons $ case orientation of
        Gtk.OrientationHorizontal -> Gtk.widgetSetHalign trayBox Gtk.AlignCenter
        _ -> Gtk.widgetSetValign trayBox Gtk.AlignCenter
      Gtk.widgetGetStyleContext trayBox
        >>= flip Gtk.styleContextAddClass "tray-box"
      contextMap <- MV.newMVar ContextMap.empty

      let getContext name = ContextMap.lookupReadyContext name <$> MV.readMVar contextMap
          showInfo info = show info {iconPixmaps = []}

          getInfoAttr fn def name = maybe def fn . Map.lookup name <$> getInfoMap

          getInfo :: ItemInfo -> DBusTypes.BusName -> IO ItemInfo
          getInfo = getInfoAttr id

          getPriorityIndex info =
            fromMaybe
              (length priorityMatchers)
              (findIndex (`trayItemMatcherPredicate` info) priorityMatchers)

          reorderTrayByPriority = unless (null priorityMatchers) $ do
            infoMap <- getInfoMap
            let orderedInfos =
                  sortOn
                    (\info -> (getPriorityIndex info, trayItemIdentity info))
                    (Map.elems infoMap)
            reorderTrayChildrenByIdentities
              trayBox
              (map trayItemIdentity orderedInfos)

          applyTransform :: Gtk.Widget -> Maybe Pixbuf -> IO (Maybe Pixbuf)
          applyTransform _ Nothing = return Nothing
          applyTransform widget (Just pb) =
            case mTransform of
              Nothing -> return (Just pb)
              Just f -> Just <$> f widget pb

          updateIconFromInfo info@ItemInfo {itemServiceName = name} =
            getContext name >>= updateIcon
            where
              updateIcon Nothing = updateHandler ItemAdded info
              updateIcon (Just ItemContext {contextSetIcon = setIcon}) = setIcon info

          getTooltipText ItemInfo {itemToolTip = Just (_, _, titleText, fullText)}
            | titleText == fullText = fullText
            | titleText == "" = fullText
            | fullText == "" = titleText
            | otherwise = printf "%s: %s" titleText fullText
          getTooltipText _ = ""

          setTooltipText widget info =
            Gtk.widgetSetTooltipText widget $ Just $ T.pack $ getTooltipText info

          updateHandler
            ItemAdded
            info@ItemInfo
              { menuPath = pathForMenu,
                itemServiceName = serviceName,
                itemServicePath = servicePath
              } =
              do
                reservation <- MV.modifyMVar contextMap $ \contexts ->
                  let (reserved, newContexts) =
                        ContextMap.reserveContext serviceName contexts
                   in pure (newContexts, reserved)
                forM_ reservation $
                  \reservedContext ->
                    flip
                      onException
                      ( MV.modifyMVar_ contextMap $
                          pure . ContextMap.cancelReservation serviceName reservedContext
                      )
                      $ do
                        let serviceNameStr = (coerce serviceName :: String)
                            servicePathStr = coerce servicePath :: String
                            logText =
                              printf
                                "Adding widget for %s - %s"
                                serviceNameStr
                                servicePathStr

                        trayLogger INFO logText

                        eventBox <- Gtk.eventBoxNew
                        Gtk.widgetAddEvents eventBox [Gdk.EventMaskScrollMask]
                        Gtk.widgetGetStyleContext eventBox
                          >>= flip Gtk.styleContextAddClass "tray-icon-button"

                        infoRef <- newIORef info
                        (iconWidget, setIcon) <- case imageSize of
                          Expand -> do
                            drawArea <- Gtk.drawingAreaNew
                            Gtk.widgetGetStyleContext drawArea
                              >>= flip Gtk.styleContextAddClass "tray-icon-image"
                            iconWidget <- Gtk.toWidget drawArea
                            let getPixbufForSize size = do
                                  currentInfo <- readIORef infoRef
                                  pixBuf0 <- getScaledPixBufFromInfo size currentInfo
                                  pixBuf <- applyTransform iconWidget pixBuf0
                                  when (isNothing pixBuf) $
                                    trayLogger WARNING $
                                      printf "Got null pixbuf for info %s" (showInfo currentInfo)
                                  return pixBuf
                            refresh <- autoFillImage drawArea getPixbufForSize orientation
                            let setIconFromInfo iconInfo = do
                                  writeIORef infoRef iconInfo
                                  refresh
                            setIconFromInfo info
                            return (iconWidget, setIconFromInfo)
                          TrayImageSize size -> do
                            image <- Gtk.imageNew
                            Gtk.widgetGetStyleContext image
                              >>= flip Gtk.styleContextAddClass "tray-icon-image"
                            iconWidget <- Gtk.toWidget image
                            let setIconFromInfo iconInfo = do
                                  writeIORef infoRef iconInfo
                                  pixBuf0 <- getScaledPixBufFromInfo size iconInfo
                                  pixBuf <- applyTransform iconWidget pixBuf0
                                  when (isNothing pixBuf) $
                                    trayLogger WARNING $
                                      printf "Got null pixbuf for info %s" $
                                        showInfo iconInfo
                                  Gtk.imageSetFromPixbuf image pixBuf
                            setIconFromInfo info
                            return (iconWidget, setIconFromInfo)

                        Gtk.containerAdd eventBox iconWidget
                        setTrayItemIdentity eventBox info
                        setTooltipText eventBox info

                        let context =
                              ItemContext
                                { contextName = serviceName,
                                  contextMenuPath = pathForMenu,
                                  contextIconWidget = iconWidget,
                                  contextSetIcon = setIcon,
                                  contextButton = eventBox
                                }

                            popupGtkMenu gtkMenu mEvent = do
                              Gtk.menuAttachToWidget gtkMenu eventBox Nothing
                              _ <- Gtk.onWidgetHide gtkMenu $
                                void $
                                  GLib.idleAdd GLib.PRIORITY_LOW $ do
                                    Gtk.widgetDestroy gtkMenu
                                    return False
                              Gtk.widgetShowAll gtkMenu
                              Gtk.menuPopupAtPointer gtkMenu mEvent

                        _ <- Gtk.onWidgetButtonPressEvent eventBox $ \event -> do
                          -- Capture the current event as a Gdk.Event before any
                          -- blocking calls (DBus etc.) so menuPopupAtPointer can
                          -- use its coordinates for popup positioning.
                          currentEvent <- Gtk.getCurrentEvent
                          currentInfo <- getInfo info serviceName
                          mouseButton <- Gdk.getEventButtonButton event
                          x <- round <$> Gdk.getEventButtonXRoot event
                          y <- round <$> Gdk.getEventButtonYRoot event
                          modifiers <- Gdk.getEventButtonState event
                          let defaultAction = case mouseButton of
                                1 -> if itemIsMenu currentInfo then PopupMenu else leftClickAction
                                2 -> middleClickAction
                                _ -> rightClickAction
                          clickDecision <-
                            maybe
                              (pure UseDefaultClickAction)
                              ( \hook ->
                                  hook
                                    TrayClickContext
                                      { trayClickItemInfo = currentInfo,
                                        trayClickButton = mouseButton,
                                        trayClickXRoot = x,
                                        trayClickYRoot = y,
                                        trayClickModifiers = modifiers,
                                        trayClickDefaultAction = defaultAction
                                      }
                              )
                              mClickHook
                          let mAction = case clickDecision of
                                UseDefaultClickAction -> Just defaultAction
                                OverrideClickAction action -> Just action
                                ConsumeClick -> Nothing
                          let logActionError actionName e =
                                trayLogger WARNING $
                                  printf
                                    "%s failed for %s: %s"
                                    (actionName :: String)
                                    (coerce serviceName :: String)
                                    (show e)
                              runAsync actionName action =
                                void $
                                  forkIO $
                                    catchAny action (logActionError actionName)
                              buildAndPopupHaskellMenu p =
                                runAsync "PopupMenu" $ do
                                  _ <- DBusMenu.aboutToShow client serviceName p 0
                                  (_, layout) <-
                                    DBusMenu.getLayout
                                      client
                                      serviceName
                                      p
                                      0
                                      (-1)
                                      dbusMenuLayoutPropNames
                                  void $
                                    GLib.idleAdd GLib.PRIORITY_DEFAULT_IDLE $ do
                                      gtkMenu <- Gtk.menuNew
                                      DBusMenu.populateGtkMenu client serviceName p gtkMenu layout
                                      popupGtkMenu gtkMenu currentEvent
                                      return False
                          traverse_
                            ( \case
                                Activate ->
                                  runAsync "Activate" $
                                    void $
                                      IC.activate client serviceName servicePath x y
                                SecondaryActivate ->
                                  runAsync "SecondaryActivate" $
                                    void $
                                      IC.secondaryActivate
                                        client
                                        serviceName
                                        servicePath
                                        x
                                        y
                                PopupMenu -> do
                                  let menuPath' = menuPath currentInfo
                                  traverse_
                                    ( \p ->
                                        case menuBackend of
                                          LibDBusMenu ->
                                            catchAny
                                              ( do
                                                  let sn = T.pack (coerce serviceName :: String)
                                                      mp = T.pack (coerce p :: String)
                                                  gtkMenu <- DM.menuNew sn mp >>= unsafeCastTo Gtk.Menu
                                                  Gtk.menuAttachToWidget gtkMenu eventBox Nothing
                                                  _ <- Gtk.onWidgetHide gtkMenu $
                                                    void $
                                                      GLib.idleAdd GLib.PRIORITY_DEFAULT_IDLE $ do
                                                        Gtk.widgetDestroy gtkMenu
                                                        return False
                                                  -- libdbusmenu-gtk fetches the menu layout
                                                  -- asynchronously; showing before the root menuitem
                                                  -- is available triggers assertion failures. Defer
                                                  -- the popup until the menu is populated.
                                                  attemptsRef <- newIORef (0 :: Int)
                                                  _ <- GLib.timeoutAdd GLib.PRIORITY_DEFAULT 50 $ do
                                                    n <- readIORef attemptsRef
                                                    if n >= 100
                                                      then do
                                                        Gtk.widgetDestroy gtkMenu
                                                        return False
                                                      else do
                                                        writeIORef attemptsRef (n + 1)
                                                        children <- Gtk.containerGetChildren gtkMenu
                                                        if null children
                                                          then return True
                                                          else do
                                                            Gtk.widgetShowAll gtkMenu
                                                            -- libdbusmenu is populated asynchronously, so we popup later via a
                                                            -- timeout. On Wayland, popups generally need the original trigger
                                                            -- event; use menuPopupAtWidget anchored to the EventBox to avoid
                                                            -- "no trigger event" and invalid rect_window assertions.
                                                            -- Anchor to the actual icon widget so the popup aligns with the
                                                            -- visible image, not the full EventBox allocation.
                                                            Gtk.menuPopupAtWidget
                                                              gtkMenu
                                                              iconWidget
                                                              GravitySouth
                                                              GravityNorth
                                                              currentEvent
                                                            return False
                                                  return ()
                                              )
                                              (logActionError "PopupMenu")
                                          HaskellDBusMenu ->
                                            buildAndPopupHaskellMenu p
                                    )
                                    menuPath'
                            )
                            mAction
                          return False
                        _ <- Gtk.onWidgetScrollEvent eventBox $ \event -> do
                          direction <- getEventScrollDirection event
                          let direction' = case direction of
                                ScrollDirectionUp -> Just "vertical"
                                ScrollDirectionDown -> Just "vertical"
                                ScrollDirectionLeft -> Just "horizontal"
                                ScrollDirectionRight -> Just "horizontal"
                                _ -> Nothing
                              delta = case direction of
                                ScrollDirectionUp -> -1
                                ScrollDirectionDown -> 1
                                ScrollDirectionLeft -> -1
                                ScrollDirectionRight -> 1
                                _ -> 0
                          traverse_
                            ( \d ->
                                catchAny
                                  (void $ IC.scroll client serviceName servicePath delta d)
                                  ( trayLogger WARNING
                                      . printf "Scroll failed for %s: %s" (coerce serviceName :: String)
                                      . show
                                  )
                            )
                            direction'
                          return False

                        didFinalize <- MV.modifyMVar contextMap $ \contexts ->
                          let (finalized, newContexts) =
                                ContextMap.setReadyContext serviceName reservedContext context contexts
                           in pure (newContexts, finalized)

                        if didFinalize
                          then do
                            let packFn =
                                  case alignment of
                                    End -> Gtk.boxPackEnd
                                    _ -> Gtk.boxPackStart

                            packFn trayBox eventBox shouldExpand True 0
                            Gtk.widgetShow iconWidget
                            when showNewIconsImmediately $
                              Gtk.widgetShow eventBox
                          else do
                            trayLogger DEBUG $
                              printf
                                "Dropping stale tray widget for %s - %s because its reservation was invalidated."
                                serviceNameStr
                                servicePathStr
                            Gtk.widgetDestroy eventBox
          updateHandler ItemRemoved ItemInfo {itemServiceName = name} =
            MV.modifyMVar contextMap removeContext >>= removeWidget
            where
              removeContext contexts =
                let readyContext = ContextMap.lookupReadyContext name contexts
                 in pure (ContextMap.deleteContext name contexts, readyContext)
              removeWidget Nothing =
                trayLogger DEBUG $
                  printf
                    "Removed tray context for %s before widget realization."
                    (coerce name :: String)
              removeWidget (Just ItemContext {contextButton = widgetToRemove}) =
                Gtk.containerRemove trayBox widgetToRemove
          updateHandler IconUpdated i = updateIconFromInfo i
          updateHandler OverlayIconUpdated i = updateIconFromInfo i
          updateHandler ToolTipUpdated info@ItemInfo {itemServiceName = name} =
            void $
              getContext name
                >>= traverse (flip setTooltipText info . contextButton)
          updateHandler _ _ = return ()

          maybeAddOverlayToPixbuf size info pixbuf = do
            destination <- fromMaybe pixbuf <$> Gdk.pixbufCopy pixbuf
            _ <- runMaybeT $ do
              let overlayHeight = floor (fromIntegral size * overlayScale)
              overlayPixbuf <-
                MaybeT $
                  getOverlayPixBufFromInfo overlayHeight info
                    >>= traverse (scalePixbufToSize overlayHeight Gtk.OrientationHorizontal)
              lift $ do
                actualOHeight <- getPixbufHeight overlayPixbuf
                actualOWidth <- getPixbufWidth overlayPixbuf
                _mainHeight <- getPixbufHeight destination
                _mainWidth <- getPixbufWidth destination
                pixbufComposite
                  overlayPixbuf
                  destination
                  0
                  0
                  actualOWidth
                  actualOHeight
                  0
                  0
                  1.0
                  1.0
                  InterpTypeBilinear
                  255
            return destination

          getScaledPixBufFromInfo size info =
            getPixBufFromInfo size info
              >>= traverse
                ( scalePixbufToSize size orientation
                    >=> maybeAddOverlayToPixbuf size info
                )

          getPixBufFromInfo
            size
            ItemInfo
              { iconName = name,
                iconThemePath = mpath,
                iconPixmaps = pixmaps
              } = getPixBufFrom size name mpath pixmaps

          getOverlayPixBufFromInfo
            size
            ItemInfo
              { overlayIconName = name,
                iconThemePath = mpath,
                overlayIconPixmaps = pixmaps
              } =
              getPixBufFrom
                size
                (fromMaybe "" name)
                mpath
                pixmaps

          getPixBufFrom size name mpath pixmaps = do
            let tooSmall (w, h, _) = w < size || h < size
                largeEnough = filter (not . tooSmall) pixmaps
                orderer (w1, h1, _) (w2, h2, _) =
                  case comparing id w1 w2 of
                    EQ -> comparing id h1 h2
                    a -> a
                selectedPixmap =
                  if null largeEnough
                    then maximumBy orderer pixmaps
                    else minimumBy orderer largeEnough
                getFromPixmaps (w, h, p) =
                  if BS.length p == 0
                    then return Nothing
                    else getIconPixbufFromByteString w h p
                getFromThemed =
                  if name == ""
                    then return Nothing
                    else getIconPixbufByName size (T.pack name) mpath
                firstJustM a b = do
                  ma <- a
                  case ma of
                    Just _ -> return ma
                    Nothing -> b

            if null pixmaps
              then getIconPixbufByName size (T.pack name) mpath
              else case iconPreference of
                PreferThemedIcons ->
                  firstJustM getFromThemed (getFromPixmaps selectedPixmap)
                PreferPixmaps ->
                  firstJustM (getFromPixmaps selectedPixmap) getFromThemed

          uiUpdateHandler updateType info =
            void $
              Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT $
                catchAny
                  ( trayLogger
                      DEBUG
                      ( printf
                          "Tray instance=%d handling update=%s service=%s"
                          trayInstance
                          (show updateType)
                          (coerce (itemServiceName info) :: String)
                      )
                      >> updateHandler updateType info
                      >> reorderTrayByPriority
                      >> return False
                  )
                  ( \e -> do
                      trayLogger WARNING $ printf "Update handler failed: %s" (show e)
                      return False
                  )

      handlerId <- addUHandler uiUpdateHandler
      trayLogger INFO $
        printf
          "Registered tray update handler tray=%d handler=%d"
          trayInstance
          (hashUnique handlerId)
      _ <- Gtk.onWidgetDestroy trayBox $ do
        trayLogger INFO $
          printf
            "Removing tray update handler tray=%d handler=%d"
            trayInstance
            (hashUnique handlerId)
        removeUHandler handlerId
      return trayBox
