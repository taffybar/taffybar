{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module StatusNotifier.Tray where

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
import Data.Word
import Foreign.Ptr (Ptr)
import qualified GI.DbusmenuGtk3.Objects.Menu as DM
import qualified GI.GLib as GLib
import GI.GLib.Structs.Bytes
import qualified GI.Gdk as Gdk
import GI.Gdk.Enums
import GI.Gdk.Structs.EventScroll
import GI.GdkPixbuf.Enums
import GI.GdkPixbuf.Objects.Pixbuf as Gdk
import qualified GI.Gtk as Gtk
import GI.Gtk.Flags
import GI.Gtk.Objects.IconTheme
import Graphics.UI.GIGtkStrut
import StatusNotifier.Host.Service
import qualified StatusNotifier.Item.Client as IC
import System.Directory
import System.FilePath
import System.Log.Logger
import Text.Printf

trayLogger :: Priority -> String -> IO ()
trayLogger = logM "StatusNotifier.Tray"

-- | Optional post-processing hook for item icons. This is applied after scaling
-- and overlay composition.
type PixbufTransform = Gtk.Image -> Pixbuf -> IO Pixbuf

logItemInfo :: ItemInfo -> String -> IO ()
logItemInfo info message =
  trayLogger INFO $
    printf
      "%s - %s pixmap count: %s"
      message
      (show $ info {iconPixmaps = []})
      (show $ length $ iconPixmaps info)

getScaledWidthHeight :: Bool -> Int32 -> Int32 -> Int32 -> (Int32, Int32)
getScaledWidthHeight shouldTargetWidth targetSize width height =
  let getRatio :: Int32 -> Rational
      getRatio toScale =
        fromIntegral targetSize / fromIntegral toScale
      getOther :: Int32 -> Int32 -> Int32
      getOther toScale other = max 1 $ floor $ getRatio toScale * fromIntegral other
   in if shouldTargetWidth
        then (targetSize, getOther width height)
        else (getOther height width, targetSize)

scalePixbufToSize :: Int32 -> Gtk.Orientation -> Pixbuf -> IO Pixbuf
scalePixbufToSize size orientation pixbuf = do
  width <- pixbufGetWidth pixbuf
  height <- pixbufGetHeight pixbuf
  let warnAndReturnOrig =
        trayLogger WARNING "Unable to scale pixbuf" >> return pixbuf
  if width <= 0 || height <= 0
    then warnAndReturnOrig
    else do
      let targetWidth = case orientation of
            Gtk.OrientationHorizontal -> False
            _ -> True
          (scaledWidth, scaledHeight) =
            getScaledWidthHeight targetWidth size width height
      trayLogger DEBUG $
        printf
          "Scaling pb to %s, actualW: %s, actualH: %s, scaledW: %s, scaledH: %s"
          (show size)
          (show width)
          (show height)
          (show scaledWidth)
          (show scaledHeight)

      trayLogger DEBUG $
        printf
          "targetW: %s, targetH: %s"
          (show scaledWidth)
          (show scaledHeight)
      maybe warnAndReturnOrig return
        =<< pixbufScaleSimple pixbuf scaledWidth scaledHeight InterpTypeBilinear

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
      tryLoad iconName =
        catchAny
          (iconThemeLoadIcon themeForIcon iconName size themeLoadFlags)
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
          else fmap join $ sequenceA $ getIconPathFromThemePath nameString <$> themePath
      fmap join $ sequenceA $ safePixbufNewFromFile <$> maybeFile

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
  | otherwise = catchGErrorsAsNothing $ do
      trayLogger DEBUG "Getting Pixbuf from bytestring"
      bytes <- bytesNew $ Just byteString
      let bytesPerPixel = 4
          rowStride = width * bytesPerPixel
          sampleBits = 8
      pixbufNewFromBytes bytes ColorspaceRgb True sampleBits width height rowStride

data ItemContext = ItemContext
  { contextName :: DBusTypes.BusName,
    contextMenuPath :: Maybe DBusTypes.ObjectPath,
    contextImage :: Gtk.Image,
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

data TrayEventHooks = TrayEventHooks
  { trayClickHook :: Maybe TrayClickHook
  }

defaultTrayEventHooks :: TrayEventHooks
defaultTrayEventHooks = TrayEventHooks {trayClickHook = Nothing}

data MenuBackend = LibDBusMenu | HaskellDBusMenu deriving (Eq, Show)

data TrayItemMatcher = TrayItemMatcher
  { trayItemMatcherDescription :: String,
    trayItemMatcherPredicate :: ItemInfo -> Bool
  }

data TrayPriorityConfig = TrayPriorityConfig
  { trayPriorityMatchers :: [TrayItemMatcher]
  }

defaultTrayPriorityConfig :: TrayPriorityConfig
defaultTrayPriorityConfig = TrayPriorityConfig {trayPriorityMatchers = []}

mkTrayItemMatcher :: String -> (ItemInfo -> Bool) -> TrayItemMatcher
mkTrayItemMatcher = TrayItemMatcher

trayMatchAny :: [TrayItemMatcher] -> TrayItemMatcher
trayMatchAny matchers = mkTrayItemMatcher "any" $ \info ->
  any (\matcher -> trayItemMatcherPredicate matcher info) matchers

trayMatchAll :: [TrayItemMatcher] -> TrayItemMatcher
trayMatchAll matchers = mkTrayItemMatcher "all" $ \info ->
  all (\matcher -> trayItemMatcherPredicate matcher info) matchers

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
      trayLogger INFO "Building tray"

      trayBox <- Gtk.boxNew orientation 0
      when centerIcons $ case orientation of
        Gtk.OrientationHorizontal -> Gtk.widgetSetHalign trayBox Gtk.AlignCenter
        _ -> Gtk.widgetSetValign trayBox Gtk.AlignCenter
      Gtk.widgetGetStyleContext trayBox
        >>= flip Gtk.styleContextAddClass "tray-box"
      contextMap <- MV.newMVar Map.empty

      let getContext name = Map.lookup name <$> MV.readMVar contextMap
          showInfo info = show info {iconPixmaps = []}

          getSize rectangle =
            case orientation of
              Gtk.OrientationHorizontal ->
                Gdk.getRectangleHeight rectangle
              _ ->
                Gdk.getRectangleWidth rectangle

          getInfoAttr fn def name = maybe def fn . Map.lookup name <$> getInfoMap

          getInfo :: ItemInfo -> DBusTypes.BusName -> IO ItemInfo
          getInfo = getInfoAttr id

          getPriorityIndex info =
            fromMaybe
              (length priorityMatchers)
              (findIndex (\matcher -> trayItemMatcherPredicate matcher info) priorityMatchers)

          reorderTrayByPriority = when (not (null priorityMatchers)) $ do
            currentChildren <- Gtk.containerGetChildren trayBox
            contexts <- MV.readMVar contextMap
            contextWidgets <- forM (Map.toList contexts) $
              \(busName, ItemContext {contextButton = button}) -> do
                widget <- Gtk.toWidget button
                return (busName, widget)
            infoMap <- getInfoMap
            let childRows =
                  [ let busName = fst <$> find (\(_, widget) -> widget == child) contextWidgets
                        itemInfo = busName >>= (`Map.lookup` infoMap)
                        priority = maybe (length priorityMatchers) getPriorityIndex itemInfo
                     in (priority, currentIndex, child)
                  | (currentIndex, child) <- zip [0 :: Int ..] currentChildren
                  ]
                sortedChildren =
                  [ child
                  | (_, _, child) <-
                      sortOn (\(priority, currentIndex, _) -> (priority, currentIndex)) childRows
                  ]
            forM_ (zip [0 :: Int ..] sortedChildren) $
              \(newIndex, child) ->
                Gtk.boxReorderChild trayBox child (fromIntegral newIndex)

          applyTransform :: Gtk.Image -> Maybe Pixbuf -> IO (Maybe Pixbuf)
          applyTransform _ Nothing = return Nothing
          applyTransform image (Just pb) =
            case mTransform of
              Nothing -> return (Just pb)
              Just f -> Just <$> f image pb

          updateIconFromInfo info@ItemInfo {itemServiceName = name} =
            getContext name >>= updateIcon
            where
              updateIcon Nothing = updateHandler ItemAdded info
              updateIcon (Just ItemContext {contextImage = image}) = do
                size <- case imageSize of
                  TrayImageSize size -> return size
                  Expand -> Gtk.widgetGetAllocation image >>= getSize
                getScaledPixBufFromInfo size info
                  >>= applyTransform image
                  >>= let handlePixbuf mpbuf =
                            if isJust mpbuf
                              then Gtk.imageSetFromPixbuf image mpbuf
                              else
                                trayLogger WARNING $
                                  printf "Failed to get pixbuf for %s" $
                                    showInfo info
                       in handlePixbuf

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

                image <- Gtk.imageNew

                case imageSize of
                  Expand -> do
                    lastAllocation <- MV.newMVar Nothing

                    let setPixbuf allocation =
                          do
                            size <- getSize allocation

                            actualWidth <- Gdk.getRectangleWidth allocation
                            actualHeight <- Gdk.getRectangleHeight allocation

                            requestResize <- MV.modifyMVar lastAllocation $ \previous ->
                              let thisTime = Just (size, actualWidth, actualHeight)
                               in return (thisTime, thisTime /= previous)

                            trayLogger DEBUG $
                              printf
                                ( "Allocating image size %s, width %s,"
                                    <> " height %s, resize %s"
                                )
                                (show size)
                                (show actualWidth)
                                (show actualHeight)
                                (show requestResize)

                            when requestResize $ do
                              trayLogger DEBUG "Requesting resize"
                              pixBuf0 <-
                                getInfo info serviceName
                                  >>= getScaledPixBufFromInfo size
                              pixBuf <- applyTransform image pixBuf0
                              when (isNothing pixBuf) $
                                trayLogger WARNING $
                                  printf "Got null pixbuf for info %s" $
                                    showInfo info
                              Gtk.imageSetFromPixbuf image pixBuf
                              void $
                                traverse
                                  ( \pb -> do
                                      width <- pixbufGetWidth pb
                                      height <- pixbufGetHeight pb
                                      Gtk.widgetSetSizeRequest image width height
                                  )
                                  pixBuf
                              void
                                ( Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT $
                                    Gtk.widgetQueueResize image >> return False
                                )

                    _ <- Gtk.onWidgetSizeAllocate image setPixbuf
                    return ()
                  TrayImageSize size -> do
                    pixBuf0 <- getScaledPixBufFromInfo size info
                    pixBuf <- applyTransform image pixBuf0
                    Gtk.imageSetFromPixbuf image pixBuf

                Gtk.widgetGetStyleContext image
                  >>= flip Gtk.styleContextAddClass "tray-icon-image"

                Gtk.containerAdd eventBox image
                setTooltipText eventBox info

                let context =
                      ItemContext
                        { contextName = serviceName,
                          contextMenuPath = pathForMenu,
                          contextImage = image,
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
                  traverse_
                    ( \action -> case action of
                        Activate ->
                          catchAny
                            (void $ IC.activate client serviceName servicePath x y)
                            (logActionError "Activate")
                        SecondaryActivate ->
                          catchAny
                            ( void $
                                IC.secondaryActivate
                                  client
                                  serviceName
                                  servicePath
                                  x
                                  y
                            )
                            (logActionError "SecondaryActivate")
                        PopupMenu -> do
                          let menuPath' = menuPath currentInfo
                          traverse_
                            ( \p ->
                                catchAny
                                  ( case menuBackend of
                                      LibDBusMenu -> do
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
                                                    image
                                                    GravitySouth
                                                    GravityNorth
                                                    currentEvent
                                                  return False
                                        return ()
                                      HaskellDBusMenu -> do
                                        gtkMenu <- DBusMenu.buildMenu client serviceName p
                                        popupGtkMenu gtkMenu currentEvent
                                  )
                                  (logActionError "PopupMenu")
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
                          ( \e ->
                              trayLogger WARNING $
                                printf
                                  "Scroll failed for %s: %s"
                                  (coerce serviceName :: String)
                                  (show e)
                          )
                    )
                    direction'
                  return False

                MV.modifyMVar_ contextMap $ return . Map.insert serviceName context

                Gtk.widgetShowAll eventBox
                let packFn =
                      case alignment of
                        End -> Gtk.boxPackEnd
                        _ -> Gtk.boxPackStart

                packFn trayBox eventBox shouldExpand True 0
          updateHandler ItemRemoved ItemInfo {itemServiceName = name} =
            getContext name >>= removeWidget
            where
              removeWidget Nothing =
                trayLogger WARNING "removeWidget: unrecognized service name."
              removeWidget (Just ItemContext {contextButton = widgetToRemove}) =
                do
                  Gtk.containerRemove trayBox widgetToRemove
                  MV.modifyMVar_ contextMap $ return . Map.delete name
          updateHandler IconUpdated i = updateIconFromInfo i
          updateHandler OverlayIconUpdated i = updateIconFromInfo i
          updateHandler ToolTipUpdated info@ItemInfo {itemServiceName = name} =
            void $
              getContext name
                >>= traverse (flip setTooltipText info . contextButton)
          updateHandler _ _ = return ()

          maybeAddOverlayToPixbuf size info pixbuf = do
            _ <- runMaybeT $ do
              let overlayHeight = floor (fromIntegral size * overlayScale)
              overlayPixbuf <-
                MaybeT $
                  getOverlayPixBufFromInfo overlayHeight info
                    >>= traverse (scalePixbufToSize overlayHeight Gtk.OrientationHorizontal)
              lift $ do
                actualOHeight <- getPixbufHeight overlayPixbuf
                actualOWidth <- getPixbufWidth overlayPixbuf
                _mainHeight <- getPixbufHeight pixbuf
                _mainWidth <- getPixbufWidth pixbuf
                pixbufComposite
                  overlayPixbuf
                  pixbuf
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
            return pixbuf

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
                  ( updateHandler updateType info
                      >> reorderTrayByPriority
                      >> return False
                  )
                  ( \e -> do
                      trayLogger WARNING $ printf "Update handler failed: %s" (show e)
                      return False
                  )

      handlerId <- addUHandler uiUpdateHandler
      _ <- Gtk.onWidgetDestroy trayBox $ removeUHandler handlerId
      return trayBox
