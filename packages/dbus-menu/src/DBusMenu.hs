{-# LANGUAGE OverloadedStrings #-}

module DBusMenu
  ( -- * High-level menu construction
    buildMenu,
    populateGtkMenu,
    buildGtkMenuItem,

    -- * DBusMenu protocol operations
    getLayout,
    aboutToShow,
    sendClicked,

    -- * Layout tree
    LayoutNode (..),
    variantToLayout,
    tupleToLayout,

    -- * Layout node property accessors
    menuItemType,
    menuItemLabel,
    menuItemVisible,
    menuItemEnabled,
    menuItemChildrenDisplay,
    menuItemToggleType,
    menuItemToggleState,
    MenuItemKind (..),
    MenuItemShape (..),
    menuItemShape,
  )
where

import Control.Concurrent (forkIO)
import Control.Exception.Enclosed (catchAny)
import Control.Monad (forM, forM_, unless, void, when)
import DBus
import DBus.Client
import qualified DBusMenu.Client as DM
import DBusMenu.Reconcile (ReconcileAction (..), planReconciliation)
import Data.Either (fromRight)
import Data.GI.Base (unsafeCastTo)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Int (Int32)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe, isNothing)
import qualified Data.Text as T
import Data.Word (Word32)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.StablePtr
  ( StablePtr,
    castPtrToStablePtr,
    castStablePtrToPtr,
    deRefStablePtr,
    freeStablePtr,
    newStablePtr,
  )
import qualified GI.GLib as GLib
import qualified GI.GObject.Objects.Object as GObject
import qualified GI.Gtk as Gtk
import System.Log.Logger (Priority (..), logM)
import Text.Printf
import Text.Read (readMaybe)

dbusMenuLogger :: Priority -> String -> IO ()
dbusMenuLogger = logM "DBusMenu"

layoutPropNames :: [String]
layoutPropNames =
  [ "type",
    "label",
    "visible",
    "enabled",
    "children-display",
    "toggle-type",
    "toggle-state"
  ]

addCssClass :: Gtk.Widget -> T.Text -> IO ()
addCssClass widget cssClass =
  Gtk.widgetGetStyleContext widget >>= (`Gtk.styleContextAddClass` cssClass)

-- | A node in the DBusMenu layout tree.
data LayoutNode = LayoutNode
  { lnId :: Int32,
    lnProps :: Map String Variant,
    lnChildren :: [LayoutNode]
  }
  deriving (Eq, Show)

type LayoutTuple = (Int32, Map String Variant, [Variant])

data MenuItemKind
  = NormalMenuItem
  | SeparatorMenuItem
  | CheckmarkMenuItem
  | RadioMenuItem
  deriving (Eq, Show)

-- | The aspects of a DBusMenu item that determine its concrete GTK widget
-- type and whether it owns a submenu. Items with equal shapes can be updated
-- in place without invalidating an in-progress GTK click.
data MenuItemShape = MenuItemShape
  { menuItemKind :: MenuItemKind,
    menuItemShapeHasSubmenu :: Bool
  }
  deriving (Eq, Show)

-- | Menu-level click dispatch table.  Maps DBusMenu item IDs to their click
-- actions.  The table is owned by the persistent 'Gtk.Menu' widget and
-- survives item rebuilds, decoupling action dispatch from individual widget
-- lifecycles.
type ClickDispatch = IORef (Map Int32 (IO ()))

clickDispatchKey :: T.Text
clickDispatchKey = "dbus-menu.click-dispatch"

getMenuClickDispatch :: Gtk.Menu -> IO (Maybe ClickDispatch)
getMenuClickDispatch menu = do
  p <- GObject.objectGetData menu clickDispatchKey
  if p == nullPtr
    then pure Nothing
    else Just <$> deRefStablePtr (castPtrToStablePtr p)

setMenuClickDispatch :: Gtk.Menu -> ClickDispatch -> IO ()
setMenuClickDispatch menu dispatch = do
  -- Tie the dispatch table to the GObject lifetime. We store a StablePtr to the
  -- IORef; freeing the StablePtr does not free the IORef itself (which can
  -- still be referenced by signal handlers), but it does avoid leaking the
  -- stable pointer.
  sp <- newStablePtr dispatch
  GObject.objectSetDataFull
    menu
    clickDispatchKey
    (castStablePtrToPtr sp :: Ptr ())
    (Just $ \p -> freeStablePtr (castPtrToStablePtr p :: StablePtr ClickDispatch))

ensureMenuClickDispatch :: Gtk.Menu -> IO ClickDispatch
ensureMenuClickDispatch menu = do
  existing <- getMenuClickDispatch menu
  case existing of
    Just dispatch -> pure dispatch
    Nothing -> do
      dispatch <- newIORef Map.empty
      setMenuClickDispatch menu dispatch
      pure dispatch

ensureMenuClickDispatchWith :: Gtk.Menu -> ClickDispatch -> IO ()
ensureMenuClickDispatchWith menu dispatch = do
  existing <- getMenuClickDispatch menu
  case existing of
    Nothing -> setMenuClickDispatch menu dispatch
    Just existingDispatch ->
      when (existingDispatch /= dispatch) $
        dbusMenuLogger WARNING "Menu already has a different click dispatch; leaving existing one in place"

-- | Parse a DBus Variant into a LayoutNode.
variantToLayout :: Variant -> Maybe LayoutNode
variantToLayout v = do
  (i, props, kids) <- fromVariant v :: Maybe LayoutTuple
  children <- traverse variantToLayout kids
  pure LayoutNode {lnId = i, lnProps = props, lnChildren = children}

-- | Convert a raw layout tuple into a LayoutNode.
tupleToLayout :: LayoutTuple -> LayoutNode
tupleToLayout (i, props, kids) =
  LayoutNode
    { lnId = i,
      lnProps = props,
      lnChildren = [n | v <- kids, Just n <- [variantToLayout v]]
    }

-- | Unwrap an Either MethodError, failing on Left.
unwrapCall :: String -> Either MethodError a -> IO a
unwrapCall label (Left err) = fail $ label <> " failed: " <> show err
unwrapCall _ (Right a) = pure a

-- | Notify the DBusMenu service that a menu item is about to be shown.
-- Returns True if the service indicates an update is needed.
aboutToShow :: Client -> BusName -> ObjectPath -> Int32 -> IO Bool
aboutToShow client dest path i =
  fromRight False <$> DM.aboutToShow client dest path i

-- | Fetch the layout tree from the DBusMenu service.
getLayout :: Client -> BusName -> ObjectPath -> Int32 -> Int32 -> [String] -> IO (Word32, LayoutNode)
getLayout client dest path parentId depth propNames = do
  (rev, tup) <-
    unwrapCall "GetLayout"
      =<< DM.getLayout client dest path parentId depth propNames
  pure (rev, tupleToLayout tup)

-- | Send a \"clicked\" event to the DBusMenu service for the given item.
-- Runs asynchronously on a forked thread.
sendClicked :: Client -> BusName -> ObjectPath -> Int32 -> Word32 -> IO ()
sendClicked client dest path itemId ts = do
  dbusMenuLogger DEBUG $
    printf
      "sendClicked: id=%d dest=%s path=%s ts=%d"
      itemId
      (show dest)
      (show path)
      ts
  let mc =
        DM.eventMethodCall
          { methodCallDestination = Just dest,
            methodCallPath = path,
            methodCallBody =
              [ toVariant itemId,
                toVariant ("clicked" :: String),
                toVariant (toVariant (0 :: Int32)),
                toVariant ts
              ]
          }
  -- Send on a forked thread to avoid blocking GTK; use `call` instead of
  -- `callNoReply` so we can detect service errors.
  void $
    forkIO $
      catchAny
        ( do
            result <- call client mc
            case result of
              Left err ->
                dbusMenuLogger WARNING $
                  printf "sendClicked: Event error: %s" (show err)
              Right _ -> dbusMenuLogger DEBUG "sendClicked: Event succeeded"
        )
        (dbusMenuLogger WARNING . printf "sendClicked: Event exception: %s" . show)

getPropS :: String -> LayoutNode -> Maybe String
getPropS key LayoutNode {lnProps = props} =
  Map.lookup key props >>= fromVariant

getPropB :: String -> LayoutNode -> Maybe Bool
getPropB key LayoutNode {lnProps = props} =
  Map.lookup key props >>= fromVariant

getPropI32 :: String -> LayoutNode -> Maybe Int32
getPropI32 key LayoutNode {lnProps = props} =
  Map.lookup key props >>= fromVariant

-- | The item type (e.g. @\"separator\"@), or Nothing for standard items.
menuItemType :: LayoutNode -> Maybe String
menuItemType = getPropS "type"

-- | The display label, defaulting to @\"\"@ if absent.
menuItemLabel :: LayoutNode -> String
menuItemLabel n =
  fromMaybe "" (getPropS "label" n)

-- | Whether the item is visible, defaulting to True.
menuItemVisible :: LayoutNode -> Bool
menuItemVisible n = fromMaybe True (getPropB "visible" n)

-- | Whether the item is enabled\/sensitive, defaulting to True.
menuItemEnabled :: LayoutNode -> Bool
menuItemEnabled n = fromMaybe True (getPropB "enabled" n)

-- | How children should be displayed for this item (typically @\"submenu\"@),
-- if provided by the service.
menuItemChildrenDisplay :: LayoutNode -> Maybe String
menuItemChildrenDisplay = getPropS "children-display"

menuItemHasSubmenu :: LayoutNode -> Bool
menuItemHasSubmenu n =
  menuItemChildrenDisplay n == Just "submenu" || not (null (lnChildren n))

menuItemShape :: LayoutNode -> MenuItemShape
menuItemShape node =
  MenuItemShape
    { menuItemKind = case menuItemType node of
        Just "separator" -> SeparatorMenuItem
        _ -> case menuItemToggleType node of
          Just "checkmark" -> CheckmarkMenuItem
          Just "radio" -> RadioMenuItem
          _ -> NormalMenuItem,
      menuItemShapeHasSubmenu = menuItemHasSubmenu node
    }

-- | The toggle type (e.g. @\"checkmark\"@, @\"radio\"@), if any.
menuItemToggleType :: LayoutNode -> Maybe String
menuItemToggleType = getPropS "toggle-type"

-- | The toggle state: 0 = off, 1 = on, -1 = indeterminate.
menuItemToggleState :: LayoutNode -> Maybe Int32
menuItemToggleState = getPropI32 "toggle-state"

-- | Populate a GTK Menu widget with items from a layout tree.
--
-- Existing items are reconciled by DBusMenu ID and retained whenever their
-- GTK shape is compatible with the new layout. Keeping the same widget is
-- important: GTK activates menu items on button release, so destroying an
-- item between button press and release silently loses the click.
--
-- CSS classes applied to the menu: @dbusmenu-menu@
populateGtkMenu :: Client -> BusName -> ObjectPath -> Gtk.Menu -> LayoutNode -> IO ()
populateGtkMenu client dest path gtkMenu root = do
  dispatch <- ensureMenuClickDispatch gtkMenu
  populateGtkMenu' client dest path dispatch gtkMenu root

-- | Internal: populate with a shared dispatch table.
populateGtkMenu' :: Client -> BusName -> ObjectPath -> ClickDispatch -> Gtk.Menu -> LayoutNode -> IO ()
populateGtkMenu' client dest path dispatch gtkMenu root = do
  gtkMenuW <- Gtk.toWidget gtkMenu
  addCssClass gtkMenuW "dbusmenu-menu"

  children <- Gtk.containerGetChildren gtkMenu
  maybeExisting <- forM children getExistingMenuItem
  forM_ (zip children maybeExisting) $ \(widget, managedItem) ->
    when (isNothing managedItem) $
      Gtk.widgetDestroy widget
  let existing = catMaybes maybeExisting
  let existingById = Map.fromList [(itemId, (widget, item, shape)) | (itemId, widget, item, shape) <- existing]
      existingShapes = Map.map (\(_, _, shape) -> shape) existingById
      desiredNodes = filter menuItemVisible (lnChildren root)
      reconciliation =
        planReconciliation
          existingShapes
          [(lnId node, menuItemShape node) | node <- desiredNodes]

  renderedItems <-
    forM (zip desiredNodes reconciliation) $ \(node, action) -> do
      let itemId = lnId node
      case action of
        ReuseItem _ -> do
          let (_, existingItem, _) = existingById Map.! itemId
          updateGtkMenuItem client dest path dispatch existingItem node
          pure (itemId, existingItem, True)
        BuildItem _ -> do
          newItem <- buildGtkMenuItem' client dest path dispatch gtkMenu node
          Gtk.menuShellAppend gtkMenu newItem
          pure (itemId, newItem, False)

  let retainedItems = [item | (_, item, True) <- renderedItems]
  forM_ existing $ \(_, widget, item, _) ->
    unless (item `elem` retainedItems) $
      Gtk.widgetDestroy widget

  remainingChildren <- Gtk.containerGetChildren gtkMenu
  remainingIds <- catMaybes <$> forM remainingChildren getManagedMenuItemId
  let desiredIds = [itemId | (itemId, _, _) <- renderedItems]
  unless (remainingIds == desiredIds) $
    forM_ (zip [0 ..] renderedItems) $ \(position, (_, item, _)) ->
      Gtk.menuReorderChild gtkMenu item position

getManagedMenuItemId :: Gtk.Widget -> IO (Maybe Int32)
getManagedMenuItemId widget = do
  name <- Gtk.widgetGetName widget
  pure $ T.stripPrefix "dbusmenu-item-" name >>= readMaybe . T.unpack

getExistingMenuItem :: Gtk.Widget -> IO (Maybe (Int32, Gtk.Widget, Gtk.MenuItem, MenuItemShape))
getExistingMenuItem widget = do
  maybeItemId <- getManagedMenuItemId widget
  case maybeItemId of
    Nothing -> pure Nothing
    Just itemId -> do
      item <- unsafeCastTo Gtk.MenuItem widget
      shape <- getRenderedMenuItemShape widget
      pure $ Just (itemId, widget, item, shape)

getRenderedMenuItemShape :: Gtk.Widget -> IO MenuItemShape
getRenderedMenuItemShape widget = do
  context <- Gtk.widgetGetStyleContext widget
  isSeparator <- Gtk.styleContextHasClass context "dbusmenu-separator"
  isCheckmark <- Gtk.styleContextHasClass context "dbusmenu-checkmark"
  isRadio <- Gtk.styleContextHasClass context "dbusmenu-radio"
  hasSubmenu <- Gtk.styleContextHasClass context "dbusmenu-has-submenu"
  let kind
        | isSeparator = SeparatorMenuItem
        | isCheckmark = CheckmarkMenuItem
        | isRadio = RadioMenuItem
        | otherwise = NormalMenuItem
  pure $
    MenuItemShape
      { menuItemKind = kind,
        menuItemShapeHasSubmenu = hasSubmenu
      }

setCssClass :: Gtk.StyleContext -> T.Text -> Bool -> IO ()
setCssClass context cssClass enabled =
  if enabled
    then Gtk.styleContextAddClass context cssClass
    else Gtk.styleContextRemoveClass context cssClass

updateGtkMenuItem :: Client -> BusName -> ObjectPath -> ClickDispatch -> Gtk.MenuItem -> LayoutNode -> IO ()
updateGtkMenuItem client dest path dispatch item node = do
  let shape = menuItemShape node
      itemId = lnId node
      isChecked = menuItemToggleState node == Just 1
  case menuItemKind shape of
    SeparatorMenuItem -> pure ()
    kind -> do
      Gtk.menuItemSetLabel item (T.pack (menuItemLabel node))
      Gtk.menuItemSetUseUnderline item True
      case kind of
        CheckmarkMenuItem -> updateCheckItem False isChecked
        RadioMenuItem -> updateCheckItem True isChecked
        _ -> pure ()

  itemW <- Gtk.toWidget item
  context <- Gtk.widgetGetStyleContext itemW
  setCssClass context "dbusmenu-checked" isChecked
  Gtk.widgetSetSensitive item (menuItemEnabled node)

  unless (menuItemShapeHasSubmenu shape) $
    atomicModifyIORef' dispatch $ \actions ->
      ( Map.insert itemId (sendClicked client dest path itemId =<< Gtk.getCurrentEventTime) actions,
        ()
      )
  where
    updateCheckItem drawAsRadio active = do
      checkItem <- unsafeCastTo Gtk.CheckMenuItem item
      Gtk.checkMenuItemSetDrawAsRadio checkItem drawAsRadio
      Gtk.checkMenuItemSetActive checkItem active

-- | Build a single GTK MenuItem from a layout node.
--
-- CSS classes applied:
--
-- * @dbusmenu-item@ on every item
-- * @dbusmenu-separator@ on separator items
-- * @dbusmenu-toggle@ on checkmark and radio items
-- * @dbusmenu-checkmark@ on checkmark items
-- * @dbusmenu-radio@ on radio items
-- * @dbusmenu-checked@ on active\/checked toggle items
-- * @dbusmenu-has-submenu@ on items with children
--
-- Submenus get:
--
-- * @dbusmenu-menu@ (base menu class)
-- * @dbusmenu-submenu@
buildGtkMenuItem :: Client -> BusName -> ObjectPath -> Gtk.Menu -> LayoutNode -> IO Gtk.MenuItem
buildGtkMenuItem client dest path parentMenu node = do
  dispatch <- ensureMenuClickDispatch parentMenu
  buildGtkMenuItem' client dest path dispatch parentMenu node

-- | Internal: build a menu item with a shared dispatch table.
--
-- Leaf-item click handlers are thin trampolines that look up the item's
-- action in the dispatch table at activation time, decoupling action
-- dispatch from individual widget lifecycles.
buildGtkMenuItem' :: Client -> BusName -> ObjectPath -> ClickDispatch -> Gtk.Menu -> LayoutNode -> IO Gtk.MenuItem
buildGtkMenuItem' client dest path dispatch _parentMenu node = do
  let isChecked = menuItemToggleState node == Just 1
  item <- case menuItemType node of
    Just "separator" -> do
      sep <- Gtk.separatorMenuItemNew
      unsafeCastTo Gtk.MenuItem sep
    _ -> do
      let label = T.pack (menuItemLabel node)
      case menuItemToggleType node of
        Just "checkmark" -> do
          c <- Gtk.checkMenuItemNewWithMnemonic label
          Gtk.checkMenuItemSetActive c isChecked
          unsafeCastTo Gtk.MenuItem c
        Just "radio" -> do
          c <- Gtk.checkMenuItemNewWithMnemonic label
          Gtk.checkMenuItemSetDrawAsRadio c True
          Gtk.checkMenuItemSetActive c isChecked
          unsafeCastTo Gtk.MenuItem c
        _ -> Gtk.menuItemNewWithMnemonic label

  Gtk.widgetSetName item (T.pack ("dbusmenu-item-" <> show (lnId node)))
  itemW <- Gtk.toWidget item
  addCssClass itemW "dbusmenu-item"

  case menuItemType node of
    Just "separator" -> addCssClass itemW "dbusmenu-separator"
    _ -> pure ()

  case menuItemToggleType node of
    Just "checkmark" -> do
      addCssClass itemW "dbusmenu-toggle"
      addCssClass itemW "dbusmenu-checkmark"
      when isChecked $ addCssClass itemW "dbusmenu-checked"
    Just "radio" -> do
      addCssClass itemW "dbusmenu-toggle"
      addCssClass itemW "dbusmenu-radio"
      when isChecked $ addCssClass itemW "dbusmenu-checked"
    _ -> pure ()

  Gtk.widgetSetSensitive item (menuItemEnabled node)

  -- Submenu handling: build children now, and refresh on show via AboutToShow/GetLayout.
  --
  -- Important: do not infer "leaf" solely from lnChildren. When GetLayout is
  -- called with a limited recursionDepth (or when a service lazily populates),
  -- submenu items can legitimately have no embedded children but still need to
  -- behave as submenus (signaled by children-display="submenu").
  if not (menuItemHasSubmenu node)
    then do
      -- Register click action in the menu-level dispatch table.
      let itemId = lnId node
      atomicModifyIORef' dispatch $ \m ->
        ( Map.insert itemId (sendClicked client dest path itemId =<< Gtk.getCurrentEventTime) m,
          ()
        )
      -- Thin trampoline: look up action from the persistent dispatch table
      -- at activation time rather than capturing it in a per-widget closure.
      _ <-
        Gtk.onMenuItemActivate item $
          catchAny
            ( do
                actions <- readIORef dispatch
                case Map.lookup itemId actions of
                  Just action -> action
                  Nothing ->
                    dbusMenuLogger WARNING $
                      printf "Dispatch: no action for item %d" itemId
            )
            (dbusMenuLogger WARNING . printf "Menu item %d dispatch failed: %s" itemId . show)
      pure ()
    else do
      addCssClass itemW "dbusmenu-has-submenu"
      submenu <- Gtk.menuNew
      ensureMenuClickDispatchWith submenu dispatch
      Gtk.widgetSetName submenu (T.pack ("dbusmenu-submenu-" <> show (lnId node)))
      submenuW <- Gtk.toWidget submenu
      addCssClass submenuW "dbusmenu-submenu"
      -- Populate with the eagerly-fetched layout so submenus are usable even if
      -- the service doesn't support/require lazy updates.
      populateGtkMenu' client dest path dispatch submenu node
      refreshGenerationRef <- newIORef (0 :: Int)
      destroyedRef <- newIORef False
      _ <- Gtk.onWidgetDestroy submenu $ writeIORef destroyedRef True
      let refresh = do
            loaded <- not . null <$> Gtk.containerGetChildren submenu
            generation <-
              atomicModifyIORef' refreshGenerationRef $ \current ->
                let next = current + 1
                 in (next, next)
            void $
              forkIO $
                catchAny
                  ( do
                      -- Keep DBus calls off the GTK main loop. Responses may
                      -- arrive while the user is interacting with the menu, so
                      -- the GTK update reconciles stable item widgets by ID.
                      needUpdate <- aboutToShow client dest path (lnId node)
                      when (needUpdate || not loaded) $ do
                        (_, layout) <- getLayout client dest path (lnId node) 1 layoutPropNames
                        void $
                          GLib.idleAdd GLib.PRIORITY_DEFAULT_IDLE $
                            catchAny
                              ( do
                                  currentGeneration <- readIORef refreshGenerationRef
                                  destroyed <- readIORef destroyedRef
                                  unless destroyed $ do
                                    visible <- Gtk.widgetGetVisible submenu
                                    when (generation == currentGeneration && visible) $ do
                                      populateGtkMenu' client dest path dispatch submenu layout
                                      Gtk.widgetShowAll submenu
                                  return False
                              )
                              ( \err -> do
                                  dbusMenuLogger WARNING $
                                    printf "Submenu %d GTK reconciliation failed: %s" (lnId node) (show err)
                                  return False
                              )
                  )
                  ( dbusMenuLogger WARNING
                      . printf "Submenu %d refresh failed (stale ID?): %s" (lnId node)
                      . show
                  )
      _ <- Gtk.onWidgetShow submenu $ do
        refresh
        Gtk.widgetShowAll submenu
      Gtk.menuItemSetSubmenu item (Just submenu)

  pure item

-- | Build a complete GTK Menu from a DBusMenu service.
--
-- CSS classes applied to the root menu: @dbusmenu-menu@, @dbusmenu-root@
buildMenu :: Client -> BusName -> ObjectPath -> IO Gtk.Menu
buildMenu client dest path = do
  dbusMenuLogger DEBUG $
    printf "buildMenu: dest=%s path=%s" (show dest) (show path)
  _ <- aboutToShow client dest path 0
  (_, layout) <- getLayout client dest path 0 (-1) layoutPropNames
  dbusMenuLogger DEBUG $
    printf "buildMenu: root has %d children" (length (lnChildren layout))
  menu <- Gtk.menuNew
  dispatch <- ensureMenuClickDispatch menu
  Gtk.widgetSetName menu "dbusmenu-root"
  menuW <- Gtk.toWidget menu
  addCssClass menuW "dbusmenu-root"
  populateGtkMenu' client dest path dispatch menu layout
  pure menu
