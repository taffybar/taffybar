{-# LANGUAGE OverloadedStrings #-}
module DBusMenu
  ( -- * High-level menu construction
    buildMenu
  , populateGtkMenu
  , buildGtkMenuItem

    -- * DBusMenu protocol operations
  , getLayout
  , aboutToShow
  , sendClicked

    -- * Layout tree
  , LayoutNode(..)
  , variantToLayout
  , tupleToLayout

    -- * Layout node property accessors
  , menuItemType
  , menuItemLabel
  , menuItemVisible
  , menuItemEnabled
  , menuItemChildrenDisplay
  , menuItemToggleType
  , menuItemToggleState
  ) where

import Control.Concurrent (forkIO)
import Control.Exception.Enclosed (catchAny)
import Control.Monad (forM_, void, when)
import Data.Int (Int32)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Word (Word32)
import DBus
import DBus.Client
import Data.GI.Base (unsafeCastTo)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.StablePtr
  ( StablePtr
  , castPtrToStablePtr
  , castStablePtrToPtr
  , deRefStablePtr
  , freeStablePtr
  , newStablePtr
  )
import qualified GI.GLib as GLib
import qualified GI.GObject.Objects.Object as GObject
import qualified GI.Gtk as Gtk
import System.Log.Logger (Priority(..), logM)
import Text.Printf

import qualified DBusMenu.Client as DM

dbusMenuLogger :: Priority -> String -> IO ()
dbusMenuLogger = logM "DBusMenu"

layoutPropNames :: [String]
layoutPropNames =
  [ "type"
  , "label"
  , "visible"
  , "enabled"
  , "children-display"
  , "toggle-type"
  , "toggle-state"
  ]

addCssClass :: Gtk.Widget -> T.Text -> IO ()
addCssClass widget cssClass =
  Gtk.widgetGetStyleContext widget >>= (`Gtk.styleContextAddClass` cssClass)

-- | A node in the DBusMenu layout tree.
data LayoutNode = LayoutNode
  { lnId :: Int32
  , lnProps :: Map String Variant
  , lnChildren :: [LayoutNode]
  } deriving (Eq, Show)

type LayoutTuple = (Int32, Map String Variant, [Variant])

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
  pure LayoutNode { lnId = i, lnProps = props, lnChildren = children }

-- | Convert a raw layout tuple into a LayoutNode.
tupleToLayout :: LayoutTuple -> LayoutNode
tupleToLayout (i, props, kids) =
  LayoutNode
    { lnId = i
    , lnProps = props
    , lnChildren = [ n | v <- kids, Just n <- [variantToLayout v] ]
    }

-- | Unwrap an Either MethodError, failing on Left.
unwrapCall :: String -> Either MethodError a -> IO a
unwrapCall label (Left err) = fail $ label <> " failed: " <> show err
unwrapCall _ (Right a) = pure a

-- | Notify the DBusMenu service that a menu item is about to be shown.
-- Returns True if the service indicates an update is needed.
aboutToShow :: Client -> BusName -> ObjectPath -> Int32 -> IO Bool
aboutToShow client dest path i =
  either (const False) id <$> DM.aboutToShow client dest path i

-- | Fetch the layout tree from the DBusMenu service.
getLayout :: Client -> BusName -> ObjectPath -> Int32 -> Int32 -> [String] -> IO (Word32, LayoutNode)
getLayout client dest path parentId depth propNames = do
  (rev, tup) <- unwrapCall "GetLayout" =<<
    DM.getLayout client dest path parentId depth propNames
  pure (rev, tupleToLayout tup)

-- | Send a \"clicked\" event to the DBusMenu service for the given item.
-- Runs asynchronously on a forked thread.
sendClicked :: Client -> BusName -> ObjectPath -> Int32 -> Word32 -> IO ()
sendClicked client dest path itemId ts = do
  dbusMenuLogger DEBUG $
    printf "sendClicked: id=%d dest=%s path=%s ts=%d"
           itemId (show dest) (show path) ts
  let mc = DM.eventMethodCall
        { methodCallDestination = Just dest
        , methodCallPath = path
        , methodCallBody =
            [ toVariant itemId
            , toVariant ("clicked" :: String)
            , toVariant (toVariant (0 :: Int32))
            , toVariant ts
            ]
        }
  -- Send on a forked thread to avoid blocking GTK; use `call` instead of
  -- `callNoReply` so we can detect service errors.
  void $ forkIO $ catchAny
    (do result <- call client mc
        case result of
          Left err -> dbusMenuLogger WARNING $
            printf "sendClicked: Event error: %s" (show err)
          Right _ -> dbusMenuLogger DEBUG "sendClicked: Event succeeded")
    (\e -> dbusMenuLogger WARNING $
           printf "sendClicked: Event exception: %s" (show e))

getPropS :: String -> LayoutNode -> Maybe String
getPropS key LayoutNode { lnProps = props } =
  Map.lookup key props >>= fromVariant

getPropB :: String -> LayoutNode -> Maybe Bool
getPropB key LayoutNode { lnProps = props } =
  Map.lookup key props >>= fromVariant

getPropI32 :: String -> LayoutNode -> Maybe Int32
getPropI32 key LayoutNode { lnProps = props } =
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

-- | The toggle type (e.g. @\"checkmark\"@, @\"radio\"@), if any.
menuItemToggleType :: LayoutNode -> Maybe String
menuItemToggleType = getPropS "toggle-type"

-- | The toggle state: 0 = off, 1 = on, -1 = indeterminate.
menuItemToggleState :: LayoutNode -> Maybe Int32
menuItemToggleState = getPropI32 "toggle-state"

-- | Populate a GTK Menu widget with items from a layout tree.
-- Clears any existing children first.
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

  -- Clear existing children (for refreshes, e.g. submenus).
  children <- Gtk.containerGetChildren gtkMenu
  forM_ children Gtk.widgetDestroy

  forM_ (lnChildren root) $ \child -> when (menuItemVisible child) $ do
    widget <- buildGtkMenuItem' client dest path dispatch gtkMenu child
    Gtk.menuShellAppend gtkMenu widget

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
        ( Map.insert itemId (sendClicked client dest path itemId =<< Gtk.getCurrentEventTime) m
        , ()
        )
      -- Thin trampoline: look up action from the persistent dispatch table
      -- at activation time rather than capturing it in a per-widget closure.
      _ <- Gtk.onMenuItemActivate item $ catchAny
        (do actions <- readIORef dispatch
            case Map.lookup itemId actions of
              Just action -> action
              Nothing -> dbusMenuLogger WARNING $
                printf "Dispatch: no action for item %d" itemId)
        (\e -> dbusMenuLogger WARNING $
               printf "Menu item %d dispatch failed: %s" itemId (show e))
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
      loadedRef <- newIORef (not (null (lnChildren node)))
      let refresh = void $ forkIO $ catchAny
            (do -- Run DBus calls on a forked thread to avoid blocking the GTK
                -- main loop (which would cause queued click events to be lost
                -- when populateGtkMenu rebuilds menu items).
                needUpdate <- aboutToShow client dest path (lnId node)
                loaded <- readIORef loadedRef
                when (needUpdate || not loaded) $ do
                  (_, layout) <- getLayout client dest path (lnId node) 1 layoutPropNames
                  -- Post GTK updates back on the main thread.  Using
                  -- PRIORITY_DEFAULT_IDLE ensures pending input events (clicks)
                  -- are processed first.
                  void $ GLib.idleAdd GLib.PRIORITY_DEFAULT_IDLE $ do
                    populateGtkMenu' client dest path dispatch submenu layout
                    writeIORef loadedRef True
                    Gtk.widgetShowAll submenu
                    return False)
            (\e -> dbusMenuLogger WARNING $
                   printf "Submenu %d refresh failed (stale ID?): %s"
                          (lnId node) (show e))
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
