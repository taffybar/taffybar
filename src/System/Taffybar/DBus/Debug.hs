{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      : System.Taffybar.DBus.Debug
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan A. Malison
-- Stability   : unstable
-- Portability : unportable
--
-- A small DBus interface for automation and debugging (e.g. triggering SNI
-- popup menus so they can be screenshot in a loop).
-----------------------------------------------------------------------------
module System.Taffybar.DBus.Debug
  ( handleDBusDebug
  ) where

import           Control.Applicative ((<|>))
import           Control.Concurrent.MVar (readMVar)
import           Control.Monad (void, when)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Reader (ask, asks, runReaderT)
import           Data.GI.Base (castTo)
import           Data.IORef (IORef, newIORef, readIORef, writeIORef)
import           Data.List (find, isInfixOf)
import           Data.Maybe (fromMaybe, listToMaybe)
import           Data.Text (pack)
import           DBus
import           DBus.Client
import qualified GI.Gdk.Enums as GdkE
import qualified GI.GLib as GLib
import qualified GI.Gtk as Gtk
import qualified DBusMenu
import           System.Log.Logger (Priority(..), logM)
import           System.Taffybar.Context

logD :: Priority -> String -> IO ()
logD = logM "System.Taffybar.DBus.Debug"

debugPath :: ObjectPath
debugPath = "/taffybar/debug"

debugInterfaceName :: InterfaceName
debugInterfaceName = "taffybar.debug"

watcherName :: BusName
watcherName = busName_ "org.kde.StatusNotifierWatcher"

watcherPath :: ObjectPath
watcherPath = objectPath_ "/StatusNotifierWatcher"

-- | DBus Properties.Get helper.
propsGet :: Client -> BusName -> ObjectPath -> String -> String -> IO (Maybe Variant)
propsGet client dest obj iface prop = do
  let mc =
        (methodCall obj "org.freedesktop.DBus.Properties" "Get")
          { methodCallDestination = Just dest
          , methodCallBody = [toVariant iface, toVariant prop]
          }
  result <- call client mc
  case result of
    Left _ -> pure Nothing
    Right reply ->
      case methodReturnBody reply of
        [v] -> pure (fromVariant v)
        _ -> pure Nothing

-- | Return (bus name, object path, display string) for currently registered SNI
-- entries from the watcher.
getRegisteredSNIEntries :: Client -> IO [(BusName, ObjectPath, String)]
getRegisteredSNIEntries client = do
  mv <- propsGet client watcherName watcherPath "org.kde.StatusNotifierWatcher" "RegisteredSNIEntries"
  let raw :: [(String, String)]
      raw = fromMaybe [] (mv >>= fromVariant)
  pure
    [ (busName_ bus, objectPath_ path, bus <> path)
    | (bus, path) <- raw
    ]

popupFirstSubmenuAtWidget :: Gtk.Menu -> IO ()
popupFirstSubmenuAtWidget rootMenu = do
  children <- Gtk.containerGetChildren rootMenu
  let go [] = pure ()
      go (w:ws) = do
        mi <- castTo Gtk.MenuItem w
        case mi of
          Nothing -> go ws
          Just menuItem -> do
            smw <- Gtk.menuItemGetSubmenu menuItem
            case smw of
              Nothing -> go ws
              Just sw -> do
                sm <- castTo Gtk.Menu sw
                case sm of
                  Nothing -> go ws
                  Just submenu -> do
                    Gtk.widgetShowAll submenu
                    Gtk.menuPopupAtWidget
                      submenu
                      menuItem
                      GdkE.GravityNorthEast
                      GdkE.GravityNorthWest
                      Nothing
  go children

destroyPreviewWindow :: IORef (Maybe Gtk.Window) -> IO ()
destroyPreviewWindow winRef = do
  mWin <- readIORef winRef
  case mWin of
    Nothing -> pure ()
    Just win -> do
      Gtk.widgetDestroy win
      writeIORef winRef Nothing

-- | Show the SNI menu (and optionally its first submenu) in a regular toplevel
-- window. This avoids Wayland xdg-popup restrictions that prevent
-- programmatic menu popups without an input event serial.
previewSniMenuWindow :: IORef (Maybe Gtk.Window) -> String -> Bool -> TaffyIO Bool
previewSniMenuWindow winRef match previewSubmenu = do
  ctx <- ask
  let client = sessionDBusClient ctx
  entries <- liftIO $ getRegisteredSNIEntries client
  let chosen =
        case match of
          "" -> listToMaybe entries
          _ -> find (\(_, _, disp) -> match `isInfixOf` disp) entries <|> listToMaybe entries
  case chosen of
    Nothing -> liftIO (logD WARNING "No SNI entries found.") >> pure False
    Just (itemBus, itemPath, disp) -> do
      mMenuPath <- liftIO $ propsGet client itemBus itemPath "org.kde.StatusNotifierItem" "Menu" >>= \mv ->
        pure (mv >>= fromVariant)
      case mMenuPath of
        Nothing -> liftIO (logD WARNING ("Entry has no Menu property: " <> disp)) >> pure False
        Just menuPath -> do
          liftIO $ void $ GLib.idleAdd GLib.PRIORITY_LOW $ do
            destroyPreviewWindow winRef

            gtkMenu <- DBusMenu.buildMenu client itemBus menuPath
            Gtk.widgetShowAll gtkMenu

            win <- Gtk.windowNew Gtk.WindowTypeToplevel
            writeIORef winRef (Just win)

            Gtk.windowSetTitle win (pack ("SNI Menu Preview: " <> disp))
            -- Keep this a normal toplevel window. Wayland compositors will
            -- often reject "popup" type windows without a parent/serial, which
            -- defeats the point of this preview.
            Gtk.windowSetDecorated win True
            Gtk.windowSetResizable win True
            Gtk.windowSetDefaultSize win 520 420

            box <- Gtk.boxNew Gtk.OrientationHorizontal 0
            Gtk.containerAdd win box

            -- Try to render the menu widgets directly. GtkMenu is not usually
            -- embedded like this, but it tends to work well enough for style
            -- debugging.
            Gtk.boxPackStart box gtkMenu False False 0

            when previewSubmenu $ do
              children <- Gtk.containerGetChildren gtkMenu
              let findFirstSubmenu [] = pure ()
                  findFirstSubmenu (w:ws) = do
                    mi <- castTo Gtk.MenuItem w
                    case mi of
                      Nothing -> findFirstSubmenu ws
                      Just menuItem -> do
                        smw <- Gtk.menuItemGetSubmenu menuItem
                        case smw of
                          Nothing -> findFirstSubmenu ws
                          Just sw -> do
                            sm <- castTo Gtk.Menu sw
                            case sm of
                              Nothing -> findFirstSubmenu ws
                              Just submenu -> do
                                Gtk.widgetShowAll submenu
                                Gtk.boxPackStart box submenu False False 0
              findFirstSubmenu children

            _ <- Gtk.onWidgetDestroy win (writeIORef winRef Nothing)
            Gtk.widgetShowAll win
            pure GLib.SOURCE_REMOVE

          liftIO $ logD INFO ("Opened preview window for " <> disp)
          pure True

popupSniMenu :: String -> Bool -> TaffyIO Bool
popupSniMenu match popupSubmenu = do
  ctx <- ask
  let client = sessionDBusClient ctx
  entries <- liftIO $ getRegisteredSNIEntries client
  let chosen =
        case match of
          "" -> listToMaybe entries
          _ -> find (\(_, _, disp) -> match `isInfixOf` disp) entries <|> listToMaybe entries
  case chosen of
    Nothing -> liftIO (logD WARNING "No SNI entries found.") >> pure False
    Just (itemBus, itemPath, disp) -> do
      mMenuPath <- liftIO $ propsGet client itemBus itemPath "org.kde.StatusNotifierItem" "Menu" >>= \mv ->
        pure (mv >>= fromVariant)
      case mMenuPath of
        Nothing -> liftIO (logD WARNING ("Entry has no Menu property: " <> disp)) >> pure False
        Just menuPath -> do
          -- Schedule GTK work on the main loop.
          liftIO $ void $ GLib.idleAdd GLib.PRIORITY_LOW $ do
            gtkMenu <- DBusMenu.buildMenu client itemBus menuPath
            -- Attach to the bar window if possible (CSS + lifetime management).
            wins <- readMVar (existingWindows ctx)
            let mWin = case wins of
                  ((_, win):_) -> Just win
                  _ -> Nothing
            case mWin of
              Just win -> do
                -- Pop it below the bar window's top-left corner.
                Gtk.widgetShowAll gtkMenu
                Gtk.menuPopupAtWidget
                  gtkMenu
                  win
                  GdkE.GravitySouthWest
                  GdkE.GravityNorthWest
                  Nothing
                when popupSubmenu $ popupFirstSubmenuAtWidget gtkMenu
              Nothing -> do
                Gtk.widgetShowAll gtkMenu
                Gtk.menuPopupAtPointer gtkMenu Nothing
            pure GLib.SOURCE_REMOVE
          liftIO $ logD INFO ("Triggered SNI menu popup for " <> disp)
          pure True

exportDebugInterface :: TaffyIO ()
exportDebugInterface = do
  ctx <- ask
  client <- asks sessionDBusClient
  previewWinRef <- liftIO $ newIORef Nothing
  let listEntriesIO = fmap (\(_, _, d) -> d) <$> getRegisteredSNIEntries client
      popupIO s b = runReaderT (popupSniMenu s b) ctx
      previewIO s b = runReaderT (previewSniMenuWindow previewWinRef s b) ctx
      iface =
        defaultInterface
          { interfaceName = debugInterfaceName
          , interfaceMethods =
              [ autoMethod "ListSNIEntries" listEntriesIO
              , autoMethod "PopupSniMenu" popupIO
              , autoMethod "PreviewSniMenu" previewIO
              ]
          }
  liftIO $ do
    _ <- requestName client "taffybar.debug" [nameAllowReplacement, nameReplaceExisting]
    export client debugPath iface

handleDBusDebug :: TaffybarConfig -> TaffybarConfig
handleDBusDebug cfg =
  cfg { startupHook = startupHook cfg >> exportDebugInterface }
