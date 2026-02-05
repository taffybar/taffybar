-----------------------------------------------------------------------------
-- |
-- Module      : System.Taffybar.Widget.XDGMenu.MenuWidget
-- Copyright   : 2017 Ulf Jasper
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ulf Jasper <ulf.jasper@web.de>
-- Stability   : unstable
-- Portability : unportable
--
-- MenuWidget provides a hierachical GTK menu containing all
-- applicable desktop entries found on the system.  The menu is built
-- according to the version 1.1 of the XDG "Desktop Menu
-- Specification", see
-- https://specifications.freedesktop.org/menu-spec/menu-spec-1.1.html
-----------------------------------------------------------------------------

module System.Taffybar.Widget.XDGMenu.MenuWidget
  (
  -- * Usage
  -- $usage
  menuWidgetNew
  )
where

import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.Text as T
import           GI.Gtk hiding (Menu, imageMenuItemNew)
import           System.Log.Logger
import           System.Process
import           System.Taffybar.Widget.Generic.AutoSizeImage
import           System.Taffybar.Widget.Util
import           System.Taffybar.Widget.XDGMenu.Menu

-- $usage
--
-- In order to use this widget add the following line to your
-- @taffybar.hs@ file:
--
-- > import System.Taffybar.Widget.XDGMenu.MenuWidget
-- > main = do
-- >   let menu = menuWidgetNew $ Just "PREFIX-"
--
-- The menu will look for a file named "PREFIX-applications.menu" in the
-- (subdirectory "menus" of the) directories specified by the environment
-- variables XDG_CONFIG_HOME and XDG_CONFIG_DIRS. (If XDG_CONFIG_HOME is not set
-- or empty then $HOME/.config is used, if XDG_CONFIG_DIRS is not set or empty
-- then "/etc/xdg" is used). If no prefix is given (i.e. if you pass Nothing)
-- then the value of the environment variable XDG_MENU_PREFIX is used, if it is
-- set. If taffybar is running inside a desktop environment like Mate, Gnome,
-- XFCE etc. the environment variables XDG_CONFIG_DIRS and XDG_MENU_PREFIX
-- should be set and you may create the menu like this:
--
-- >   let menu = menuWidgetNew Nothing
--
-- Now you can use @menu@ as any other Taffybar widget.

logHere :: Priority -> String -> IO ()
logHere = logM "System.Taffybar.Widget.XDGMenu.MenuWidget"

-- | Add a desktop entry to a gtk menu by appending a gtk menu item.
addItem :: (IsMenuShell msc) =>
           msc -- ^ GTK menu
        -> MenuEntry -- ^ Desktop entry
        -> IO ()
addItem ms de = do
  item <- imageMenuItemNew (feName de) (getImageForMaybeIconName (feIcon de))
  setWidgetTooltipText item (feComment de)
  menuShellAppend ms item
  _ <- onMenuItemActivate item $ do
    let cmd = feCommand de
    logHere DEBUG $ "Launching '" ++ cmd ++ "'"
    _ <- spawnCommand cmd
    return ()
  return ()

-- | Add an xdg menu to a gtk menu by appending gtk menu items and submenus.
addMenu
  :: (IsMenuShell msc)
  => msc -- ^ A GTK menu
  -> Menu -- ^ A menu object
  -> IO ()
addMenu ms fm = do
  let subMenus = fmSubmenus fm
      items = fmEntries fm
  when (not (null items) || not (null subMenus)) $ do
    item <- imageMenuItemNew (T.pack $ fmName fm)
            (getImageForMaybeIconName (T.pack <$> fmIcon fm))
    menuShellAppend ms item
    subMenu <- menuNew
    menuItemSetSubmenu item (Just subMenu)
    mapM_ (addMenu subMenu) subMenus
    mapM_ (addItem subMenu) items

-- | Create a new XDG Menu Widget.
menuWidgetNew
  :: MonadIO m
  => Maybe String -- ^ menu name, must end with a dash, e.g. "mate-" or "gnome-"
  -> m GI.Gtk.Widget
menuWidgetNew mMenuPrefix = liftIO $ do
  mb <- menuBarNew
  m <- buildMenu mMenuPrefix
  addMenu mb m
  widgetShow mb
  toWidget mb
