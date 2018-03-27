-----------------------------------------------------------------------------
-- |
-- Module      : System.Taffybar.Menu.MenuWidget
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

module System.Taffybar.Menu.MenuWidget (
  -- * Usage
  -- $usage
  menuWidgetNew)
where

import Control.Monad
import Graphics.UI.Gtk hiding (Menu)
import System.Directory
import System.FilePath.Posix
import System.Process
import System.Taffybar.Menu.Menu

-- $usage
--
-- In order to use this widget add the following line to your
-- @taffybar.hs@ file:
--
-- > import System.Taffybar.Menu.MenuWidget
-- > main = do
-- >   let menu = menuWidgetNew $ Just "PREFIX-"
--
-- The menu will look for a file named "PREFIX-applications.menu" in
-- the (subdirectory "menus" of the) directories specified by the
-- environment variable XDG_CONFIG_DIRS and "/etc/xdg".  If no prefix
-- is given (i.e. if you pass Nothing) then the value of the
-- environment variable XDG_MENU_PREFIX is used, if it is set.  If
-- taffybar is running inside a desktop environment like Mate, Gnome,
-- XFCE etc. the environment variables XDG_CONFIG_DIRS and
-- XDG_MENU_PREFIX should be set and you may create the menu like this:
--
-- >   let menu = menuWidgetNew Nothing
--
-- Now you can use @menu@ as any other Taffybar widget.


-- | Add a desktop entry to a gtk menu by appending a gtk menu item.
addItem :: (MenuShellClass msc) =>
           msc -- ^ GTK menu
        -> MenuEntry -- ^ Desktop entry
        -> IO ()
addItem ms de = do
  item <- imageMenuItemNewWithLabel (feName de)
  set item [ widgetTooltipText := Just (feComment de)]
  setIcon item (feIcon de)
  menuShellAppend ms item
  _ <- on item menuItemActivated $ do
    let cmd = feCommand de
    putStrLn $ "Launching '" ++ cmd ++ "'"
    _ <- spawnCommand cmd
    return ()
  return ()

-- | Add an xdg menu to a gtk menu by appending gtk menu items and
-- submenus.
addMenu :: (MenuShellClass msc) =>
           msc -- ^ GTK menu
        -> Menu -- ^ menu
        -> IO ()
addMenu ms fm = do
  let subMenus = fmSubmenus fm
      items = fmEntries fm
  when (not (null items) || not (null subMenus)) $ do
    item <- imageMenuItemNewWithLabel (fmName fm)
    setIcon item (fmIcon fm)
    menuShellAppend ms item
    subMenu <- menuNew
    menuItemSetSubmenu item subMenu
    mapM_ (addMenu subMenu) subMenus
    mapM_ (addItem subMenu) $ items

setIcon :: ImageMenuItem -> Maybe String -> IO ()
setIcon _ Nothing = return ()
setIcon item (Just iconName) = do
  iconTheme <- iconThemeGetDefault
  hasIcon <- iconThemeHasIcon iconTheme iconName
  mImg <- if hasIcon
          then return . Just =<< imageNewFromIconName iconName IconSizeMenu
          else if isAbsolute iconName
               then do ex <- doesFileExist iconName
                       if ex
                         then do let defaultSize = 24 -- FIXME should auto-adjust to font size
                                 pb <- pixbufNewFromFileAtScale iconName
                                   defaultSize defaultSize True
                                 return . Just =<< imageNewFromPixbuf pb
                         else return Nothing
               else return Nothing
  case mImg of
    Just img -> imageMenuItemSetImage item img
    Nothing -> putStrLn $ "Icon not found: " ++ iconName


-- | Create a new XDG Menu Widget.
menuWidgetNew :: Maybe String -- ^ menu name, must end with a dash,
                              -- e.g. "mate-" or "gnome-"
              -> IO Widget
menuWidgetNew mMenuPrefix = do
  mb <- menuBarNew
  m <- buildMenu mMenuPrefix
  addMenu mb m
  widgetShowAll mb
  return (toWidget mb)

-- -- | Show Xdg Menu Widget in a standalone frame.
-- testMenuWidget :: IO ()
-- testMenuWidget = do
--    _ <- initGUI
--    window <- windowNew
--    _ <- window `on` deleteEvent $ liftIO mainQuit >> return False
--    containerAdd window =<< menuWidgetNew Nothing
--    widgetShowAll window
--    mainGUI
