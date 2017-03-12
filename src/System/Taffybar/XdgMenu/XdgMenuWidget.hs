-----------------------------------------------------------------------------
-- |
-- Module      : System.Taffybar.XdgMenu.DesktopEntryCondition
-- Copyright   : (c) Ulf Jasper
-- License     : GPL3 (see LICENSE)
--
-- Maintainer  : Ulf Jasper <ulf.jasper@web.de>
-- Stability   : unstable
-- Portability : unportable
--
-- XdgMenuWidget provides a hierachical GTK menu which provides all
-- applicable desktop entries found on the system.  The menu is built
-- according to the version 1.1 of the XDG "Desktop Menu
-- Specification", see
-- https://specifications.freedesktop.org/menu-spec/menu-spec-1.1.html
--
-----------------------------------------------------------------------------
module System.Taffybar.XdgMenu.XdgMenuWidget (
  -- * Usage
  -- $usage
  xdgMenuWidgetNew)
where

import Control.Monad
import Control.Monad.Trans
import Data.Maybe
import Graphics.UI.Gtk 
import System.Taffybar.XdgMenu.DesktopEntry
import System.Taffybar.XdgMenu.DesktopEntryCondition
import System.Taffybar.XdgMenu.XdgMenu
import System.Environment
import System.Process

-- $usage
--
-- XdgMenuWidget provides a hierachical GTK menu which provides all
-- applicable desktop entries found on the system.  The menu is built
-- according to the version 1.1 of the XDG "Desktop Menu
-- Specification", see
-- https://specifications.freedesktop.org/menu-spec/menu-spec-1.1.html
--
-- In order to use this widget add the following line to your
-- @taffybar.hs@ file:
--
-- > import System.Taffybar.XdgMenu.XdgMenuWidget
-- > main = do
-- >   let menu = xdgMenuWidgetNew
--
-- now you can use @menu@ as any other Taffybar widget.


-- | Add a desktop entry to a gtk menu by appending a gtk menu item.
addItem :: (MenuShellClass msc) =>
           msc -- ^ GTK menu
        -> FinalEntry -- ^ Desktop entry
        -> IO ()
addItem ms de = do
  item <- menuItemNewWithLabel (feName de)
  set item [ widgetTooltipText := Just (feComment de)]
  menuShellAppend ms item
  _ <- on item menuItemActivated $ do
    let cmd = feCommand de
    putStrLn $ "Launching '" ++ cmd ++ "'"
    spawnCommand cmd
    return ()
  return ()
  
-- | Add an xdg menu to a gtk menu by appending gtk menu items and
-- submenus.
addMenu :: (MenuShellClass msc) =>
           msc -- ^ GTK menu
        -> FinalMenu -- ^ XDG menu
        -> IO ()
addMenu ms fm = do
  let subMenus = fmSubmenus fm
      items = fmEntries fm
  when (not (null items) || not (null subMenus)) $ do
    item <- menuItemNewWithLabel (fmName fm)
    menuShellAppend ms item
    subMenu <- menuNew
    menuItemSetSubmenu item subMenu
    mapM_ (addMenu subMenu) subMenus
    mapM_ (addItem subMenu) $ items

-- | Create a new XDG Menu Widget.
xdgMenuWidgetNew :: Maybe String -- ^ menu name, must end with a dash,
                                 -- e.g. "mate-" or "gnome-"
                 -> IO Widget
xdgMenuWidgetNew mMenuPrefix = do
  mb <- menuBarNew
  m <- buildFinalMenu mMenuPrefix
  addMenu mb m
  widgetShowAll mb
  return (toWidget mb)


-- | Show Xdg Menu Widget in a standalone frame.
testXdgMenuWidget :: IO ()
testXdgMenuWidget = do
   _ <- initGUI
   window <- windowNew
   _ <- window `on` deleteEvent $ liftIO mainQuit >> return False
   containerAdd window =<< xdgMenuWidgetNew Nothing
   widgetShowAll window
   mainGUI
