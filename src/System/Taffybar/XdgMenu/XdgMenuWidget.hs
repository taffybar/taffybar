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
addItem :: (MenuShellClass msc) => msc -- ^ GTK menu
        -> DesktopEntry -- ^ Desktop entry
        -> IO ()
addItem ms de = do
  item <- menuItemNewWithLabel (deName de)
  set item [ widgetTooltipText := deComment de ]
  menuShellAppend ms item
  _ <- on item menuItemActivated $ deLaunch de
  return ()

-- | Add an xdg menu to a gtk menu by appending gtk menu items and
-- submenus.
addMenu :: (MenuShellClass msc) => msc -- ^ GTK menu
        -> [DesktopEntry] -- ^ List of all available desktop entries
        -> XdgMenu -- ^ XDG menu
        -> IO ()
addMenu ms des xm = do
  let subMenus = xmSubmenus xm
  let items = filter (flip matchesCondition (fromMaybe None (xmInclude xm))) des
  when (not (null items) || not (null subMenus)) $ do
    item <- menuItemNewWithLabel (xmName xm)
    menuShellAppend ms item
    subMenu <- menuNew
    menuItemSetSubmenu item subMenu
    mapM_ (addMenu subMenu des ) subMenus
    mapM_ (addItem subMenu) $ items

-- | Create a new XDG Menu Widget.
xdgMenuWidgetNew :: IO Widget
xdgMenuWidgetNew = do
  mb <- menuBarNew
  mm <- buildXdgMenu
  case mm of
    Just m -> do des <-getDesktopEntries m
                 addMenu mb des m
    Nothing -> do item <- menuItemNewWithLabel "???"
                  menuShellAppend mb item
  widgetShowAll mb
  return (toWidget mb)

-- | Show Xdg Menu Widget in a standlone frame.
testXdgMenuWidget :: IO ()
testXdgMenuWidget = do
   _ <- initGUI
   window <- windowNew
   _ <- window `on` deleteEvent $ liftIO mainQuit >> return False
   containerAdd window =<< xdgMenuWidgetNew
   widgetShowAll window
   mainGUI
