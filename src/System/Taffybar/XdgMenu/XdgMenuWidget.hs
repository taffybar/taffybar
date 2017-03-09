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
           [String] -- ^ Preferred languages
        -> msc -- ^ GTK menu
        -> DesktopEntry -- ^ Desktop entry
        -> IO ()
addItem langs ms de = do
  item <- menuItemNewWithLabel (deName langs de)
  let mc = case deCommand de of
             Nothing -> Nothing
             Just cmd -> Just $ "(" ++ cmd ++ ")"
      mtt = case deComment langs de of
        Nothing -> mc
        Just tt -> Just $ tt ++ maybe "" ("\n" ++) mc
  set item [ widgetTooltipText := mtt ]
  menuShellAppend ms item
  _ <- on item menuItemActivated $ deLaunch de
  return ()

-- | Add an xdg menu to a gtk menu by appending gtk menu items and
-- submenus.
addMenu :: (MenuShellClass msc) =>
           [String] -- ^ Preferred languages
        -> msc -- ^ GTK menu
        -> [DesktopEntry] -- ^ List of all available desktop entries
        -> XdgMenu -- ^ XDG menu
        -> IO ()
addMenu langs ms des xm = do
  let subMenus = xmSubmenus xm
  let items = filter (not . flip matchesCondition (fromMaybe None (xmExclude xm))) $
        filter (flip matchesCondition (fromMaybe None (xmInclude xm))) des
  when (not (null items) || not (null subMenus)) $ do
    item <- menuItemNewWithLabel (xmName xm)
    menuShellAppend ms item
    subMenu <- menuNew
    menuItemSetSubmenu item subMenu
    mapM_ (addMenu langs subMenu des ) subMenus
    mapM_ (addItem langs subMenu) $ items

-- | Create a new XDG Menu Widget.
xdgMenuWidgetNew :: Maybe String -- ^ menu name, must end with a dash,
                                 -- e.g. "mate-" or "gnome-"
                 -> IO Widget
xdgMenuWidgetNew mMenuPrefix = do
  mb <- menuBarNew
  mm <- buildXdgMenu mMenuPrefix
  langs <- getPreferredLanguages
  case mm of
    Just xm -> do des <- getDesktopEntries langs xm
                  addMenu langs mb des xm
    Nothing -> do item <- menuItemNewWithLabel "???"
                  menuShellAppend mb item
  widgetShowAll mb
  return (toWidget mb)

-- | Determine locale language settings
getPreferredLanguages :: IO [String]
getPreferredLanguages = do
  mLcMessages <- lookupEnv "LC_MESSAGES"
  lang <- case mLcMessages of
               Nothing -> lookupEnv "LANG" -- FIXME?
               Just lm -> return (Just lm)
  case lang of
    Nothing -> return []
    Just l -> return $ doGetPreferredLanguages l

doGetPreferredLanguages :: String -> [String]
doGetPreferredLanguages l =
  let woEncoding = takeWhile (/= '.') l
      (language, _cm) = span (/= '_') woEncoding
      (country, _m) = span (/= '@') (if null _cm then "" else tail _cm)
      modifier = if null _m then "" else tail _m
  in dgl language country modifier
  where dgl "" "" "" = []
        dgl l  "" "" = [l]
        dgl l  c  "" = [l ++ "_" ++ c, l]
        dgl l  "" m  = [l ++ "@" ++ m, l]
        dgl l  c  m  = [l ++ "_" ++ c ++ "@" ++ m, l ++ "_" ++ c, l ++ "@" ++ m]


-- | Show Xdg Menu Widget in a standalone frame.
testXdgMenuWidget :: IO ()
testXdgMenuWidget = do
   _ <- initGUI
   window <- windowNew
   _ <- window `on` deleteEvent $ liftIO mainQuit >> return False
   containerAdd window =<< xdgMenuWidgetNew Nothing
   widgetShowAll window
   mainGUI
