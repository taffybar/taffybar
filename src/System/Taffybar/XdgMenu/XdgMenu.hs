-----------------------------------------------------------------------------
-- |
-- Module      : System.Taffybar.XdgMenu.XdgMenu
-- Copyright   : (c) Ulf Jasper
-- License     : GPL3 (see LICENSE)
--
-- Maintainer  : Ulf Jasper <ulf.jasper@web.de>
-- Stability   : unstable
-- Portability : unportable
--
-- Implementation of version 1.1 of the XDG "Desktop Menu
-- Specification", see
-- https://specifications.freedesktop.org/menu-spec/menu-spec-1.1.html
---- specification, see
-- See also 'XdgMenuWidget'.
--
-----------------------------------------------------------------------------
module System.Taffybar.XdgMenu.XdgMenu (
  XdgMenu(..),
  buildXdgMenu,
  getDesktopEntries)
where

import Control.Monad
import Data.List
import Data.Maybe
import System.Taffybar.XdgMenu.DesktopEntry
import System.Taffybar.XdgMenu.DesktopEntryCondition
import System.Environment
import System.FilePath.Posix
import Text.XML.Light
import Text.XML.Light.Helpers
import System.Posix.Files
import qualified Data.Set as S
import qualified Debug.Trace as D



-- Environment Variables
getXdgConfigDir :: IO String
getXdgConfigDir = do
  ch <- lookupEnv "XDG_CONFIG_HOME"
  cd <- lookupEnv "XDG_CONFIG_DIRS"
  let dirs = catMaybes [ch]
             ++ maybe [] splitSearchPath cd
  exDirs <- existingDirs dirs
  return $ if null exDirs
    then "/etc/xdg"
    else head exDirs

existingDirs :: [FilePath] -> IO [FilePath]
existingDirs  dirs = do
  exs <- mapM fileExist dirs
  return $ S.toList $ S.fromList $ map fst $ filter snd $ zip dirs exs
    
getXdgMenuPrefix :: IO String
getXdgMenuPrefix = do
  mPf <- lookupEnv "XDG_MENU_PREFIX"
  return $ fromMaybe "gnome-" mPf

getXdgDataDirs :: IO [String]
getXdgDataDirs = do
  mPf <- lookupEnv "XDG_DATA_DIRS"
  let dirs = maybe [] splitSearchPath mPf ++ ["/usr/local/share/", "/usr/share/"]
  existingDirs dirs

getXdgMenuFilename :: Maybe String -> IO FilePath
getXdgMenuFilename mMenuPrefix = do
  cd <- getXdgConfigDir
  pf <- case mMenuPrefix of
          Nothing -> getXdgMenuPrefix
          Just prefix -> return prefix
  return $ cd ++ "/menus/" ++ pf ++ "applications.menu"

-- | XDG Menu, cf. "Desktop Menu Specification".
data XdgMenu = XdgMenu {
  xmAppDir               :: Maybe String,
  xmDefaultAppDirs       :: Bool, -- Use $XDG_DATA_DIRS/applications
  xmDirectoryDir         :: Maybe String,
  xmDefaultDirectoryDirs :: Bool, -- Use $XDG_DATA_DIRS/desktop-directories
  xmLegacyDirs           :: [String],
  xmName                 :: String,
  xmDirectory            :: String,
  xmOnlyUnallocated      :: Bool,
  xmDeleted              :: Bool,
  xmInclude              :: Maybe DesktopEntryCondition,
  xmExclude              :: Maybe DesktopEntryCondition,
  xmSubmenus             :: [XdgMenu]}
  deriving(Show)

-- | Return a list of all available desktop entries for a given xdg menu.
getDesktopEntries :: [String] -- ^ Preferred languages
                  -> XdgMenu
                  -> IO [DesktopEntry]
getDesktopEntries langs menu = do
  defEntries <- if xmDefaultAppDirs menu
    then do dataDirs <- getXdgDataDirs
            print dataDirs
            liftM concat $ mapM (listDesktopEntries . (++ "/applications")) dataDirs
    else return []
  putStrLn $ "DesktopEntries in " ++ xmName menu
  -- print defEntries
  return $ sortBy (\de1 de2 -> compare (deName langs de1) (deName langs de2)) defEntries

-- | Parse menu.
parseMenu :: Element -> Maybe XdgMenu
parseMenu elt =
  let appDir = getChildData "AppDir" elt
      defaultAppDirs = case getChildData "DefaultAppDirs" elt of
                         Nothing -> False
                         Just _  -> True
      directoryDir = getChildData "DirectoryDir" elt
      defaultDirectoryDirs = case getChildData "DefaultDirectoryDirs" elt of
                               Nothing -> False
                               Just _  -> True
      name = fromMaybe "Name?" $ getChildData "Name" elt
      dir = fromMaybe "Dir?" $ getChildData "Directory" elt
      onlyUnallocated = False   -- FIXME
      deleted = False   -- FIXME
      include = parseConditions "Include" elt
      exclude = parseConditions "Exclude" elt
      subMenus = fromMaybe [] $ mapChildren "Menu" elt parseMenu
  in Just XdgMenu {xmAppDir               = appDir,
                   xmDefaultAppDirs       = defaultAppDirs,
                   xmDirectoryDir         = directoryDir,
                   xmDefaultDirectoryDirs = defaultDirectoryDirs,
                   xmLegacyDirs           = [],
                   xmName                 = name,
                   xmDirectory            = dir,
                   xmOnlyUnallocated      = onlyUnallocated,
                   xmDeleted              = deleted,
                   xmInclude              = include,
                   xmExclude              = exclude,
                   xmSubmenus             = subMenus}

-- | Parse Desktop Entry conditions for Include/Exclude clauses.
parseConditions :: String -> Element -> Maybe DesktopEntryCondition
parseConditions key elt = case findChild (unqual key) elt of
  Nothing -> Nothing
  Just inc -> doParseConditions (elChildren inc)
  where doParseConditions :: [Element] -> Maybe DesktopEntryCondition
        doParseConditions []   = Nothing
        doParseConditions [e]  = parseSingleItem e
        doParseConditions elts = Just $ Or $ catMaybes $ map parseSingleItem elts

        parseSingleItem e = case qName (elName e) of
          "Category" -> Just $ Category $ strContent e
          "Filename" -> Just $ Filename $ strContent e
          "And"      -> Just $ And $ catMaybes $ map parseSingleItem $ elChildren e
          "Or"       -> Just $ Or  $ catMaybes $ map parseSingleItem $ elChildren e
          "Not"      -> case parseSingleItem (head (elChildren e)) of
                          Nothing   -> Nothing
                          Just rule -> Just $ Not rule
          unknown    -> D.trace ("Ooopsi: " ++  unknown) Nothing

-- | Fetch menus and desktop entries and assemble the XDG menu.
buildXdgMenu :: Maybe String -> IO (Maybe XdgMenu)
buildXdgMenu mMenuPrefix = do
  filename <- getXdgMenuFilename mMenuPrefix
  putStrLn $ "Reading " ++ filename
  contents <- readFile filename
  case parseXMLDoc contents of
    Nothing -> do print "Parsing failed"
                  return Nothing
    Just element -> return $ parseMenu element

-- | Test
testXdgMenu :: IO ()
testXdgMenu = do
  m <- buildXdgMenu (Just "mate-")
  print $ m
  return ()                     -- 


