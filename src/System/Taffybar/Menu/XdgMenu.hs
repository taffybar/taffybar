-----------------------------------------------------------------------------
-- |
-- Module      : System.Taffybar.Menu.XdgMenu
-- Copyright   : 2017 Ulf Jasper
-- License     : GPLv3 (see file COPYING)
--
-- Maintainer  : Ulf Jasper <ulf.jasper@web.de>
-- Stability   : unstable
-- Portability : unportable
--
-- Implementation of version 1.1 of the XDG "Desktop Menu
-- Specification", see
-- https://specifications.freedesktop.org/menu-spec/menu-spec-1.1.html
---- specification, see
-- See also 'MenuWidget'.
--
-----------------------------------------------------------------------------
module System.Taffybar.Menu.XdgMenu (
  XdgMenu(..),
  DesktopEntryCondition(..),
  readXdgMenu,
  matchesCondition,
  getXdgDesktop,
  getDirectoryDirs,
  getApplicationEntries,
  getPreferredLanguages)
where

import GHC.IO.Encoding
import Control.Monad
import Data.Char (toLower)
import Data.List
import Data.Maybe
import qualified Data.Set as S
import qualified Debug.Trace as D
import System.Directory
import System.Environment
import System.FilePath.Posix
import System.Posix.Files
import System.Taffybar.Menu.DesktopEntry
import Text.XML.Light
import Text.XML.Light.Helpers


-- Environment Variables
getXdgConfigDir :: IO String
getXdgConfigDir = do
  ch <- lookupEnv "XDG_CONFIG_HOME"
  cd <- lookupEnv "XDG_CONFIG_DIRS"
  let dirs = catMaybes [ch]
             ++ maybe [] splitSearchPath cd
  exDirs <- existingDirs dirs
  return $ if null exDirs
    then "/etc/xdg/"
    else normalise $ head exDirs

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
  mDh <- lookupEnv "XDG_DATA_HOME"
  dh <- case mDh of
          Nothing -> do h <- getHomeDirectory
                        return $ h </> ".local" </> "share"
          Just d -> return d
  mPf <- lookupEnv "XDG_DATA_DIRS"
  let dirs = maybe [] (map normalise . splitSearchPath) mPf ++ ["/usr/local/share", "/usr/share"]
  return . nubBy equalFilePath =<< existingDirs (dh:dirs)

getXdgMenuFilename :: Maybe String -> IO FilePath
getXdgMenuFilename mMenuPrefix = do
  cd <- getXdgConfigDir
  pf <- case mMenuPrefix of
          Nothing -> getXdgMenuPrefix
          Just prefix -> return prefix
  return $ cd </> "menus" </> pf ++ "applications.menu"

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
getApplicationEntries :: [String] -- ^ Preferred languages
                      -> XdgMenu
                      -> IO [DesktopEntry]
getApplicationEntries langs xm = do
  defEntries <- if xmDefaultAppDirs xm
    then do dataDirs <- getXdgDataDirs
            print dataDirs
            liftM concat $ mapM (listDesktopEntries ".desktop" .
                                  (</> "applications")) dataDirs
    else return []
  -- print xm
  -- print defEntries
  return $ sortBy (\de1 de2 -> compare (map toLower (deName langs de1))
                               (map toLower (deName langs de2))) defEntries

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
      onlyUnallocated = case (getChildData "OnlyUnallocated" elt,
                              getChildData "NotOnlyUnallocated" elt) of
                          (Nothing, Nothing) -> False -- ?!
                          (Nothing, Just _)  -> False
                          (Just _, Nothing)  -> True
                          (Just _, Just _)   -> False -- ?!
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

-- | Combinable conditions for Include and Exclude statements.
data DesktopEntryCondition = Category String
                           | Filename String
                           | Not DesktopEntryCondition
                           | And [DesktopEntryCondition]
                           | Or [DesktopEntryCondition]
                           | All
                           | None
  deriving (Read, Show, Eq)

-- | Determine whether a desktop entry fulfils a condition.
matchesCondition :: DesktopEntry -> DesktopEntryCondition -> Bool
matchesCondition de (Category cat) = deHasCategory de cat
matchesCondition de (Filename fn)  = fn == deFilename de
matchesCondition de (Not cond)     = not $ matchesCondition de cond
matchesCondition de (And conds)    = and $ map (matchesCondition de) conds
matchesCondition de (Or conds)     = or $ map (matchesCondition de) conds
matchesCondition _  All            = True
matchesCondition _  None           = False

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

-- | Determine current Desktop
getXdgDesktop :: IO String
getXdgDesktop = do
  mCurDt <- lookupEnv "XDG_CURRENT_DESKTOP"
  return $ fromMaybe "???" mCurDt

getDirectoryDirs :: IO [FilePath]
getDirectoryDirs = do
  dataDirs <- getXdgDataDirs
  existingDirs $ map (</> "desktop-directories") dataDirs

doGetPreferredLanguages :: String -- ^ locale lang
                        -> [String]
doGetPreferredLanguages llang =
  let woEncoding      = takeWhile (/= '.') llang
      (language, _cm) = span (/= '_') woEncoding
      (country, _m)   = span (/= '@') (if null _cm then "" else tail _cm)
      modifier        = if null _m then "" else tail _m

  in dgl language country modifier
  where dgl "" "" "" = []
        dgl l  "" "" = [l]
        dgl l  c  "" = [l ++ "_" ++ c, l]
        dgl l  "" m  = [l ++ "@" ++ m, l]
        dgl l  c  m  = [l ++ "_" ++ c ++ "@" ++ m, l ++ "_" ++ c, l ++ "@" ++ m]

  -- | Fetch menus and desktop entries and assemble the XDG menu.
readXdgMenu :: Maybe String -> IO (Maybe (XdgMenu, [DesktopEntry]))
readXdgMenu mMenuPrefix = do
  setLocaleEncoding utf8
  filename <- getXdgMenuFilename mMenuPrefix
  putStrLn $ "Reading " ++ filename
  contents <- readFile filename
  langs <- getPreferredLanguages
  case parseXMLDoc contents of
    Nothing      -> do print "Parsing XDG menu failed"
                       return Nothing
    Just element -> do case parseMenu element of
                         Nothing -> return Nothing
                         Just m -> do des <- getApplicationEntries langs m
                                      return $ Just (m, des)

-- -- | Test
-- testXdgMenu :: IO ()
-- testXdgMenu = do
--   m <- buildFinalMenu (Just "mate-")
--   print $ m
--   return ()

