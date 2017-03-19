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
  FinalMenu(..),
  FinalEntry(..),
  buildFinalMenu,
  getApplicationEntries)
where

import Control.Monad
import Data.List
import Data.Char (toLower)
import Data.Maybe
import System.Taffybar.XdgMenu.DesktopEntry
import System.Taffybar.XdgMenu.DesktopEntryCondition
import System.Environment
import System.Directory
import System.FilePath.Posix
import Text.XML.Light
import Text.XML.Light.Helpers
import System.Posix.Files
import qualified Data.Set as S
import qualified Debug.Trace as D
import GHC.IO.Encoding


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
  let dirs = maybe [] (map normalise . splitSearchPath) mPf ++ ["/usr/local/share/", "/usr/share/"]
  return . nub =<< existingDirs (dh:dirs)

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

-- | Return a list of all available desktop entries for a given xdg menu.
getDirectoryEntries :: [String] -- ^ Preferred languages
                    -> XdgMenu
                    -> IO [DesktopEntry]
getDirectoryEntries langs menu = do
  if xmDefaultDirectoryDirs menu
    then do dataDirs <- getXdgDataDirs
            liftM concat $ mapM (listDesktopEntries ".directory" .
                                  (</> "desktop-diretories")) dataDirs
    else return []

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


data FinalEntry = FinalEntry {
  feName           :: String,
  feComment        :: String,
  feCommand        :: String,
  feIcon           :: Maybe String}
  deriving (Eq, Show)

data FinalMenu = FinalMenu {
  fmName           :: String,
  fmComment        :: String,
  fmIcon           :: Maybe String,
  fmSubmenus       :: [FinalMenu],
  fmEntries        :: [FinalEntry],
  fmOnlyUnallocated :: Bool}
  deriving (Show)

-- | Fetch menus and desktop entries and assemble the XDG menu.
buildFinalMenu :: Maybe String -> IO FinalMenu
buildFinalMenu mMenuPrefix = do
  setLocaleEncoding utf8
  filename <- getXdgMenuFilename mMenuPrefix
  dt <- getDesktop
  putStrLn $ "Reading " ++ filename
  contents <- readFile filename
  langs <- getPreferredLanguages
  dirDirs <- getDirectoryDirs
  case parseXMLDoc contents of
    Nothing      -> do print "Parsing failed"
                       return $ FinalMenu "???" "Parsing failed" Nothing [] [] False
    Just element -> do case parseMenu element of
                         Nothing -> return $ FinalMenu "???" "Parsing failed" Nothing [] [] False
                         Just m -> do des <- getApplicationEntries langs m
                                      -- print des
                                      (fm, ae) <- xdgToFinalMenu dt langs dirDirs des m
                                      -- print ae
                                      let fm' = fixOnlyUnallocated ae fm

                                      return fm'

xdgToFinalMenu :: String -> [String] -> [FilePath] -> [DesktopEntry] -> XdgMenu -> IO (FinalMenu, [FinalEntry])
xdgToFinalMenu desktop langs dirDirs des xm = do
  dirEntry <- getDirectoryEntry (xmDirectory xm) dirDirs
  mas <- mapM (xdgToFinalMenu desktop langs dirDirs des) (xmSubmenus xm)
  let (menus, subaes) = unzip mas
      menus' = sortBy (\fm1 fm2 -> compare (map toLower $ fmName fm1)
                                   (map toLower $ fmName fm2)) menus
      entries = map (xdgToFinalEntry langs) $
                -- hide NoDisplay
                filter (not . deNoDisplay) $
                -- onlyshowin
                filter (matchesOnlyShowIn desktop) $
                -- excludes
                filter (not . flip matchesCondition (fromMaybe None (xmExclude xm))) $
                -- includes
                filter (flip matchesCondition (fromMaybe None (xmInclude xm))) des
      onlyUnallocated = xmOnlyUnallocated xm
      aes = if onlyUnallocated then [] else entries ++ concat subaes
  let fm = FinalMenu {fmName = maybe (xmName xm) (deName langs) dirEntry,
                      fmComment = maybe "???" (fromMaybe "???" . deComment langs) dirEntry,
                      fmIcon = deIcon =<< dirEntry,
                      fmSubmenus = menus',
                      fmEntries = entries,
                      fmOnlyUnallocated = onlyUnallocated}
  -- print des
  return (fm, aes)

matchesOnlyShowIn :: String -> DesktopEntry -> Bool
matchesOnlyShowIn desktop de = matchesShowIn && notMatchesNotShowIn
  where matchesShowIn = case deOnlyShowIn de of
                          [] -> True
                          desktops -> desktop `elem` desktops
        notMatchesNotShowIn = case deNotShowIn de of
                                [] -> True
                                desktops -> not $ desktop `elem` desktops

xdgToFinalEntry langs de = FinalEntry {feName = name,
                                       feComment = comment,
                                       feCommand = cmd,
                                       feIcon = mIcon}
  where mc = case deCommand de of
               Nothing -> Nothing
               Just cmd -> Just $ "(" ++ cmd ++ ")"
        comment = fromMaybe "??" $ case deComment langs de of
                                     Nothing -> mc
                                     Just tt -> Just $ tt ++ maybe "" ("\n" ++) mc
        cmd = fromMaybe "FIXME" $ deCommand de
        name = deName langs de
        mIcon = deIcon de

fixOnlyUnallocated fes fm = fm {fmEntries = entries,
                                fmSubmenus = map (fixOnlyUnallocated fes) (fmSubmenus fm)}
  where entries = if (fmOnlyUnallocated fm)
                  then filter (not . (`elem` fes)) (fmEntries fm)
                  else fmEntries fm

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
getDesktop :: IO String
getDesktop = do
  mCurDt <- lookupEnv "XDG_CURRENT_DESKTOP"
  return $ fromMaybe "???" mCurDt

getDirectoryDirs :: IO [FilePath]
getDirectoryDirs = do
  dataDirs <- getXdgDataDirs
  existingDirs $ map (</> "desktop-directories") dataDirs

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

-- | Test
testXdgMenu :: IO ()
testXdgMenu = do
  m <- buildFinalMenu (Just "mate-")
  print $ m
  return ()                     -- 


