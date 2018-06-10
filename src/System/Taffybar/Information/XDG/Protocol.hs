-----------------------------------------------------------------------------
-- |
-- Module      : System.Taffybar.Information.XDG.Protocol
-- Copyright   : 2017 Ulf Jasper
-- License     : BSD3-style (see LICENSE)
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
module System.Taffybar.Information.XDG.Protocol
  ( XDGMenu(..)
  , DesktopEntryCondition(..)
  , readXDGMenu
  , matchesCondition
  , getXDGDesktop
  , getDirectoryDirs
  , getApplicationEntries
  , getPreferredLanguages
  ) where

import           Control.Applicative
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Data.Char (toLower)
import           Data.List
import           Data.Maybe
import qualified Debug.Trace as D
import           GHC.IO.Encoding
import           Prelude
import           Safe (headMay)
import           System.Directory
import           System.Environment
import           System.FilePath.Posix
import           System.Taffybar.Information.XDG.DesktopEntry
import           System.Taffybar.Util
import           Text.XML.Light
import           Text.XML.Light.Helpers

-- Environment Variables

-- | Produce a list of config locations to search, starting with XDG_CONFIG_HOME
-- and XDG_CONFIG_DIRS, with fallback to /etc/xdg
getXDGConfigDirs :: IO [String]
getXDGConfigDirs = do
  mXdgConfigHome <- lookupEnv "XDG_CONFIG_HOME"
  xdgConfigDirs <- maybe [] splitSearchPath <$>
                   lookupEnv "XDG_CONFIG_DIRS"
  let xdgDirs = if null xdgConfigDirs
                then ["/etc/xdg/"]
                else map normalise xdgConfigDirs
  existingDirs $ case mXdgConfigHome of
                   Nothing -> xdgDirs
                   Just h  -> normalise h : xdgDirs

getXDGMenuPrefix :: IO (Maybe String)
getXDGMenuPrefix = lookupEnv "XDG_MENU_PREFIX"

-- | Find filename(s) of the application menu(s).
getXDGMenuFilenames
  :: Maybe String -- ^ Overrides the value of the environment variable
                  -- XDG_MENU_PREFIX. Specifies the prefix for the menu (e.g.
                  -- 'Just "mate-"').
  -> IO [FilePath]
getXDGMenuFilenames mMenuPrefix = do
  configDirs <- getXDGConfigDirs
  maybePrefix <- (mMenuPrefix <|>) <$> getXDGMenuPrefix
  let maybeAddDash t = if last t == '-' then t else t ++ "-"
      dashedPrefix = maybe "" maybeAddDash maybePrefix
  return $ map (</> "menus" </> dashedPrefix ++ "applications.menu") configDirs

-- | XDG Menu, cf. "Desktop Menu Specification".
data XDGMenu = XDGMenu
  { xmAppDir :: Maybe String
  , xmDefaultAppDirs :: Bool -- Use $XDG_DATA_DIRS/applications
  , xmDirectoryDir :: Maybe String
  , xmDefaultDirectoryDirs :: Bool -- Use $XDG_DATA_DIRS/desktop-directories
  , xmLegacyDirs :: [String]
  , xmName :: String
  , xmDirectory :: String
  , xmOnlyUnallocated :: Bool
  , xmDeleted :: Bool
  , xmInclude :: Maybe DesktopEntryCondition
  , xmExclude :: Maybe DesktopEntryCondition
  , xmSubmenus :: [XDGMenu]
  , xmLayout :: [XDGLayoutItem]
  } deriving (Show)

data XDGLayoutItem =
  XliFile String | XliSeparator | XliMenu String | XliMerge String
  deriving(Show)

-- | Return a list of all available desktop entries for a given xdg menu.
getApplicationEntries
  :: [String] -- ^ Preferred languages
  -> XDGMenu
  -> IO [DesktopEntry]
getApplicationEntries langs xm = do
  defEntries <- if xmDefaultAppDirs xm
    then do dataDirs <- getXDGDataDirs
            concat <$> mapM (listDesktopEntries ".desktop" .
                                                  (</> "applications")) dataDirs
    else return []
  return $ sortBy (\de1 de2 -> compare (map toLower (deName langs de1))
                               (map toLower (deName langs de2))) defEntries

-- | Parse menu.
parseMenu :: Element -> Maybe XDGMenu
parseMenu elt =
  let appDir = getChildData "AppDir" elt
      defaultAppDirs = isJust $ getChildData "DefaultAppDirs" elt
      directoryDir = getChildData "DirectoryDir" elt
      defaultDirectoryDirs = isJust $ getChildData "DefaultDirectoryDirs" elt
      name = fromMaybe "Name?" $ getChildData "Name" elt
      dir = fromMaybe "Dir?" $ getChildData "Directory" elt
      onlyUnallocated =
        case ( getChildData "OnlyUnallocated" elt
             , getChildData "NotOnlyUnallocated" elt) of
          (Nothing, Nothing) -> False -- ?!
          (Nothing, Just _) -> False
          (Just _, Nothing) -> True
          (Just _, Just _) -> False -- ?!
      deleted = False -- FIXME
      include = parseConditions "Include" elt
      exclude = parseConditions "Exclude" elt
      layout = parseLayout elt
      subMenus = fromMaybe [] $ mapChildren "Menu" elt parseMenu
  in Just
       XDGMenu
       { xmAppDir = appDir
       , xmDefaultAppDirs = defaultAppDirs
       , xmDirectoryDir = directoryDir
       , xmDefaultDirectoryDirs = defaultDirectoryDirs
       , xmLegacyDirs = []
       , xmName = name
       , xmDirectory = dir
       , xmOnlyUnallocated = onlyUnallocated
       , xmDeleted = deleted
       , xmInclude = include
       , xmExclude = exclude
       , xmSubmenus = subMenus
       , xmLayout = layout -- FIXME
       }

-- | Parse Desktop Entry conditions for Include/Exclude clauses.
parseConditions :: String -> Element -> Maybe DesktopEntryCondition
parseConditions key elt = case findChild (unqual key) elt of
  Nothing -> Nothing
  Just inc -> doParseConditions (elChildren inc)
  where doParseConditions :: [Element] -> Maybe DesktopEntryCondition
        doParseConditions []   = Nothing
        doParseConditions [e]  = parseSingleItem e
        doParseConditions elts = Just $ Or $ mapMaybe parseSingleItem elts

        parseSingleItem e = case qName (elName e) of
          "Category" -> Just $ Category $ strContent e
          "Filename" -> Just $ Filename $ strContent e
          "And"      -> Just $ And $ mapMaybe parseSingleItem
                          $ elChildren e
          "Or"       -> Just $ Or  $ mapMaybe parseSingleItem
                          $ elChildren e
          "Not"      -> case parseSingleItem (head (elChildren e)) of
                          Nothing   -> Nothing
                          Just rule -> Just $ Not rule
          unknown    -> D.trace ("Unknown Condition item: " ++  unknown) Nothing

-- | Combinable conditions for Include and Exclude statements.
data DesktopEntryCondition = Category String
                           | Filename String
                           | Not DesktopEntryCondition
                           | And [DesktopEntryCondition]
                           | Or [DesktopEntryCondition]
                           | All
                           | None
  deriving (Read, Show, Eq)

parseLayout :: Element -> [XDGLayoutItem]
parseLayout elt = case findChild (unqual "Layout") elt of
  Nothing -> []
  Just lt -> mapMaybe parseLayoutItem (elChildren lt)
  where parseLayoutItem :: Element -> Maybe XDGLayoutItem
        parseLayoutItem e = case qName (elName e) of
          "Separator" -> Just XliSeparator
          "Filename"  -> Just $ XliFile $ strContent e
          unknown     -> D.trace ("Unknown layout item: " ++ unknown) Nothing

-- | Determine whether a desktop entry fulfils a condition.
matchesCondition :: DesktopEntry -> DesktopEntryCondition -> Bool
matchesCondition de (Category cat) = deHasCategory de cat
matchesCondition de (Filename fn)  = fn == deFilename de
matchesCondition de (Not cond)     = not $ matchesCondition de cond
matchesCondition de (And conds)    = all (matchesCondition de) conds
matchesCondition de (Or conds)     = any (matchesCondition de) conds
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
    Just l -> return $
      let woEncoding      = takeWhile (/= '.') l
          (language, _cm) = span (/= '_') woEncoding
          (country, _m)   = span (/= '@') (if null _cm then "" else tail _cm)
          modifier        = if null _m then "" else tail _m
                       in dgl language country modifier
    where dgl "" "" "" = []
          dgl l  "" "" = [l]
          dgl l  c  "" = [l ++ "_" ++ c, l]
          dgl l  "" m  = [l ++ "@" ++ m, l]
          dgl l  c  m  = [l ++ "_" ++ c ++ "@" ++ m, l ++ "_" ++ c,
                          l ++ "@" ++ m]

-- | Determine current Desktop
getXDGDesktop :: IO String
getXDGDesktop = do
  mCurDt <- lookupEnv "XDG_CURRENT_DESKTOP"
  return $ fromMaybe "???" mCurDt

-- | Return desktop directories
getDirectoryDirs :: IO [FilePath]
getDirectoryDirs = do
  dataDirs <- getXDGDataDirs
  existingDirs $ map (</> "desktop-directories") dataDirs

-- | Fetch menus and desktop entries and assemble the XDG menu.
readXDGMenu :: Maybe String -> IO (Maybe (XDGMenu, [DesktopEntry]))
readXDGMenu mMenuPrefix = do
  setLocaleEncoding utf8
  filenames <- getXDGMenuFilenames mMenuPrefix
  headMay . catMaybes <$> traverse maybeMenu filenames

-- | Load and assemble the XDG menu from a specific file, if it exists.
maybeMenu :: FilePath -> IO (Maybe (XDGMenu, [DesktopEntry]))
maybeMenu filename =
  ifM (doesFileExist filename)
      (do
        putStrLn $ "Reading " ++ filename
        contents <- readFile filename
        langs <- getPreferredLanguages
        runMaybeT $ do
          m <- MaybeT $ return $ parseXMLDoc contents >>= parseMenu
          des <- lift $ getApplicationEntries langs m
          return (m, des))
      (do
        putStrLn $ "Error: menu file '" ++ filename ++ "' does not exist!"
        return Nothing)
