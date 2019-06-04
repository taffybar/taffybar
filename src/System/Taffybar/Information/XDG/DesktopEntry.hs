-----------------------------------------------------------------------------
-- |
-- Module      : System.Taffybar.Information.XDG.DesktopEntry
-- Copyright   : 2017 Ulf Jasper
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ulf Jasper <ulf.jasper@web.de>
-- Stability   : unstable
-- Portability : unportable
--
-- Implementation of version 1.1 of the freedesktop "Desktop Entry
-- specification", see
-- https://specifications.freedesktop.org/desktop-entry-spec/desktop-entry-spec-1.2.html.
-----------------------------------------------------------------------------

module System.Taffybar.Information.XDG.DesktopEntry
  ( DesktopEntry(..)
  , deCommand
  , deComment
  , deHasCategory
  , deIcon
  , deName
  , deNoDisplay
  , deNotShowIn
  , deOnlyShowIn
  , existingDirs
  , getDefaultConfigHome
  , getDefaultDataHome
  , getDirectoryEntriesDefault
  , getDirectoryEntry
  , getDirectoryEntryDefault
  , getXDGDataDirs
  , listDesktopEntries
  ) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except
import           Data.Char
import qualified Data.ConfigFile as CF
import           Data.List
import           Data.Maybe
import           Debug.Trace
import           System.Directory
import           System.Environment
import           System.FilePath.Posix
import           System.Log.Logger
import           System.Posix.Files
import           Text.Printf
import           Text.Read (readMaybe)

logHere = logM "System.Taffybar.Information.XDG.DesktopEntry"

data DesktopEntryType = Application | Link | Directory
  deriving (Read, Show, Eq)

existingDirs :: [FilePath] -> IO [FilePath]
existingDirs  dirs = do
  exs <- mapM fileExist dirs
  let exDirs = nub $ map fst $ filter snd $ zip dirs exs
  mapM_ (putStrLn . ("Directory does not exist: " ++)) $ dirs \\ exDirs
  return exDirs

getDefaultConfigHome :: IO FilePath
getDefaultConfigHome = do
  h <- getHomeDirectory
  return $ h </> ".config"

getDefaultDataHome :: IO FilePath
getDefaultDataHome = do
  h <- getHomeDirectory
  return $ h </> ".local" </> "share"


-- XXX: We really ought to use
-- https://hackage.haskell.org/package/directory-1.3.2.2/docs/System-Directory.html#v:getXdgDirectory
getXDGDataDirs :: IO [FilePath]
getXDGDataDirs = do
  dataHome <- lookupEnv "XDG_DATA_HOME" >>= maybe getDefaultDataHome return
  dataDirs <- map normalise . splitSearchPath . fromMaybe "" <$>
              lookupEnv "XDG_DATA_DIRS"
  nubBy equalFilePath <$>
        existingDirs (  dataHome:dataDirs
                     ++ ["/usr/local/share", "/usr/share"]
                     )

-- | Desktop Entry. All attributes (key-value-pairs) are stored in an
-- association list.
data DesktopEntry = DesktopEntry
  { deType :: DesktopEntryType
  , deFilename :: FilePath -- ^ unqualified filename, e.g. "taffybar.desktop"
  , deAttributes :: [(String, String)] -- ^ Key-value pairs
  } deriving (Read, Show, Eq)

-- | Determine whether the Category attribute of a desktop entry contains a
-- given value.
deHasCategory
  :: DesktopEntry -- ^ desktop entry
  -> String -- ^ category to be checked
  -> Bool
deHasCategory de cat =
  maybe False ((cat `elem`) . splitAtSemicolon) $
        lookup "Categories" (deAttributes de)

splitAtSemicolon :: String -> [String]
splitAtSemicolon = lines . map (\c -> if c == ';' then '\n' else c)

-- | Return the proper name of the desktop entry, depending on the list of
-- preferred languages.
deName
  :: [String] -- ^ Preferred languages
  -> DesktopEntry
  -> String
deName langs de = fromMaybe (deFilename de) $ deLocalisedAtt langs de "Name"

-- | Return the categories in which the entry shall be shown
deOnlyShowIn :: DesktopEntry -> [String]
deOnlyShowIn = maybe [] splitAtSemicolon . deAtt "OnlyShowIn"

-- | Return the categories in which the entry shall not be shown
deNotShowIn :: DesktopEntry -> [String]
deNotShowIn = maybe [] splitAtSemicolon . deAtt "NotShowIn"

-- | Return the value of the given attribute key
deAtt :: String -> DesktopEntry -> Maybe String
deAtt att = lookup att . deAttributes

-- | Return the Icon attribute
deIcon :: DesktopEntry -> Maybe String
deIcon = deAtt "Icon"

-- | Return True if the entry must not be displayed
deNoDisplay :: DesktopEntry -> Bool
deNoDisplay de = maybe False (("true" ==) . map toLower) $ deAtt "NoDisplay" de

deLocalisedAtt
  :: [String] -- ^ Preferred languages
  -> DesktopEntry
  -> String
  -> Maybe String
deLocalisedAtt langs de att =
  let localeMatches =
        mapMaybe (\l -> lookup (att ++ "[" ++ l ++ "]") (deAttributes de)) langs
  in if null localeMatches
       then lookup att $ deAttributes de
       else Just $ head localeMatches

-- | Return the proper comment of the desktop entry, depending on the list of
-- preferred languages.
deComment :: [String] -- ^ Preferred languages
          -> DesktopEntry
          -> Maybe String
deComment langs de = deLocalisedAtt langs de "Comment"

-- | Return the command defined by the given desktop entry.
-- TODO: should check  the dbus thing.
-- TODO: are there "field codes", i.e. %<char> things, that
deCommand :: DesktopEntry -> Maybe String
deCommand de =
  reverse . dropWhile (== ' ') . reverse . takeWhile (/= '%') <$>
  lookup "Exec" (deAttributes de)

-- | Return a list of all desktop entries in the given directory.
listDesktopEntries
  :: String -- ^ The extension to use in the search
  -> FilePath -- ^ The filepath at which to search
  -> IO [DesktopEntry]
listDesktopEntries extension dir = do
  let normalizedDir = normalise dir
  ex <- doesDirectoryExist normalizedDir
  if ex
  then do
    files <-
      map (normalizedDir </>) . filter (\v -> v /= "." && v /= "..") <$>
      getDirectoryContents dir
    entries <-
      (nub . catMaybes) <$>
      mapM readDesktopEntry (filter (extension `isSuffixOf`) files)
    subDirs <- filterM doesDirectoryExist files
    subEntries <- concat <$> mapM (listDesktopEntries extension) subDirs
    return $ entries ++ subEntries
  else return []

-- XXX: This function doesn't recurse, but `listDesktopEntries` does. Why?
-- Shouldn't they really share logic...
-- | Retrieve a desktop entry with a specific name.
getDirectoryEntry :: [FilePath] -> String -> IO (Maybe DesktopEntry)
getDirectoryEntry dirs name = do
  liftIO $ logHere DEBUG $ printf "Searching %s for %s" (show dirs) name
  exFiles <- filterM doesFileExist $ map ((</> name) . normalise) dirs
  if null exFiles
  then return Nothing
  else readDesktopEntry $ head exFiles

getDirectoryEntryDefault :: String -> IO (Maybe DesktopEntry)
getDirectoryEntryDefault entry =
  fmap (</> "applications") <$> getXDGDataDirs >>=
  flip getDirectoryEntry (printf "%s.desktop" entry)

getDirectoryEntriesDefault :: IO [DesktopEntry]
getDirectoryEntriesDefault =
  fmap (</> "applications") <$> getXDGDataDirs >>= foldM addDirectories []
  where addDirectories soFar directory =
          (soFar ++) <$> listDesktopEntries "desktop" directory

-- | Main section of a desktop entry file.
sectionMain :: String
sectionMain = "Desktop Entry"

-- | Read a desktop entry from a file.
readDesktopEntry :: FilePath -> IO (Maybe DesktopEntry)
readDesktopEntry filePath = doReadDesktopEntry >>= either logWarning (return . Just)
  where
    logWarning error =
      logHere  WARNING (printf "Failed to parse desktop entry at %s" filePath) >>
               return Nothing
    doReadDesktopEntry = runExceptT $ do
         result <- (join $ liftIO $ CF.readfile CF.emptyCP filePath) >>=
                   flip CF.items sectionMain
         return DesktopEntry
                { deType = fromMaybe Application $ lookup "Type" result >>= readMaybe
                , deFilename = filePath
                , deAttributes = result
                }
