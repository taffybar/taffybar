-----------------------------------------------------------------------------
-- |
-- Module      : System.Environment.XDG.DesktopEntry
-- Copyright   : 2019 Ivan Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan Malison
-- Stability   : unstable
-- Portability : unportable
--
-- Implementation of version 1.2 of the freedesktop "Desktop Entry
-- specification", see
-- https://specifications.freedesktop.org/desktop-entry-spec/desktop-entry-spec-1.2.html.
-----------------------------------------------------------------------------

module System.Environment.XDG.DesktopEntry
  ( DesktopEntry(..)
  , deCommand
  , deComment
  , deHasCategory
  , deIcon
  , deName
  , deNoDisplay
  , deNotShowIn
  , deOnlyShowIn
  , getClassNames
  , getDirectoryEntriesDefault
  , getDirectoryEntry
  , getDirectoryEntryDefault
  , getXDGDataDirs
  , indexDesktopEntriesBy
  , indexDesktopEntriesByClassName
  , listDesktopEntries
  , readDesktopEntry
  ) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except
import           Data.Bifunctor (bimap)
import           Data.Char
import qualified Data.Ini as Ini
import           Data.Either
import           Data.Either.Combinators
import qualified Data.HashMap.Strict as HM
import qualified Data.MultiMap as MM
import           Data.List
import           Data.Maybe
import           Data.Text (pack, unpack)
import           Safe
import           System.Directory
import           System.FilePath.Posix
import           Text.Printf
import           Text.Read (readMaybe)

data DesktopEntryType = Application | Link | Directory
  deriving (Read, Show, Eq)

-- | Get all of the XDG data directories (both global and user).
getXDGDataDirs :: IO [FilePath]
getXDGDataDirs =
  liftM2 (:) (getXdgDirectory XdgData "") (getXdgDirectoryList XdgDataDirs)

-- | Desktop Entry. All attributes (key-value-pairs) are stored in an
-- association list.
data DesktopEntry = DesktopEntry
  { deType :: DesktopEntryType
  , deFilename :: FilePath -- ^ unqualified filename, e.g. "firefox.desktop"
  , deAttributes :: [(String, String)] -- ^ Key-value pairs
  } deriving (Read, Show, Eq)

-- | Determine whether the Category attribute of a desktop entry contains a
-- given value.
deHasCategory
  :: DesktopEntry
  -> String
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
  in case localeMatches of
       [] -> lookup att $ deAttributes de
       (x:_) -> Just x

-- | Return the proper comment of the desktop entry, depending on the list of
-- preferred languages.
deComment :: [String] -- ^ Preferred languages
          -> DesktopEntry
          -> Maybe String
deComment langs de = deLocalisedAtt langs de "Comment"

-- | Return the command that should be executed when running this desktop entry.
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
    files <- map (normalizedDir </>) <$> listDirectory dir
    entries <-
      (nub . rights) <$>
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
  exFiles <- filterM doesFileExist $ map ((</> name) . normalise) dirs
  join . (fmap rightToMaybe) <$> traverse readDesktopEntry (headMay exFiles)

-- | Get a desktop entry with a specific name from the default directory entry
-- locations.
getDirectoryEntryDefault :: String -> IO (Maybe DesktopEntry)
getDirectoryEntryDefault entry =
  fmap (</> "applications") <$> getXDGDataDirs >>=
  flip getDirectoryEntry (printf "%s.desktop" entry)

-- | Get all instances of 'DesktopEntry' for all desktop entry files that can be
-- found by looking in the directories specified by the XDG specification.
getDirectoryEntriesDefault :: IO [DesktopEntry]
getDirectoryEntriesDefault =
  fmap (</> "applications") <$> getXDGDataDirs >>= foldM addDesktopEntries []
  where addDesktopEntries soFar directory =
          (soFar ++) <$> listDesktopEntries "desktop" directory

-- | Read a desktop entry from a file.
readDesktopEntry :: FilePath -> IO (Either String DesktopEntry)
readDesktopEntry filePath = runExceptT $ do
  -- let foo1 = join . fmap except . liftIO $ Ini.readIniFile filePath
  -- let bar :: ExceptT String IO (HM.HashMap Text [(Text, Text)]) = map Ini.iniSections . liftIO $ Ini.readIniFile filePath
  -- sections <- fmap Ini.iniSections . join . fmap except . liftIO $ Ini.readIniFile filePath
  sections <- liftIO (Ini.readIniFile filePath) >>= fmap Ini.iniSections . except
  result <- maybe (throwE "Section [Desktop Entry] not found") (pure . fmap (bimap unpack unpack)) $
              HM.lookup (pack "Desktop Entry") sections
  return DesktopEntry
         { deType = fromMaybe Application $ lookup "Type" result >>= readMaybe
         , deFilename = filePath
         , deAttributes = result
         }

-- | Construct a 'MM.Multimap' where each 'DesktopEntry' in the provided
-- foldable is indexed by the keys returned from the provided indexing function.
indexDesktopEntriesBy ::
  Foldable t => (DesktopEntry -> [String]) ->
  t DesktopEntry -> MM.MultiMap String DesktopEntry
indexDesktopEntriesBy getIndices = foldl insertByIndices MM.empty
  where
    insertByIndices entriesMap entry =
      foldl insertForKey entriesMap $ getIndices entry
        where insertForKey innerMap key = MM.insert key entry innerMap

-- | Get all the text elements that could be interpreted as class names from a
-- 'DesktopEntry'.
getClassNames :: DesktopEntry -> [String]
getClassNames DesktopEntry { deAttributes = attributes, deFilename = filepath } =
  (snd $ splitExtensions $ snd $ splitFileName filepath) :
  catMaybes [lookup "StartupWMClass" attributes, lookup "Name" attributes]

-- | Construct a multimap where desktop entries are indexed by their class
-- names.
indexDesktopEntriesByClassName
  :: Foldable t => t DesktopEntry -> MM.MultiMap String DesktopEntry
indexDesktopEntriesByClassName = indexDesktopEntriesBy getClassNames
