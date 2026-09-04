-----------------------------------------------------------------------------

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
module System.Environment.XDG.DesktopEntry
  ( DesktopEntry (..),
    deCommand,
    deComment,
    deHasCategory,
    deIcon,
    deName,
    deNoDisplay,
    deNotShowIn,
    deOnlyShowIn,
    getClassNames,
    getDirectoryEntriesDefault,
    getDirectoryEntry,
    getDirectoryEntryDefault,
    getXDGDataDirs,
    indexDesktopEntriesBy,
    indexDesktopEntriesByClassName,
    listDesktopEntries,
    readDesktopEntry,
  )
where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import qualified Data.ByteString as BS
import Data.Char
import Data.Either
import Data.Either.Combinators
import Data.List
import Data.Maybe
import qualified Data.MultiMap as MM
import Data.Text (unpack)
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Safe
import System.Directory
import System.FilePath.Posix
import Text.Printf
import Text.Read (readMaybe)

data DesktopEntryType = Application | Link | Directory
  deriving (Read, Show, Eq)

-- | Get all of the XDG data directories (both global and user).
getXDGDataDirs :: IO [FilePath]
getXDGDataDirs =
  liftM2 (:) (getXdgDirectory XdgData "") (getXdgDirectoryList XdgDataDirs)

-- | Desktop Entry. All attributes (key-value-pairs) are stored in an
-- association list.
data DesktopEntry = DesktopEntry
  { deType :: DesktopEntryType,
    -- | unqualified filename, e.g. "firefox.desktop"
    deFilename :: FilePath,
    -- | Key-value pairs
    deAttributes :: [(String, String)]
  }
  deriving (Read, Show, Eq)

-- | Determine whether the Category attribute of a desktop entry contains a
-- given value.
deHasCategory ::
  DesktopEntry ->
  String ->
  Bool
deHasCategory de cat =
  maybe False ((cat `elem`) . splitAtSemicolon) $
    lookup "Categories" (deAttributes de)

splitAtSemicolon :: String -> [String]
splitAtSemicolon = lines . map (\c -> if c == ';' then '\n' else c)

-- | Return the proper name of the desktop entry, depending on the list of
-- preferred languages.
deName ::
  -- | Preferred languages
  [String] ->
  DesktopEntry ->
  String
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

deLocalisedAtt ::
  -- | Preferred languages
  [String] ->
  DesktopEntry ->
  String ->
  Maybe String
deLocalisedAtt langs de att =
  let localeMatches =
        mapMaybe (\l -> lookup (att ++ "[" ++ l ++ "]") (deAttributes de)) langs
   in case localeMatches of
        [] -> lookup att $ deAttributes de
        (x : _) -> Just x

-- | Return the proper comment of the desktop entry, depending on the list of
-- preferred languages.
deComment ::
  -- | Preferred languages
  [String] ->
  DesktopEntry ->
  Maybe String
deComment langs de = deLocalisedAtt langs de "Comment"

-- | Return the command that should be executed when running this desktop entry.
deCommand :: DesktopEntry -> Maybe String
deCommand de =
  reverse . dropWhile (== ' ') . reverse . takeWhile (/= '%')
    <$> lookup "Exec" (deAttributes de)

-- | Return a list of all desktop entries in the given directory.
listDesktopEntries ::
  -- | The extension to use in the search
  String ->
  -- | The filepath at which to search
  FilePath ->
  IO [DesktopEntry]
listDesktopEntries extension dir = do
  let normalizedDir = normalise dir
  ex <- doesDirectoryExist normalizedDir
  if ex
    then do
      files <- map (normalizedDir </>) <$> listDirectory dir
      entries <-
        nub . rights
          <$> mapM readDesktopEntry (filter (extension `isSuffixOf`) files)
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
  (rightToMaybe =<<) <$> traverse readDesktopEntry (headMay exFiles)

-- | Get a desktop entry with a specific name from the default directory entry
-- locations.
getDirectoryEntryDefault :: String -> IO (Maybe DesktopEntry)
getDirectoryEntryDefault entry =
  getXDGDataDirs
    >>= flip getDirectoryEntry (printf "%s.desktop" entry) . fmap (</> "applications")

-- | Get all instances of 'DesktopEntry' for all desktop entry files that can be
-- found by looking in the directories specified by the XDG specification.
getDirectoryEntriesDefault :: IO [DesktopEntry]
getDirectoryEntriesDefault =
  getXDGDataDirs >>= foldM addDesktopEntries [] . fmap (</> "applications")
  where
    addDesktopEntries soFar directory =
      (soFar ++) <$> listDesktopEntries "desktop" directory

-- | Read a desktop entry from a file.
readDesktopEntry :: FilePath -> IO (Either String DesktopEntry)
readDesktopEntry filePath = runExceptT $ do
  contents <- liftIO $ unpack . decodeUtf8With lenientDecode <$> BS.readFile filePath
  groups <- except $ parseDesktopEntryGroups contents
  result <-
    maybe (throwE "Section [Desktop Entry] not found") pure $
      lookup "Desktop Entry" groups
  return
    DesktopEntry
      { deType = fromMaybe Application $ lookup "Type" result >>= readMaybe,
        deFilename = filePath,
        deAttributes = result
      }

-- | Parse the groups of a desktop entry file into association lists, in file
-- order. Keys keep their locale suffix (e.g. @Name[de]@), whitespace around
-- @=@ is ignored, and blank and @#@ comment lines are skipped.
parseDesktopEntryGroups :: String -> Either String [(String, [(String, String)])]
parseDesktopEntryGroups = go Nothing [] . zip [1 :: Int ..] . lines
  where
    go current done [] = Right $ reverse $ finish current done
    go current done ((lineNo, rawLine) : rest)
      | null line || "#" `isPrefixOf` line = go current done rest
      | "[" `isPrefixOf` line && "]" `isSuffixOf` line =
          go (Just (takeWhile (/= ']') $ drop 1 line, [])) (finish current done) rest
      | otherwise =
          case break (== '=') line of
            (rawKey, '=' : rawValue)
              | not (null key) ->
                  case current of
                    Nothing ->
                      Left $ printf "line %d: entry before any group header" lineNo
                    Just (name, entries) ->
                      go (Just (name, (key, trim rawValue) : entries)) done rest
              where
                key = trim rawKey
            _ -> Left $ printf "line %d: expected a group header or key=value" lineNo
      where
        line = trim rawLine
    finish Nothing done = done
    finish (Just (name, entries)) done = (name, reverse entries) : done
    trim = dropWhileEnd isSpace . dropWhile isSpace

-- | Construct a 'MM.Multimap' where each 'DesktopEntry' in the provided
-- foldable is indexed by the keys returned from the provided indexing function.
indexDesktopEntriesBy ::
  (Foldable t) =>
  (DesktopEntry -> [String]) ->
  t DesktopEntry ->
  MM.MultiMap String DesktopEntry
indexDesktopEntriesBy getIndices = foldl insertByIndices MM.empty
  where
    insertByIndices entriesMap entry =
      foldl insertForKey entriesMap $ getIndices entry
      where
        insertForKey innerMap key = MM.insert key entry innerMap

-- | Get all the text elements that could be interpreted as class names from a
-- 'DesktopEntry'.
getClassNames :: DesktopEntry -> [String]
getClassNames DesktopEntry {deAttributes = attributes, deFilename = filepath} =
  snd (splitExtensions $ snd $ splitFileName filepath)
    : catMaybes [lookup "StartupWMClass" attributes, lookup "Name" attributes]

-- | Construct a multimap where desktop entries are indexed by their class
-- names.
indexDesktopEntriesByClassName ::
  (Foldable t) => t DesktopEntry -> MM.MultiMap String DesktopEntry
indexDesktopEntriesByClassName = indexDesktopEntriesBy getClassNames
