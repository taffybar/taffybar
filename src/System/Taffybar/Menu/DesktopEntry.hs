{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      : System.Taffybar.Menu.DesktopEntry
-- Copyright   : 2017 Ulf Jasper
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ulf Jasper <ulf.jasper@web.de>
-- Stability   : unstable
-- Portability : unportable
--
-- Implementation of version 1.1 of the freedesktop "Desktop Entry
-- specification", see
-- https://specifications.freedesktop.org/desktop-entry-spec/desktop-entry-spec-1.1.html.
--
-- See also 'MenuWidget'.
-----------------------------------------------------------------------------

module System.Taffybar.Menu.DesktopEntry (
  DesktopEntry(..),
  listDesktopEntries,
  getDirectoryEntry,
  deHasCategory,
  deName,
  deOnlyShowIn,
  deNotShowIn,
  deComment,
  deCommand,
  deIcon,
  deNoDisplay)

where

import Control.Monad.Error
import Data.Char
import qualified Data.ConfigFile as CF
import Data.List
import Data.Maybe
import System.Directory
import System.FilePath.Posix

data DesktopEntryType = Application | Link | Directory
  deriving (Read, Show, Eq)

-- | Desktop Entry.  All attributes (key-value-pairs) are stored in an
-- association list.
data DesktopEntry = DesktopEntry {
  deType       :: DesktopEntryType,
  deFilename   :: FilePath, -- ^ unqualified filename, e.g. "taffybar.desktop"
  deAttributes :: [(String, String)], -- ^ Key-value pairs
  deAllocated  :: Bool -- ^ already contained in some menu?
  }
  deriving (Read, Show, Eq)

-- | Determine whether the Category attribute of a desktop entry
-- contains a given value.
deHasCategory :: DesktopEntry -- ^ desktop entry
              -> String -- ^ category to be checked
              -> Bool
deHasCategory de cat = case lookup "Categories" (deAttributes de) of
                         Nothing -> False
                         Just cats -> cat `elem` splitAtSemicolon cats

splitAtSemicolon :: String -> [String]
splitAtSemicolon = lines . (map (\c -> if c == ';' then '\n' else c))

-- | Return the proper name of the desktop entry, depending on the
-- list of preferred languages.
deName :: [String] -- ^ Preferred languages
       -> DesktopEntry
       -> String
deName langs de = fromMaybe (deFilename de) $ deLocalisedAtt langs de "Name" 

-- | Return the categories in which the entry shall be shown
deOnlyShowIn :: DesktopEntry -> [String]
deOnlyShowIn = maybe [] (splitAtSemicolon) . deAtt "OnlyShowIn" 

-- | Return the categories in which the entry shall not be shown
deNotShowIn :: DesktopEntry -> [String]
deNotShowIn = maybe [] (splitAtSemicolon) . deAtt "NotShowIn" 

-- | Return the value of the given attribute key
deAtt :: String -> DesktopEntry -> Maybe String
deAtt att = lookup att . deAttributes

-- | Return the Icon attribute
deIcon :: DesktopEntry -> Maybe String
deIcon = deAtt "Icon"

-- | Return True if the entry must not be displayed
deNoDisplay :: DesktopEntry -> Bool
deNoDisplay de = maybe False (("true" ==) . (map toLower)) $ deAtt "NoDisplay" de

deLocalisedAtt :: [String] -- ^ Preferred languages
               -> DesktopEntry
               -> String
               -> Maybe String
deLocalisedAtt langs de att = 
  let localeMatches = catMaybes $ map (\l -> lookup (att ++ "[" ++ l ++ "]") (deAttributes de)) langs
  in if null localeMatches
     then lookup att $ deAttributes de
     else Just $ head localeMatches

-- | Return the proper comment of the desktop entry, depending on the
-- list of preferred languages.
deComment :: [String] -- ^ Preferred languages
          -> DesktopEntry
          -> Maybe String
deComment langs de = deLocalisedAtt langs de "Comment"

-- | Return the command defined by the given desktop entry.  FIXME:
-- should check the dbus thing.  FIXME: are there "field codes",
-- i.e. %<char> things, that should be respected?
deCommand :: DesktopEntry -> Maybe String
deCommand de = 
  case lookup "Exec" (deAttributes de) of
    Nothing -> Nothing
    Just cmd -> Just $ reverse $ dropWhile (== ' ') $ reverse $ takeWhile (/= '%') cmd

-- | Return a list of all desktop entries in the given directory.
listDesktopEntries :: String -> FilePath -> IO [DesktopEntry]
listDesktopEntries extension dir = do
  let ndir = normalise dir
  putStrLn $ "Listing desktop entries in " ++ ndir
  ex <- doesDirectoryExist ndir
  if ex
    then do files <- mapM (return . (ndir </>))
                     =<< return . filter (/= ".")
                     =<< return . filter (/= "..")
                     =<< getDirectoryContents dir
            mEntries <- mapM (readDesktopEntry) $ filter (extension `isSuffixOf`) files
            let entries = nub $ catMaybes mEntries
            subDirs <- filterM doesDirectoryExist files
            subEntries <- return . concat =<< mapM (listDesktopEntries extension) subDirs 
            return $ entries ++ subEntries
    else do putStrLn $ "Does not exist: " ++ ndir
            return []

-- | Return a list of all desktop entries in the given directory.
getDirectoryEntry :: String -> [FilePath] -> IO (Maybe DesktopEntry)
getDirectoryEntry name dirs = do
  exFiles <- filterM doesFileExist $ map ((</> name) . normalise) dirs
  if null exFiles
  then return Nothing
  else readDesktopEntry $ head exFiles

-- | Main section of a desktop entry file.
sectionMain :: String
sectionMain = "Desktop Entry"

-- | Read a desktop entry from a file.
readDesktopEntry :: FilePath -> IO (Maybe DesktopEntry)
readDesktopEntry fp = do
  ex <- doesFileExist fp
  if ex
    then doReadDesktopEntry fp
    else do putStrLn $ "File does not exist: '" ++ fp ++ "'"
            return Nothing

  where doReadDesktopEntry :: FilePath -> IO (Maybe DesktopEntry)
        doReadDesktopEntry f = do
          eResult <- runErrorT $ do
            cp <- join $ liftIO $ CF.readfile CF.emptyCP f
            items <- CF.items cp sectionMain
            return items

          case eResult of
            Left _ -> return Nothing
            Right r -> return $ Just $ DesktopEntry
                       {deType       = maybe Application read (lookup "Type" r),
                        deFilename   = f,
                        deAttributes = r,
                        deAllocated  = False}

-- -- | Test
-- testDesktopEntry :: IO ()
-- testDesktopEntry = do
--   print =<< readDesktopEntry "/usr/share/applications/taffybar.desktop"

