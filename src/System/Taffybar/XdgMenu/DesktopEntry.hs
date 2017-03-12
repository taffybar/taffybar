-----------------------------------------------------------------------------
-- |
-- Module      : System.Taffybar.XdgMenu.DesktopEntry
-- Copyright   : (c) Ulf Jasper
-- License     : GPL3 (see LICENSE)
--
-- Maintainer  : Ulf Jasper <ulf.jasper@web.de>
-- Stability   : unstable
-- Portability : unportable
--
-- Implementation of version 1.1 of the freedesktop Desktop Entry
-- specification, see
-- https://specifications.freedesktop.org/desktop-entry-spec/desktop-entry-spec-1.1.html.
-- See also 'XdgMenuWidget'.
--
-----------------------------------------------------------------------------
module System.Taffybar.XdgMenu.DesktopEntry (
  DesktopEntry(..),
  listDesktopEntries,
  deHasCategory,
  deLaunch,
  deName,
  deComment,
  deCommand)

where

import qualified Data.ConfigFile as CF
import Data.Maybe
import Data.List
import System.Directory
import System.Process
import Control.Monad.Error

-- | Desktop Entry.  All attributes (key-value-pairs) are stored in an
-- association list.
data DesktopEntry = DesktopEntry {
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
                         Just cats -> cat `elem` lines (map (\c -> if c == ';' then '\n' else c) cats)

-- | Return the proper name of the desktop entry, depending on the
-- list of preferred languages.
deName :: [String] -- ^ Preferred languages
       -> DesktopEntry
       -> String
deName langs de = fromMaybe (deFilename de) $ deLocalisedAtt langs de "Name" 

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

-- | Launch the given desktop entry.  Spawns a command in the
-- background.  FIXME: should check the dbus thing.  FIXME: are there
-- "field codes", i.e. %<char> things, that should be respected?
deLaunch :: DesktopEntry -> IO ()
deLaunch de = do
  case deCommand de of
    Nothing -> return ()
    Just cmd -> do putStrLn $ "Launching '" ++ cmd ++ "'"
                   spawnCommand cmd >> return ()

-- | Return a list of all desktop entries in the given directory.
listDesktopEntries :: FilePath -> IO [DesktopEntry]
listDesktopEntries dir = do
  files <-  getDirectoryContents dir
  mEntries <- mapM (readDesktopEntry . ((dir ++ "/") ++)) $ filter (".desktop" `isSuffixOf`) files
  return $ catMaybes mEntries

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
            Right r -> return $ Just (DesktopEntry f r False)

-- | Test          
testDesktopEntry :: IO ()
testDesktopEntry = do
  print =<< readDesktopEntry "/usr/share/applications/taffybar.desktop"

