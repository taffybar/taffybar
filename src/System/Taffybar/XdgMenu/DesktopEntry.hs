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
  deName)

where

import System.Directory (doesFileExist)
import qualified Data.ConfigFile as CF
import Data.Maybe
import Data.List
import System.Directory
import System.Process
import Control.Monad.Error

-- | Desktop Entry.  All attributes (key-value-pairs) are stored in an
-- association list.
data DesktopEntry = DesktopEntry {
  deFilename :: FilePath, -- ^ unqualified filename, e.g. "taffybar.desktop"
  deAttributes :: [(String, String)] -- ^ Key-value pairs
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

-- | Return the proper name of the desktop entry, depending on the active locale.
deName :: DesktopEntry -> String
deName de = fromMaybe (deFilename de) $ lookup "Name" $ deAttributes de
  -- mDirs <- lookupEnv "LC_MESSAGES"

-- | Launch the given desktop entry.  Spawns a command in the
-- background.  FIXME: should check the dbus thing.
deLaunch :: DesktopEntry -> IO ()
deLaunch de = do
  case lookup "Exec" (deAttributes de) of
    Nothing -> return ()
    Just cmd -> do putStrLn $ "Launching '" ++ cmd ++ "'"
                   spawnCommand cmd >> return ()

-- | Return a list of all desktop entries in the given directory.
listDesktopEntries :: FilePath -> IO [DesktopEntry]
listDesktopEntries dir = do
  files <-  listDirectory dir
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
            Right r -> return $ Just (DesktopEntry f r)

-- | Test          
testDesktopEntry :: IO ()
testDesktopEntry = do
  print =<< readDesktopEntry "/usr/share/applications/taffybar.desktop"

