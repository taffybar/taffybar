module System.Taffybar.SpecUtil
  ( withMockCommand
  , writeScript
  , withEnv
  , prependPath
  ) where

import Control.Exception (assert, bracket)
import System.Directory (Permissions (..), getPermissions, setPermissions)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath (takeFileName, (</>))
import System.IO.Temp (withSystemTempDirectory)

-- | Run the given 'IO' action with the @PATH@ environment variable
-- set up so that executing the given command name will run a
-- script.
withMockCommand
  :: FilePath -- ^ Name of command - should not contain slashes
  -> String -- ^ Contents of script
  -> IO a -- ^ Action to run with command available in search path
  -> IO a
withMockCommand name content action = withSystemTempDirectory "specutil" $ \dir -> do
  writeScript (dir </> takeFileName name) content
  withEnv [("PATH", prependPath dir)] action

-- | Write a text file, make it executable.
-- It ought to have a shebang line.
writeScript :: FilePath -> String -> IO ()
writeScript scriptFile content = assert (take 2 content == "#!") $ do
  writeFile scriptFile content
  p <- getPermissions scriptFile
  setPermissions scriptFile (p { executable = True })

-- | Run an 'IO' action with the given environment variables set up
-- according to their current value. 'Nothing' denotes an unset
-- environment variable. After the 'IO' action completes, environment
-- variables are restored to their previous state.
withEnv :: [(String, Maybe String -> Maybe String)] -> IO a -> IO a
withEnv mods = bracket setup teardown . const
  where
    setup = mapM (uncurry changeEnv) mods
    teardown = mapM (uncurry putEnv) . reverse

    changeEnv name f = do
      old <- lookupEnv name
      putEnv name (f old)
      pure (name, old)

    putEnv :: String -> Maybe String -> IO ()
    putEnv name = maybe (unsetEnv name) (setEnv name)

-- | Use this as a modifier function argument of 'withEnv' to ensure
-- that the given directory is prepended to a search path variable.
prependPath :: FilePath -> Maybe String -> Maybe String
prependPath p = Just . (++ ":/usr/bin") . (p ++) . maybe "" (":" ++)
