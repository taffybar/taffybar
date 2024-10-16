{-# LANGUAGE ViewPatterns #-}

module System.Taffybar.SpecUtil
  ( withMockCommand
  , writeScript
  , withEnv
  , prependPath
  ) where

import Control.Arrow (second)
import Control.Monad (guard, join)
import Data.List (uncons)
import System.FilePath (isRelative, takeFileName, (</>))
import UnliftIO.Directory (Permissions (..), findExecutable, getPermissions, setPermissions)
import UnliftIO.Environment (lookupEnv, setEnv, unsetEnv)
import UnliftIO.Temporary (withSystemTempDirectory)
import UnliftIO.Exception (bracket)

-- | Run the given 'IO' action with the @PATH@ environment variable
-- set up so that executing the given command name will run a
-- script.
withMockCommand
  :: FilePath -- ^ Name of command - should not contain slashes
  -> String -- ^ Contents of script
  -> IO a -- ^ Action to run with command available in search path
  -> IO a
withMockCommand name content action =
  withSystemTempDirectory "specutil" $ \dir -> do
    writeScript (dir </> takeFileName name) content
    withEnv [("PATH", prependPath dir)] action

-- | Write a text file, make it executable.
-- It ought to have a shebang line.
writeScript :: FilePath -> String -> IO ()
writeScript scriptFile content = do
  content' <- patchShebangs content
  writeFile scriptFile content'
  p <- getPermissions scriptFile
  setPermissions scriptFile (p { executable = True })

-- | Given the text of a shell script, this replaces any relative path
-- in the shebang with an absolute path, according to the current
-- environment's @PATH@ variable.
--
-- The only reason this exists is so that we can generate shell
-- scripts containing @#!/usr/bin/env bash@ and then be able to
-- execute them within a Nix build sandbox (which does not allow
-- @/usr/bin/env@).
patchShebangs :: String -> IO String
patchShebangs = patchShebangs' findExe
  where
    findExe = fmap join . traverse findExecutable . takeRelativeFileName

    takeRelativeFileName :: FilePath -> Maybe FilePath
    takeRelativeFileName fp = guard (isRelative fp) >> pure (takeFileName fp)

patchShebangs' :: Applicative m => (FilePath -> m (Maybe FilePath)) -> String -> m String
patchShebangs' replaceExe script = case parseInterpreter script of
  Just (interpreter, rest) -> do
    let unparse exe = "#! " ++ exe ++ rest
    maybe script unparse <$> replaceExe interpreter
  Nothing -> pure script

parseInterpreter :: String -> Maybe (String, String)
parseInterpreter (lines -> content) = do
  (header, rest) <- uncons content
  (interpreter, args) <- parseShebang header
  pure (interpreter, unlines (args:rest))

  where
    parseShebang :: String -> Maybe (String, String)
    parseShebang ('#':'!':(findInterpreter -> shebang)) =
      let catArgs args = unwords ("":args)
      in second catArgs <$> shebang
    parseShebang _ = Nothing

    findInterpreter = uncons . dropWhile ((== "env") . takeFileName) . words

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
