import Distribution.Simple
import Distribution.Simple.Setup (ConfigFlags(..), fromFlagOrDefault)
import Distribution.Verbosity (Verbosity, normal)
import Distribution.Simple.Utils (die')
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)
import Control.Exception (try, IOException)
import Data.List (intercalate)
import Data.Maybe (isJust)
import System.Environment (lookupEnv)

main :: IO ()
main =
  defaultMainWithHooks simpleUserHooks
    { confHook = \pkg flags -> do
        preflightPkgConfig (fromFlagOrDefault normal (configVerbosity flags))
        confHook simpleUserHooks pkg flags
    }

preflightPkgConfig :: Verbosity -> IO ()
preflightPkgConfig verbosity = do
  -- Cabal's pkg-config failure mode can be a bit opaque if you aren't already
  -- in the Nix/dev environment. Fail early with a message that points to the
  -- common fixes.
  -- Only hard-require GTK itself. The standalone executable also uses
  -- gtk-layer-shell on Wayland, but we don't want to prevent building the
  -- library when that optional dependency is unavailable.
  let required = ["gtk+-3.0"]
  present <- mapM (\p -> (\ok -> (p, ok)) <$> pkgConfigExists p) required
  let missing = [p | (p, ok) <- present, not ok]
  if null missing
  then pure ()
  else do
    inDirenv <- isJust <$> lookupEnv "DIRENV_DIR"
    let nixHint =
          if inDirenv
          then "If you just changed Nix inputs, try: `direnv reload`"
          else intercalate " " [ "If you're using Nix, enter the dev shell:"
                               , "`nix develop` (or `direnv allow` + `direnv reload`)."
                               ]
        msg = unlines
          [ "Missing system dependencies required via pkg-config:"
          , "  " ++ intercalate ", " missing
          , ""
          , "This package needs `pkg-config` to find its C dependencies."
          , "If you are building the standalone executable for Wayland, you will also need: gtk-layer-shell-0"
          , ""
          , nixHint
          , "Otherwise, install the development packages for your distro (and pkg-config)."
          ]
    die' verbosity msg

pkgConfigExists :: String -> IO Bool
pkgConfigExists name = do
  let cmd = "pkg-config"
      args = ["--exists", name]
  e <- try (readProcessWithExitCode cmd args "") :: IO (Either IOException (ExitCode, String, String))
  pure $ case e of
    Right (ExitSuccess, _, _) -> True
    _ -> False
