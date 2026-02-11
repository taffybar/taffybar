module System.Taffybar.AppearanceSpec (spec) where

import Codec.Picture qualified as JP
import Control.Monad (when)
import Data.ByteString.Lazy qualified as BL
import System.Directory (doesFileExist, findExecutable, makeAbsolute)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Process.Typed
  ( inherit,
    proc,
    setStderr,
    setStdout,
    stopProcess,
    waitExitCode,
    withProcessTerm,
  )
import System.Taffybar.Test.DBusSpec (withTestDBus)
import System.Taffybar.Test.UtilSpec (withEnv, withSetEnv)
import System.Taffybar.Test.XvfbSpec (setDefaultDisplay_, withXvfb)
import System.Timeout (timeout)
import Test.Hspec
import UnliftIO.Directory (createDirectoryIfMissing)
import UnliftIO.Environment (lookupEnv)
import UnliftIO.Temporary (withSystemTempDirectory)

spec :: Spec
spec = aroundAll withIntegrationEnv $ do
  it "renders a bar under an EWMH window manager" $ \env -> do
    goldenFile <- makeAbsolute "test/data/appearance-ewmh-bar.png"
    actualPng <- renderBarScreenshot env

    shouldUpdate <- lookupEnv "TAFFYBAR_UPDATE_GOLDENS"
    case shouldUpdate of
      Just _ -> do
        BL.writeFile goldenFile actualPng
        createDirectoryIfMissing True "dist"
        BL.writeFile "dist/appearance-actual.png" actualPng
      Nothing -> do
        goldenPng <- BL.readFile goldenFile
        let actualImg = decodePngRGBA8 "actual" actualPng
            goldenImg = decodePngRGBA8 "golden" goldenPng
        when (actualImg /= goldenImg) $ do
          createDirectoryIfMissing True "dist"
          BL.writeFile "dist/appearance-actual.png" actualPng
          BL.writeFile "dist/appearance-golden.png" goldenPng
          expectationFailure $
            "Appearance golden mismatch: "
              ++ goldenFile
              ++ " (wrote dist/appearance-actual.png and dist/appearance-golden.png)"

newtype Env = Env
  { envTmpDir :: FilePath
  }

withIntegrationEnv :: ActionWith Env -> IO ()
withIntegrationEnv action =
  withXvfb $ \dn ->
    setDefaultDisplay_ dn $
      withTestDBus $
        withSystemTempDirectory "taffybar-appearance" $ \tmp -> do
          let runtimeDir = tmp </> "xdg-run"
          createDirectoryIfMissing True runtimeDir

          -- Keep user/system config out of the test run and reduce variability.
          withEnv
            [ ("WAYLAND_DISPLAY", const Nothing),
              ("HYPRLAND_INSTANCE_SIGNATURE", const Nothing)
            ]
            $ withSetEnv
              [ ("GDK_BACKEND", "x11"),
                ("GDK_SCALE", "1"),
                ("GDK_DPI_SCALE", "1"),
                ("GTK_CSD", "0"),
                ("GTK_THEME", "Adwaita"),
                ("XDG_SESSION_TYPE", "x11"),
                ("XDG_RUNTIME_DIR", runtimeDir),
                ("NO_AT_BRIDGE", "1"),
                ("GSETTINGS_BACKEND", "memory"),
                ("HOME", tmp),
                ("XDG_CONFIG_HOME", tmp </> "xdg-config"),
                ("XDG_CACHE_HOME", tmp </> "xdg-cache"),
                ("XDG_DATA_HOME", tmp </> "xdg-data")
              ]
            $ action (Env {envTmpDir = tmp})

renderBarScreenshot :: Env -> IO BL.ByteString
renderBarScreenshot Env {envTmpDir = tmp} = do
  exePath <-
    findComponentExecutable
      "taffybar-appearance-snap"
      [ "dist/build/taffybar-appearance-snap/taffybar-appearance-snap"
      ]

  cssPath <- makeAbsolute "test/data/appearance-test.css"
  outPath <- makeAbsolute (tmp </> "appearance-actual.png")

  let pc =
        setStdout inherit $
          setStderr inherit $
            proc exePath ["--out", outPath, "--css", cssPath]

  withProcessTerm pc $ \p -> do
    mEc <- timeout 60_000_000 (waitExitCode p)
    case mEc of
      Nothing -> do
        stopProcess p
        expectationFailure "Timed out running taffybar-appearance-snap"
      Just ExitSuccess -> pure ()
      Just (ExitFailure n) ->
        expectationFailure ("taffybar-appearance-snap exited with " ++ show n)

  BL.readFile outPath

findComponentExecutable :: String -> [FilePath] -> IO FilePath
findComponentExecutable name localCandidates = do
  mexe <- findExecutable name
  case mexe of
    Just exe -> makeAbsolute exe
    Nothing -> go localCandidates
  where
    go [] = fail (name ++ " not found on PATH")
    go (p : ps) = do
      exists <- doesFileExist p
      if exists then makeAbsolute p else go ps

decodePngRGBA8 :: String -> BL.ByteString -> JP.Image JP.PixelRGBA8
decodePngRGBA8 label bs =
  case JP.decodePng (BL.toStrict bs) of
    Left err -> error (label ++ " PNG decode failed: " ++ err)
    Right dyn -> JP.convertRGBA8 dyn
