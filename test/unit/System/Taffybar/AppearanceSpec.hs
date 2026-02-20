module System.Taffybar.AppearanceSpec (spec) where

import Codec.Picture qualified as JP
import Control.Monad (unless, when)
import Data.ByteString.Lazy qualified as BL
import Data.Maybe (isJust)
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
spec = do
  aroundAll withIntegrationEnv $ do
    it "renders a bar under an EWMH window manager" $ \env -> do
      goldenFile <- makeAbsolute "test/data/appearance-ewmh-bar.png"
      actualPng <- renderBarScreenshot env LegacyLayout LegacyWidget
      assertGolden "appearance" goldenFile actualPng

    it "renders a two-level bar under an EWMH window manager" $ \env -> do
      goldenFile <- makeAbsolute "test/data/appearance-ewmh-bar-levels.png"
      actualPng <- renderBarScreenshot env LevelsLayout LegacyWidget
      assertGolden "appearance-levels" goldenFile actualPng

    it "renders the channel workspaces widget under an EWMH window manager" $ \env -> do
      actualPng <- renderBarScreenshot env LegacyLayout ChannelWidget
      assertPngLooksRendered "ewmh-channel-workspaces" actualPng

  it "renders the channel workspaces widget under Hyprland when available" $ do
    available <- hyprlandTestAvailable
    unless available $
      pendingWith
        "Hyprland integration environment unavailable (needs WAYLAND_DISPLAY, HYPRLAND_INSTANCE_SIGNATURE and grim)"
    actualPng <- renderHyprlandScreenshot LegacyLayout ChannelWidget
    assertPngLooksRendered "hyprland-channel-workspaces" actualPng

  it "keeps configured bar height when the windows title has oversized glyph metrics" $ \env -> do
    actualPng <- renderBarScreenshot env WindowsTitleStressLayout
    let actualImg = decodePngRGBA8 "stress" actualPng
    JP.imageHeight actualImg `shouldBe` 55

assertGolden :: String -> FilePath -> BL.ByteString -> IO ()
assertGolden label goldenFile actualPng = do
  shouldUpdate <- lookupEnv "TAFFYBAR_UPDATE_GOLDENS"
  case shouldUpdate of
    Just _ -> do
      BL.writeFile goldenFile actualPng
      createDirectoryIfMissing True "dist"
      BL.writeFile ("dist/" ++ label ++ "-actual.png") actualPng
    Nothing -> do
      goldenPng <- BL.readFile goldenFile
      let actualImg = decodePngRGBA8 "actual" actualPng
          goldenImg = decodePngRGBA8 "golden" goldenPng
      when (actualImg /= goldenImg) $ do
        createDirectoryIfMissing True "dist"
        BL.writeFile ("dist/" ++ label ++ "-actual.png") actualPng
        BL.writeFile ("dist/" ++ label ++ "-golden.png") goldenPng
        expectationFailure $
          "Appearance golden mismatch: "
            ++ goldenFile
            ++ " (wrote dist/"
            ++ label
            ++ "-actual.png and dist/"
            ++ label
            ++ "-golden.png)"

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

data LayoutKind = LegacyLayout | LevelsLayout | WindowsTitleStressLayout

data WidgetKind = LegacyWidget | ChannelWidget

renderBarScreenshot :: Env -> LayoutKind -> WidgetKind -> IO BL.ByteString
renderBarScreenshot Env {envTmpDir = tmp} layout widgetKind = do
  exePath <-
    findComponentExecutable
      "taffybar-appearance-snap"
      [ "dist/build/taffybar-appearance-snap/taffybar-appearance-snap"
      ]

  cssPath <- makeAbsolute "test/data/appearance-test.css"
  outPath <- makeAbsolute (tmp </> "appearance-actual.png")

  let levelArgs =
        case layout of
          LegacyLayout -> []
          LevelsLayout -> ["--levels"]
          WindowsTitleStressLayout -> ["--windows-title-stress"]
      widgetArgs =
        case widgetKind of
          LegacyWidget -> []
          ChannelWidget -> ["--channel-workspaces"]
      pc =
        setStdout inherit $
          setStderr inherit $
            proc exePath (["--out", outPath, "--css", cssPath] ++ levelArgs ++ widgetArgs)

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

renderHyprlandScreenshot :: LayoutKind -> WidgetKind -> IO BL.ByteString
renderHyprlandScreenshot layout widgetKind =
  withSystemTempDirectory "taffybar-appearance-hyprland" $ \tmp -> do
    exePath <-
      findComponentExecutable
        "taffybar-appearance-snap-hyprland"
        [ "dist/build/taffybar-appearance-snap-hyprland/taffybar-appearance-snap-hyprland"
        ]
    cssPath <- makeAbsolute "test/data/appearance-test.css"
    outPath <- makeAbsolute (tmp </> "appearance-hyprland-actual.png")
    let levelArgs =
          case layout of
            LegacyLayout -> []
            LevelsLayout -> ["--levels"]
        widgetArgs =
          case widgetKind of
            LegacyWidget -> []
            ChannelWidget -> ["--channel-workspaces"]
        pc =
          setStdout inherit $
            setStderr inherit $
              proc exePath (["--out", outPath, "--css", cssPath] ++ levelArgs ++ widgetArgs)
    withProcessTerm pc $ \p -> do
      mEc <- timeout 60_000_000 (waitExitCode p)
      case mEc of
        Nothing -> do
          stopProcess p
          expectationFailure "Timed out running taffybar-appearance-snap-hyprland"
        Just ExitSuccess -> pure ()
        Just (ExitFailure n) ->
          expectationFailure ("taffybar-appearance-snap-hyprland exited with " ++ show n)
    BL.readFile outPath

assertPngLooksRendered :: String -> BL.ByteString -> IO ()
assertPngLooksRendered label actualPng = do
  let img = decodePngRGBA8 label actualPng
      w = JP.imageWidth img
      h = JP.imageHeight img
      opaquePixelCount =
        length
          [ ()
          | y <- [0 .. h - 1],
            x <- [0 .. w - 1],
            let JP.PixelRGBA8 _ _ _ a = JP.pixelAt img x y,
            a > 200
          ]
  when (w <= 0 || h <= 0) $
    expectationFailure ("Rendered PNG has invalid dimensions for " ++ label)
  when (opaquePixelCount <= 4000) $
    expectationFailure ("Rendered PNG seems empty for " ++ label)

hyprlandTestAvailable :: IO Bool
hyprlandTestAvailable = do
  waylandDisplay <- lookupEnv "WAYLAND_DISPLAY"
  hyprSig <- lookupEnv "HYPRLAND_INSTANCE_SIGNATURE"
  grimExe <- findExecutable "grim"
  pure (isJust waylandDisplay && isJust hyprSig && isJust grimExe)

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
