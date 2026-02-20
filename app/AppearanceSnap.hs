{-# LANGUAGE OverloadedStrings #-}

-- Helper executable for the appearance golden test.
--
-- This is intentionally a separate process from the Hspec runner because
-- 'startTaffybar' enters the GTK main loop (an FFI call) and is not reliably
-- interruptible by async exceptions. The Hspec test can always timeout/kill
-- this executable in CI.
module Main (main) where

import qualified Codec.Picture as JP
import Control.Concurrent (MVar, forkIO, newEmptyMVar, takeMVar, threadDelay)
import Control.Concurrent.MVar (readMVar, tryPutMVar, tryReadMVar)
import Control.Exception (SomeException, bracket, try)
import Control.Monad (filterM, unless, void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ask)
import Data.Bits (shiftL, (.|.))
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Default (def)
import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import Data.Unique (Unique, newUnique)
import Data.Word (Word32)
import Foreign.C.Types (CLong)
import qualified GI.Gdk as Gdk
import qualified GI.GdkPixbuf.Objects.Pixbuf as PB
import qualified GI.Gtk as Gtk
import Graphics.UI.GIGtkStrut
  ( StrutConfig (..),
    StrutPosition (TopPos),
    StrutSize (ExactSize),
    defaultStrutConfig,
  )
import Graphics.X11.Xlib
  ( Atom,
    Display,
    Window,
    closeDisplay,
    createSimpleWindow,
    defaultRootWindow,
    internAtom,
    mapWindow,
    openDisplay,
    storeName,
    sync,
  )
import Graphics.X11.Xlib.Extras
  ( ClassHint (..),
    changeProperty32,
    getWindowProperty32,
    propModeReplace,
    setClassHint,
  )
import System.Directory (createDirectoryIfMissing, doesFileExist, findExecutable, makeAbsolute)
import System.Environment (getArgs, setEnv, unsetEnv)
import System.Exit (ExitCode (..), exitWith)
import System.FilePath (takeDirectory, (</>))
import System.IO (hPutStrLn, stderr)
import System.Info (arch, os)
import System.Posix.Files (createSymbolicLink, removeLink)
import System.Posix.Process (exitImmediately)
import System.Posix.Signals (sigKILL, sigTERM, signalProcess)
import System.Process.Typed (Process, getPid, proc, startProcess)
import System.Taffybar (startTaffybar)
import System.Taffybar.Context
  ( BarConfig (..),
    BarLevelConfig (..),
    Context (..),
    TaffyIO,
    TaffybarConfig (..),
    exitTaffybar,
  )
import System.Taffybar.Util (postGUIASync)
import qualified System.Taffybar.Widget.Windows as Windows
import qualified System.Taffybar.Widget.Workspaces.Config as WorkspaceConfig
import System.Taffybar.Widget.Workspaces.EWMH
  ( WorkspacesConfig,
    getWindowIconPixbufFromEWMH,
    sortWindowsByStackIndex,
    workspacesNew,
  )
import qualified System.Taffybar.Widget.Workspaces.EWMH as Workspaces
import UnliftIO.Temporary (withSystemTempDirectory)

data Args = Args
  { outFile :: FilePath,
    cssFile :: FilePath,
    layoutMode :: LayoutMode
  }

data LayoutMode = LayoutLegacy | LayoutLevels | LayoutWindowsTitleStress
  deriving (Eq, Show)

main :: IO ()
main = do
  Args {outFile = outPath, cssFile = cssPath, layoutMode = mode} <- parseArgs =<< getArgs

  -- Force X11 backend selection even if the surrounding session is Wayland.
  unsetEnv "WAYLAND_DISPLAY"
  unsetEnv "HYPRLAND_INSTANCE_SIGNATURE"
  setEnv "XDG_SESSION_TYPE" "x11"
  setEnv "GDK_BACKEND" "x11"
  setEnv "GDK_SCALE" "1"
  setEnv "GDK_DPI_SCALE" "1"
  setEnv "GTK_CSD" "0"
  setEnv "GTK_THEME" "Adwaita"
  setEnv "NO_AT_BRIDGE" "1"
  setEnv "GSETTINGS_BACKEND" "memory"

  ec <- withSystemTempDirectory "taffybar-appearance-snap" $ \tmp -> do
    let runtimeDir = tmp </> "xdg-run"
    createDirectoryIfMissing True runtimeDir
    setEnv "XDG_RUNTIME_DIR" runtimeDir

    wmExePath <-
      findComponentExecutable
        "taffybar-test-wm"
        [ "dist/build/taffybar-test-wm/taffybar-test-wm"
        ]

    -- XMonad expects the running binary to be named like xmonad-<arch>-<os>.
    -- If it isn't, it tries to re-exec (and can fail in CI).
    let expectedName = "xmonad-" ++ arch ++ "-" ++ os
        linkPath = tmp </> expectedName
    void (try (removeLink linkPath) :: IO (Either SomeException ()))
    createSymbolicLink wmExePath linkPath

    let wmCfg = proc linkPath []
    wmProc <- startProcess wmCfg
    waitForEwmh 10_000_000
    -- Best-effort cleanup. We do not wait for the WM process here because
    -- 'waitForProcess' can fail with ECHILD in some CI environments.
    --
    -- If this fails, the parent Xvfb teardown will still kill the X server,
    -- which causes the WM to exit.
    res <- runUnderWm wmProc outPath cssPath mode
    killProcessNoWait wmProc
    pure res

  exitWith ec

runUnderWm :: Process () () () -> FilePath -> FilePath -> LayoutMode -> IO ExitCode
runUnderWm wmProc outPath cssPath mode = do
  ctxVar <- (newEmptyMVar :: IO (MVar Context))
  resultVar <- (newEmptyMVar :: IO (MVar (Either String BL.ByteString)))
  doneVar <- (newEmptyMVar :: IO (MVar ExitCode))

  createDirectoryIfMissing True (takeDirectory outPath)

  -- Writes the result out and requests bar shutdown.
  void $ forkIO $ finalizeThread ctxVar resultVar doneVar outPath

  -- Hard watchdog for CI stability: always tries to end the GTK loop, and
  -- stops the WM as a last resort.
  void $ forkIO $ watchdogThread wmProc ctxVar resultVar doneVar 30_000_000

  barUnique <- newUnique
  let wsCfgDefault = def :: WorkspacesConfig
      wsCfg =
        wsCfgDefault
          { Workspaces.workspacesConfig =
              (Workspaces.workspacesConfig wsCfgDefault)
                { -- Avoid font-dependent output in the appearance golden: we only care
                  -- that icons/layout render deterministically.
                  WorkspaceConfig.labelSetter = const (pure ""),
                  WorkspaceConfig.maxIcons = Just 1,
                  WorkspaceConfig.minIcons = 1,
                  WorkspaceConfig.getWindowIconPixbuf = getWindowIconPixbufFromEWMH,
                  WorkspaceConfig.iconSort = sortWindowsByStackIndex
                }
          }
      barCfg = buildBarConfig wsCfg barUnique mode
      cfg =
        def
          { dbusClientParam = Nothing,
            cssPaths = [cssPath],
            getBarConfigsParam = pure [barCfg],
            startupHook = scheduleSnapshot ctxVar resultVar,
            errorMsg = Nothing
          }

  withTestWindows $ do
    -- Blocks in Gtk.main until we request shutdown.
    startTaffybar cfg

    -- Should have been set by finalizeThread or watchdogThread.
    takeMVar doneVar

buildBarConfig :: WorkspacesConfig -> Unique -> LayoutMode -> BarConfig
buildBarConfig wsCfg barUnique mode =
  case mode of
    LayoutLegacy ->
      BarConfig
        { strutConfig = baseStrutConfig 40,
          widgetSpacing = 8,
          startWidgets = [workspacesNew wsCfg],
          centerWidgets = [testBoxWidget "test-center-box" 200 20],
          endWidgets =
            [ testBoxWidget "test-pill" 52 20,
              testBoxWidget "test-pill" 52 20,
              testBoxWidget "test-right-box" 16 16
            ],
          barLevels = Nothing,
          barId = barUnique
        }
    LayoutLevels ->
      BarConfig
        { strutConfig = baseStrutConfig 72,
          widgetSpacing = 8,
          startWidgets = [testBoxWidget "test-ignored-old-left" 40 16],
          centerWidgets = [testBoxWidget "test-ignored-old-center" 120 16],
          endWidgets = [testBoxWidget "test-ignored-old-right" 40 16],
          barLevels =
            Just
              [ BarLevelConfig
                  { levelStartWidgets = [workspacesNew wsCfg],
                    levelCenterWidgets = [testBoxWidget "test-center-box" 200 20],
                    levelEndWidgets =
                      [ testBoxWidget "test-pill" 52 20,
                        testBoxWidget "test-right-box" 16 16
                      ]
                  },
                BarLevelConfig
                  { levelStartWidgets = [testBoxWidget "test-level2-left" 78 14],
                    levelCenterWidgets = [testBoxWidget "test-level2-center" 150 14],
                    levelEndWidgets = [testBoxWidget "test-level2-right" 78 14]
                  }
              ],
          barId = barUnique
        }
    LayoutWindowsTitleStress ->
      BarConfig
        { strutConfig = baseStrutConfig 55,
          widgetSpacing = 8,
          startWidgets = [workspacesNew wsCfg, Windows.windowsNew windowsCfg],
          centerWidgets = [],
          endWidgets = [testBoxWidget "test-pill" 52 20],
          barLevels = Nothing,
          barId = barUnique
        }
  where
    windowsCfg =
      def
        { Windows.getActiveLabel = pure "line 1\nline 2\nline 3\nline 4",
          Windows.getActiveWindowIconPixbuf = Nothing
        }
    baseStrutConfig h =
      defaultStrutConfig
        { strutHeight = ExactSize h,
          strutMonitor = Just 0,
          strutPosition = TopPos
        }

testBoxWidget :: T.Text -> Int -> Int -> TaffyIO Gtk.Widget
testBoxWidget klass w h = liftIO $ do
  box <- Gtk.eventBoxNew
  widget <- Gtk.toWidget box
  Gtk.widgetSetSizeRequest widget (fromIntegral w) (fromIntegral h)
  sc <- Gtk.widgetGetStyleContext widget
  Gtk.styleContextAddClass sc klass
  -- Taffybar does not automatically show arbitrary user widgets; most built-in
  -- widgets call show/showAll themselves. For deterministic appearance tests,
  -- ensure these test widgets are visible.
  Gtk.widgetShowAll widget
  pure widget

withTestWindows :: IO a -> IO a
withTestWindows action =
  bracket setup closeDisplay (const action)
  where
    setup :: IO Display
    setup = do
      d <- openDisplay ""
      let root = defaultRootWindow d
      iconAtom <- internAtom d "_NET_WM_ICON" False
      cardinalAtom <- internAtom d "CARDINAL" False

      -- Create two managed windows with deterministic EWMH icons so the
      -- workspaces widget exercises its icon rendering path in CI.
      --
      -- Important: keep the X11 connection open until the snapshot is taken.
      -- X11 resources are owned by the client; closing the Display would
      -- destroy the windows.
      w1 <- createSimpleWindow d root 10 60 150 100 0 0 0
      w2 <- createSimpleWindow d root 170 60 150 100 0 0 0
      storeName d w1 "taffybar-test-1"
      storeName d w2 "taffybar-test-2"
      setClassHint d w1 (ClassHint "redwin" "RedWin")
      setClassHint d w2 (ClassHint "greenwin" "GreenWin")

      let setIcon win px =
            changeProperty32
              d
              win
              iconAtom
              cardinalAtom
              propModeReplace
              (ewmhIconDataSolid 16 16 px)

      setIcon w1 (argb 0xFF 0xE0 0x3A 0x3A) -- red
      setIcon w2 (argb 0xFF 0x3A 0xE0 0x6A) -- green
      mapWindow d w1
      mapWindow d w2
      sync d False

      -- Give the WM a moment to manage/map and publish EWMH properties.
      threadDelay 300_000
      pure d

argb :: Word32 -> Word32 -> Word32 -> Word32 -> Word32
argb a r g b = shiftL a 24 .|. shiftL r 16 .|. shiftL g 8 .|. b

ewmhIconDataSolid :: Int -> Int -> Word32 -> [CLong]
ewmhIconDataSolid w h px =
  let header = [fromIntegral w, fromIntegral h]
      pixels = replicate (w * h) (fromIntegral px)
   in header ++ pixels

scheduleSnapshot :: MVar Context -> MVar (Either String BL.ByteString) -> TaffyIO ()
scheduleSnapshot ctxVar resultVar = do
  ctx <- ask
  liftIO $ void (tryPutMVar ctxVar ctx)
  liftIO $ void $ forkIO (pollLoop ctx)
  where
    pollLoop ctx' = do
      done <- tryReadMVar resultVar
      case done of
        Just _ -> pure ()
        Nothing -> do
          postGUIASync (trySnapshotOnGuiThread ctx' resultVar)
          threadDelay 50_000
          pollLoop ctx'

finalizeThread ::
  MVar Context ->
  MVar (Either String BL.ByteString) ->
  MVar ExitCode ->
  FilePath ->
  IO ()
finalizeThread ctxVar resultVar doneVar outPath = do
  res <- takeMVar resultVar
  case res of
    Left msg -> do
      BL.writeFile outPath (BL.fromStrict B.empty)
      hPutStrLn stderr msg
      void $ tryPutMVar doneVar (ExitFailure 1)
    Right png -> do
      BL.writeFile outPath png
      void $ tryPutMVar doneVar ExitSuccess

  mCtx <- tryReadMVar ctxVar
  case mCtx of
    Nothing -> pure ()
    Just ctx -> void (try (exitTaffybar ctx) :: IO (Either SomeException ()))

watchdogThread ::
  Process () () () ->
  MVar Context ->
  MVar (Either String BL.ByteString) ->
  MVar ExitCode ->
  Int ->
  IO ()
watchdogThread wmProc ctxVar resultVar doneVar usec = do
  threadDelay usec
  didSet <- tryPutMVar resultVar (Left "Timed out waiting for appearance snapshot")
  when didSet $ void $ tryPutMVar doneVar (ExitFailure 124)
  mCtx <- tryReadMVar ctxVar
  case mCtx of
    Nothing -> pure ()
    Just ctx -> void (try (exitTaffybar ctx) :: IO (Either SomeException ()))
  killProcessNoWait wmProc
  -- If the GTK loop doesn't exit promptly, force the process to end so
  -- the Hspec runner never hangs.
  threadDelay 2_000_000
  exitImmediately (ExitFailure 124)

killProcessNoWait :: Process () () () -> IO ()
killProcessNoWait p = do
  mpid <- getPid p
  case mpid of
    Nothing -> pure ()
    Just pid -> do
      -- Best-effort: TERM, then KILL shortly after.
      void (try (signalProcess sigTERM pid) :: IO (Either SomeException ()))
      threadDelay 250_000
      void (try (signalProcess sigKILL pid) :: IO (Either SomeException ()))

findComponentExecutable :: String -> [FilePath] -> IO FilePath
findComponentExecutable name localCandidates = do
  mexe <- findExecutable name
  case mexe of
    Just exe -> makeAbsolute exe
    Nothing -> do
      existing <- filterM doesFileExist localCandidates
      case existing of
        (p : _) -> makeAbsolute p
        [] -> die (name ++ " not found on PATH")

waitForEwmh :: Int -> IO ()
waitForEwmh maxUsec = do
  let stepUsec = 50_000
      maxSteps = maxUsec `div` stepUsec
  ed <- try (openDisplay "") :: IO (Either SomeException Display)
  d <- either (const (die "Failed to open X11 display (DISPLAY unset?)")) pure ed
  let root = defaultRootWindow d
  atom <- internAtom d "_NET_NUMBER_OF_DESKTOPS" False
  ok <- go d root atom 0 maxSteps
  closeDisplay d
  unless ok $ die "Timed out waiting for EWMH WM"
  where
    go :: Display -> Window -> Atom -> Int -> Int -> IO Bool
    go d root atom n maxSteps' = do
      r <- try (getWindowProperty32 d atom root) :: IO (Either SomeException (Maybe [CLong]))
      case r of
        Right (Just _) -> pure True
        _ ->
          if n > maxSteps'
            then pure False
            else threadDelay 50_000 >> go d root atom (n + 1) maxSteps'

trySnapshotOnGuiThread :: Context -> MVar (Either String BL.ByteString) -> IO ()
trySnapshotOnGuiThread ctx resultVar = do
  -- Read the windows list and snapshot the first bar window.
  ws <- readMVar (existingWindows ctx)
  case listToMaybe ws of
    Nothing -> pure ()
    Just (_, win) -> do
      mImg <- try (snapshotGtkWindowImageRGBA8 win) :: IO (Either SomeException (JP.Image JP.PixelRGBA8))
      case mImg of
        Left _ -> pure ()
        Right img ->
          -- Don't accept a snapshot until the workspace icons and basic bar
          -- styling are actually rendered, otherwise we can end up with a
          -- largely transparent screenshot that doesn't exercise much beyond
          -- the icon path.
          when (imageHasTestIcons img) $
            void (tryPutMVar resultVar (Right (JP.encodePng img)))

imageHasTestIcons :: JP.Image JP.PixelRGBA8 -> Bool
imageHasTestIcons img =
  let tol :: Int
      tol = 40
      opaqueThreshold :: Int
      opaqueThreshold = 5000
      near (JP.PixelRGBA8 r g b a) (tr, tg, tb) =
        a > 200
          && abs (fromIntegral r - tr) <= tol
          && abs (fromIntegral g - tg) <= tol
          && abs (fromIntegral b - tb) <= tol
      redTarget :: (Int, Int, Int)
      redTarget = (224, 58, 58)
      greenTarget :: (Int, Int, Int)
      greenTarget = (58, 224, 106)
      w = JP.imageWidth img
      h = JP.imageHeight img
      go y seenR seenG opaqueCount
        | y >= h = seenR && seenG && opaqueCount > opaqueThreshold
        | otherwise =
            let goX x sr sg oc
                  | x >= w = go (y + 1) sr sg oc
                  | otherwise =
                      let px = JP.pixelAt img x y
                          sr1 = sr || near px redTarget
                          sg1 = sg || near px greenTarget
                          oc1 =
                            oc
                              + (case px of JP.PixelRGBA8 _ _ _ a -> if a > 200 then 1 else 0)
                       in (sr1 && sg1 && oc1 > opaqueThreshold) || goX (x + 1) sr1 sg1 oc1
             in goX 0 seenR seenG opaqueCount
   in go 0 False False (0 :: Int)

snapshotGtkWindowImageRGBA8 :: Gtk.Window -> IO (JP.Image JP.PixelRGBA8)
snapshotGtkWindowImageRGBA8 win = do
  drainGtkEvents 200

  mw <- Gtk.widgetGetWindow win
  gdkWindow <- maybe (fail "No GdkWindow for Gtk.Window") pure mw

  w <- fromIntegral <$> Gtk.widgetGetAllocatedWidth win
  h <- fromIntegral <$> Gtk.widgetGetAllocatedHeight win
  when (w <= 0 || h <= 0) $ fail "Window has invalid size"

  mpb <- Gdk.pixbufGetFromWindow gdkWindow 0 0 w h
  pb <- maybe (fail "Failed to capture pixbuf from window") pure mpb

  pixbufToImageRGBA8 pb

pixbufToImageRGBA8 :: PB.Pixbuf -> IO (JP.Image JP.PixelRGBA8)
pixbufToImageRGBA8 pb = do
  w <- fromIntegral <$> PB.pixbufGetWidth pb
  h <- fromIntegral <$> PB.pixbufGetHeight pb
  rowstride <- fromIntegral <$> PB.pixbufGetRowstride pb
  nChannels <- fromIntegral <$> PB.pixbufGetNChannels pb
  hasAlpha <- PB.pixbufGetHasAlpha pb
  pixels <- PB.pixbufGetPixels pb

  let aIndex = if hasAlpha then 3 else (-1)
      pxAt x y =
        let base = y * rowstride + x * nChannels
            r = B.index pixels (base + 0)
            g = B.index pixels (base + 1)
            b = B.index pixels (base + 2)
            a = if aIndex >= 0 then B.index pixels (base + aIndex) else 255
         in JP.PixelRGBA8 r g b a

  pure (JP.generateImage pxAt w h)

drainGtkEvents :: Int -> IO ()
drainGtkEvents 0 = pure ()
drainGtkEvents n = do
  pendingEvents <- Gtk.eventsPending
  when pendingEvents $ do
    _ <- Gtk.mainIterationDo False
    drainGtkEvents (n - 1)

parseArgs :: [String] -> IO Args
parseArgs args =
  case args of
    ["--out", outPath, "--css", cssPath] ->
      pure Args {outFile = outPath, cssFile = cssPath, layoutMode = LayoutLegacy}
    ["--css", cssPath, "--out", outPath] ->
      pure Args {outFile = outPath, cssFile = cssPath, layoutMode = LayoutLegacy}
    ["--out", outPath, "--css", cssPath, "--levels"] ->
      pure Args {outFile = outPath, cssFile = cssPath, layoutMode = LayoutLevels}
    ["--css", cssPath, "--out", outPath, "--levels"] ->
      pure Args {outFile = outPath, cssFile = cssPath, layoutMode = LayoutLevels}
    ["--levels", "--out", outPath, "--css", cssPath] ->
      pure Args {outFile = outPath, cssFile = cssPath, layoutMode = LayoutLevels}
    ["--levels", "--css", cssPath, "--out", outPath] ->
      pure Args {outFile = outPath, cssFile = cssPath, layoutMode = LayoutLevels}
    ["--out", outPath, "--css", cssPath, "--windows-title-stress"] ->
      pure Args {outFile = outPath, cssFile = cssPath, layoutMode = LayoutWindowsTitleStress}
    ["--css", cssPath, "--out", outPath, "--windows-title-stress"] ->
      pure Args {outFile = outPath, cssFile = cssPath, layoutMode = LayoutWindowsTitleStress}
    ["--windows-title-stress", "--out", outPath, "--css", cssPath] ->
      pure Args {outFile = outPath, cssFile = cssPath, layoutMode = LayoutWindowsTitleStress}
    ["--windows-title-stress", "--css", cssPath, "--out", outPath] ->
      pure Args {outFile = outPath, cssFile = cssPath, layoutMode = LayoutWindowsTitleStress}
    _ ->
      die
        "usage: taffybar-appearance-snap --out OUT.png --css appearance-test.css [--levels|--windows-title-stress]"

die :: String -> IO a
die msg = do
  hPutStrLn stderr msg
  fail msg
