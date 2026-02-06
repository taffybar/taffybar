{-# LANGUAGE OverloadedStrings #-}

-- Helper executable for the appearance golden test.
--
-- This is intentionally a separate process from the Hspec runner because
-- 'startTaffybar' enters the GTK main loop (an FFI call) and is not reliably
-- interruptible by async exceptions. The Hspec test can always timeout/kill
-- this executable in CI.
module Main (main) where

import Control.Concurrent (MVar, forkIO, newEmptyMVar, threadDelay, takeMVar)
import Control.Concurrent.MVar (readMVar, tryPutMVar, tryReadMVar)
import Control.Exception (SomeException, try)
import Control.Monad (filterM, unless, void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ask)
import Data.Default (def)
import Data.Maybe (listToMaybe)
import Data.Unique (newUnique)
import System.Directory (createDirectoryIfMissing, doesFileExist, findExecutable, makeAbsolute)
import System.Environment (getArgs, setEnv, unsetEnv)
import System.Exit (ExitCode (..), exitWith)
import System.FilePath (takeDirectory, (</>))
import System.Info (arch, os)
import System.IO (hPutStrLn, stderr)

import UnliftIO.Temporary (withSystemTempDirectory)

import Foreign.C.Types (CLong)
import System.Posix.Process (exitImmediately)
import System.Posix.Signals (signalProcess, sigKILL, sigTERM)

import qualified Codec.Picture as JP
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import qualified GI.Gdk as Gdk
import qualified GI.GdkPixbuf.Objects.Pixbuf as PB
import qualified GI.Gtk as Gtk

import Graphics.X11.Xlib (Atom, Display, Window, closeDisplay, defaultRootWindow, internAtom, openDisplay)
import Graphics.X11.Xlib.Extras (getWindowProperty32)

import Graphics.UI.GIGtkStrut
  ( StrutConfig (..)
  , StrutPosition (TopPos)
  , StrutSize (ExactSize)
  , defaultStrutConfig
  )
import System.Posix.Files (createSymbolicLink, removeLink)
import System.Process.Typed (Process, getPid, proc, startProcess)

import System.Taffybar (startTaffybar)
import System.Taffybar.Context
  ( BarConfig (..)
  , Context (..)
  , TaffyIO
  , TaffybarConfig (..)
  , exitTaffybar
  )
import System.Taffybar.Util (postGUIASync)
import System.Taffybar.Widget.Workspaces (WorkspacesConfig (..), workspacesNew)

data Args = Args
  { outFile :: FilePath
  , cssFile :: FilePath
  }

main :: IO ()
main = do
  Args { outFile = outPath, cssFile = cssPath } <- parseArgs =<< getArgs

  -- Force X11 backend selection even if the surrounding session is Wayland.
  unsetEnv "WAYLAND_DISPLAY"
  unsetEnv "HYPRLAND_INSTANCE_SIGNATURE"
  setEnv "XDG_SESSION_TYPE" "x11"
  setEnv "GDK_BACKEND" "x11"
  setEnv "GDK_SCALE" "1"
  setEnv "GDK_DPI_SCALE" "1"
  setEnv "GTK_CSD" "0"
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
    res <- runUnderWm wmProc outPath cssPath
    killProcessNoWait wmProc
    pure res

  exitWith ec

runUnderWm :: Process () () () -> FilePath -> FilePath -> IO ExitCode
runUnderWm wmProc outPath cssPath = do
  ctxVar :: MVar Context <- newEmptyMVar
  resultVar :: MVar (Either String BL.ByteString) <- newEmptyMVar
  doneVar :: MVar ExitCode <- newEmptyMVar

  createDirectoryIfMissing True (takeDirectory outPath)

  -- Writes the result out and requests bar shutdown.
  void $ forkIO $ finalizeThread ctxVar resultVar doneVar outPath

  -- Hard watchdog for CI stability: always tries to end the GTK loop, and
  -- stops the WM as a last resort.
  void $ forkIO $ watchdogThread wmProc ctxVar resultVar doneVar 30_000_000

  barUnique <- newUnique
  let wsCfg = (def :: WorkspacesConfig) { labelSetter = const (pure "") }
      barCfg =
        BarConfig
          { strutConfig =
              defaultStrutConfig
                { strutHeight = ExactSize 40
                , strutMonitor = Just 0
                , strutPosition = TopPos
                }
          , widgetSpacing = 0
          , startWidgets = [workspacesNew wsCfg]
          , centerWidgets = []
          , endWidgets = []
          , barId = barUnique
          }
      cfg =
        def
          { dbusClientParam = Nothing
          , cssPaths = [cssPath]
          , getBarConfigsParam = pure [barCfg]
          , startupHook = scheduleSnapshot ctxVar resultVar
          , errorMsg = Nothing
          }

  -- Blocks in Gtk.main until we request shutdown.
  startTaffybar cfg

  -- Should have been set by finalizeThread or watchdogThread.
  takeMVar doneVar

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

finalizeThread
  :: MVar Context
  -> MVar (Either String BL.ByteString)
  -> MVar ExitCode
  -> FilePath
  -> IO ()
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

watchdogThread
  :: Process () () ()
  -> MVar Context
  -> MVar (Either String BL.ByteString)
  -> MVar ExitCode
  -> Int
  -> IO ()
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
        (p:_) -> makeAbsolute p
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
      mPng <- try (snapshotGtkWindowPng win) :: IO (Either SomeException BL.ByteString)
      case mPng of
        Left _ -> pure ()
        Right png -> void (tryPutMVar resultVar (Right png))

snapshotGtkWindowPng :: Gtk.Window -> IO BL.ByteString
snapshotGtkWindowPng win = do
  drainGtkEvents 200

  mw <- Gtk.widgetGetWindow win
  gdkWindow <- maybe (fail "No GdkWindow for Gtk.Window") pure mw

  w <- fromIntegral <$> Gtk.widgetGetAllocatedWidth win
  h <- fromIntegral <$> Gtk.widgetGetAllocatedHeight win
  when (w <= 0 || h <= 0) $ fail "Window has invalid size"

  mpb <- Gdk.pixbufGetFromWindow gdkWindow 0 0 w h
  pb <- maybe (fail "Failed to capture pixbuf from window") pure mpb

  img <- pixbufToImageRGBA8 pb
  pure (JP.encodePng img)

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
    ["--out", outPath, "--css", cssPath] -> pure Args { outFile = outPath, cssFile = cssPath }
    ["--css", cssPath, "--out", outPath] -> pure Args { outFile = outPath, cssFile = cssPath }
    _ -> die "usage: taffybar-appearance-snap --out OUT.png --css appearance-test.css"

die :: String -> IO a
die msg = do
  hPutStrLn stderr msg
  fail msg
