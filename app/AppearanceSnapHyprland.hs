{-# LANGUAGE OverloadedStrings #-}

-- Helper executable for the Hyprland (Wayland) appearance golden test.
--
-- This is intentionally a separate process from the Hspec runner because
-- 'startTaffybar' enters the GTK main loop (an FFI call) and is not reliably
-- interruptible by async exceptions. The VM harness can always timeout/kill
-- this executable in CI.
module Main (main) where

import Control.Concurrent (MVar, forkIO, newEmptyMVar, threadDelay, takeMVar)
import Control.Concurrent.MVar (tryPutMVar, tryReadMVar)
import Control.Exception (SomeException, try)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ask)
import Data.Default (def)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Unique (newUnique)
import System.Directory
  ( createDirectoryIfMissing
  , findExecutable
  , makeAbsolute
  )
import System.Environment (getArgs, lookupEnv, setEnv, unsetEnv)
import System.Exit (ExitCode (..), exitWith)
import System.FilePath (takeDirectory, (</>))
import System.IO (hPutStrLn, stderr)
import System.Posix.Process (exitImmediately)

import qualified Codec.Picture as JP
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import UnliftIO.Temporary (withSystemTempDirectory)

import System.Process.Typed
  ( proc
  , readProcess
  )

import qualified GI.Gtk as Gtk

import Graphics.UI.GIGtkStrut
  ( StrutConfig (..)
  , StrutPosition (TopPos)
  , StrutSize (ExactSize)
  , defaultStrutConfig
  )

import System.Taffybar (startTaffybar)
import System.Taffybar.Context
  ( BarConfig (..)
  , Context (..)
  , TaffyIO
  , TaffybarConfig (..)
  , exitTaffybar
  )

data Args = Args
  { outFile :: FilePath
  , cssFile :: FilePath
  }

main :: IO ()
main = do
  Args { outFile = outPath, cssFile = cssPath } <- parseArgs =<< getArgs

  -- Reduce variability (but do not clobber WAYLAND_DISPLAY / XDG_RUNTIME_DIR /
  -- HYPRLAND_INSTANCE_SIGNATURE; those are provided by the compositor session).
  unsetEnv "DISPLAY"
  setEnv "GDK_BACKEND" "wayland"
  setEnv "XDG_SESSION_TYPE" "wayland"
  setEnv "GDK_SCALE" "1"
  setEnv "GDK_DPI_SCALE" "1"
  setEnv "GTK_CSD" "0"
  setEnv "GTK_THEME" "Adwaita"
  setEnv "NO_AT_BRIDGE" "1"
  setEnv "GSETTINGS_BACKEND" "memory"

  _ <- requireEnv "XDG_RUNTIME_DIR"
  _ <- requireEnv "WAYLAND_DISPLAY"
  _ <- requireEnv "HYPRLAND_INSTANCE_SIGNATURE"
  _ <- requireExe "grim"

  ec <- withSystemTempDirectory "tb-hyprland" $ \tmp -> do
    let homeDir = tmp </> "home"
        xdgCfg = homeDir </> "xdg-config"
        xdgCache = homeDir </> "xdg-cache"
        xdgData = homeDir </> "xdg-data"

    createDirectoryIfMissing True homeDir
    createDirectoryIfMissing True xdgCfg
    createDirectoryIfMissing True xdgCache
    createDirectoryIfMissing True xdgData

    setEnv "HOME" homeDir
    setEnv "XDG_CONFIG_HOME" xdgCfg
    setEnv "XDG_CACHE_HOME" xdgCache
    setEnv "XDG_DATA_HOME" xdgData

    createDirectoryIfMissing True (takeDirectory outPath)

    runUnderHyprland outPath cssPath

  exitWith ec

runUnderHyprland :: FilePath -> FilePath -> IO ExitCode
runUnderHyprland outPath cssPath = do
  ctxVar :: MVar Context <- newEmptyMVar
  resultVar :: MVar (Either String BL.ByteString) <- newEmptyMVar
  doneVar :: MVar ExitCode <- newEmptyMVar
  lastShotRef :: IORef (Maybe BL.ByteString) <- newIORef Nothing

  -- Writes the result out and requests bar shutdown.
  void $ forkIO $ finalizeThread ctxVar resultVar doneVar outPath

  -- Hard watchdog for CI stability: always tries to end the GTK loop.
  void $ forkIO $ watchdogThread ctxVar resultVar doneVar lastShotRef 30_000_000

  barUnique <- newUnique

  let barCfg =
        BarConfig
          { strutConfig =
              defaultStrutConfig
                { strutHeight = ExactSize 40
                , strutMonitor = Just 0
                , strutPosition = TopPos
                }
          , widgetSpacing = 8
          , startWidgets = [testPillBoxWidget "test-pill" 56 20]
          , centerWidgets = [testBoxWidget "test-center-box" 200 20]
          , endWidgets =
              [ testPillBoxWidget "test-pill" 52 20
              , testPillBoxWidget "test-pill" 46 20
              , testBoxWidget "test-right-box" 16 16
              ]
          , barId = barUnique
          }

      cfg =
        def
          { dbusClientParam = Nothing
          , cssPaths = [cssPath]
          , getBarConfigsParam = pure [barCfg]
          , startupHook = scheduleSnapshot ctxVar resultVar lastShotRef
          , errorMsg = Nothing
          }

  -- Blocks in Gtk.main until we request shutdown.
  startTaffybar cfg

  -- Should have been set by finalizeThread or watchdogThread.
  takeMVar doneVar

testBoxWidget :: T.Text -> Int -> Int -> TaffyIO Gtk.Widget
testBoxWidget klass w h = liftIO $ do
  box <- Gtk.eventBoxNew
  widget <- Gtk.toWidget box
  Gtk.widgetSetSizeRequest widget (fromIntegral w) (fromIntegral h)
  sc <- Gtk.widgetGetStyleContext widget
  Gtk.styleContextAddClass sc klass
  Gtk.widgetShowAll widget
  pure widget

testPillBoxWidget :: T.Text -> Int -> Int -> TaffyIO Gtk.Widget
testPillBoxWidget klass w h = liftIO $ do
  box <- Gtk.eventBoxNew
  widget <- Gtk.toWidget box
  Gtk.widgetSetSizeRequest widget (fromIntegral w) (fromIntegral h)
  sc <- Gtk.widgetGetStyleContext widget
  Gtk.styleContextAddClass sc klass
  Gtk.widgetShowAll widget
  pure widget

scheduleSnapshot :: MVar Context -> MVar (Either String BL.ByteString) -> IORef (Maybe BL.ByteString) -> TaffyIO ()
scheduleSnapshot ctxVar resultVar lastShotRef = do
  ctx <- ask
  liftIO $ void (tryPutMVar ctxVar ctx)

  -- Delay slightly to give the compositor time to map the layer-surface and
  -- for widgets to render.
  liftIO $ void $ forkIO $ do
    threadDelay 2_000_000
    -- Require two consecutive identical frames to avoid capturing during
    -- initial GTK/compositor settling (animations, late mapping, etc).
    takeSnapshotWithRetries lastShotRef resultVar 60

takeSnapshotWithRetries :: IORef (Maybe BL.ByteString) -> MVar (Either String BL.ByteString) -> Int -> IO ()
takeSnapshotWithRetries _ resultVar 0 =
  void $ tryPutMVar resultVar (Left "Failed to capture Hyprland appearance snapshot")
takeSnapshotWithRetries lastShotRef resultVar n = do
  done <- tryReadMVar resultVar
  case done of
    Just _ -> pure ()
    Nothing -> do
      shot <- takeSnapshot
      case shot of
        Left _ -> do
          threadDelay 200_000
          takeSnapshotWithRetries lastShotRef resultVar (n - 1)
        Right encoded -> do
          prev <- readIORef lastShotRef
          writeIORef lastShotRef (Just encoded)
          case prev of
            Just prevEncoded | prevEncoded == encoded ->
              void $ tryPutMVar resultVar (Right encoded)
            _ -> do
              threadDelay 200_000
              takeSnapshotWithRetries lastShotRef resultVar (n - 1)

takeSnapshot :: IO (Either String BL.ByteString)
takeSnapshot =
  withSystemTempDirectory "tbshot" $ \tmp -> do
    let shotPath = tmp </> "shot.png"
    e <-
      try (readProcess (proc "grim" ["-s", "1", "-l", "1", "-g", "0,0 1024x40", shotPath]))
        :: IO (Either SomeException (ExitCode, BL.ByteString, BL.ByteString))
    case e of
      Left _ -> pure (Left ("grim failed" :: String))
      Right (ec, _stdout, _stderr) ->
        case ec of
          ExitFailure _ -> pure (Left ("grim failed" :: String))
          ExitSuccess -> do
            png <- BL.readFile shotPath
            case JP.decodePng (BL.toStrict png) of
              Left _ -> pure (Left "PNG decode failed")
              Right dyn ->
                let img = JP.convertRGBA8 dyn
                 in
                  if hasExpectedMarkers img
                    then pure (Right (JP.encodePng img))
                    else pure (Left "Expected marker colors not present (bar likely not rendered yet)")

hasExpectedMarkers :: JP.Image JP.PixelRGBA8 -> Bool
hasExpectedMarkers img =
  -- These solid colors come from test/data/appearance-test.css and are chosen
  -- specifically so we can detect when the bar has actually rendered.
  let centerBox = JP.PixelRGBA8 0x3a 0x3a 0x3a 0xff
      rightBox = JP.PixelRGBA8 0x3a 0x5a 0x7a 0xff
      centerCount = countColor img centerBox
      rightCount = countColor img rightBox
   in centerCount >= 200 && rightCount >= 50

countColor :: JP.Image JP.PixelRGBA8 -> JP.PixelRGBA8 -> Int
countColor img needle =
  let w = JP.imageWidth img
      h = JP.imageHeight img
   in
    length
      [ ()
      | y <- [0 .. h - 1]
      , x <- [0 .. w - 1]
      , JP.pixelAt img x y == needle
      ]

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
  :: MVar Context
  -> MVar (Either String BL.ByteString)
  -> MVar ExitCode
  -> IORef (Maybe BL.ByteString)
  -> Int
  -> IO ()
watchdogThread ctxVar resultVar doneVar lastShotRef usec = do
  threadDelay usec
  void $ tryPutMVar doneVar (ExitFailure 124)
  lastShot <- readIORef lastShotRef
  let payload =
        case lastShot of
          Just png -> Right png
          Nothing -> Left "Timed out waiting for Hyprland appearance snapshot"
  void $ tryPutMVar resultVar payload
  mCtx <- tryReadMVar ctxVar
  case mCtx of
    Nothing -> pure ()
    Just ctx -> void (try (exitTaffybar ctx) :: IO (Either SomeException ()))
  -- If the GTK loop doesn't exit promptly, force the process to end so the
  -- harness never hangs.
  threadDelay 2_000_000
  exitImmediately (ExitFailure 124)

requireExe :: String -> IO FilePath
requireExe name = do
  mexe <- findExecutable name
  maybe (fail (name ++ " not found on PATH")) makeAbsolute mexe

requireEnv :: String -> IO String
requireEnv name = do
  v <- lookupEnv name
  case v of
    Nothing -> die ("Required environment variable missing: " ++ name)
    Just s -> pure s

parseArgs :: [String] -> IO Args
parseArgs args =
  case args of
    ["--out", outPath, "--css", cssPath] -> pure Args { outFile = outPath, cssFile = cssPath }
    ["--css", cssPath, "--out", outPath] -> pure Args { outFile = outPath, cssFile = cssPath }
    _ -> fail "usage: taffybar-appearance-snap-hyprland --out OUT.png --css appearance-test.css"

die :: String -> IO a
die msg = do
  hPutStrLn stderr msg
  fail msg
