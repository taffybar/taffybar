{-# LANGUAGE OverloadedStrings #-}

-- Helper executable for the Hyprland (Wayland) appearance golden test.
--
-- This is intentionally a separate process from the Hspec runner because
-- 'startTaffybar' enters the GTK main loop (an FFI call) and is not reliably
-- interruptible by async exceptions. The VM harness can always timeout/kill
-- this executable in CI.
module Main (main) where

import qualified Codec.Picture as JP
import Control.Concurrent (MVar, forkIO, newEmptyMVar, takeMVar, threadDelay)
import Control.Concurrent.MVar (tryPutMVar, tryReadMVar)
import Control.Exception (SomeException, try)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ask)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Default (def)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.Text as T
import Data.Unique (Unique, newUnique)
import qualified GI.Gtk as Gtk
import Graphics.UI.GIGtkStrut
  ( StrutConfig (..),
    StrutPosition (TopPos),
    StrutSize (ExactSize),
    defaultStrutConfig,
  )
import System.Directory
  ( createDirectoryIfMissing,
    findExecutable,
    makeAbsolute,
  )
import System.Environment (getArgs, lookupEnv, setEnv, unsetEnv)
import System.Exit (ExitCode (..), exitWith)
import System.FilePath (takeDirectory, (</>))
import System.IO (hPutStrLn, stderr)
import System.Posix.Process (exitImmediately)
import System.Process.Typed
  ( proc,
    readProcess,
  )
import System.Taffybar (startTaffybar)
import System.Taffybar.Context
  ( BarConfig (..),
    BarLevelConfig (..),
    Context (..),
    TaffyIO,
    TaffybarConfig (..),
    exitTaffybar,
  )
import qualified System.Taffybar.Widget.Workspaces.Channel as ChannelWorkspaces
import UnliftIO.Temporary (withSystemTempDirectory)

data Args = Args
  { outFile :: FilePath,
    cssFile :: FilePath,
    layoutMode :: LayoutMode,
    workspaceWidgetMode :: WorkspaceWidgetMode
  }

data LayoutMode = LayoutLegacy | LayoutLevels
  deriving (Eq, Show)

data WorkspaceWidgetMode
  = UseLegacyWorkspaces
  | UseChannelWorkspaces
  deriving (Eq, Show)

main :: IO ()
main = do
  Args
    { outFile = outPath,
      cssFile = cssPath,
      layoutMode = mode,
      workspaceWidgetMode = wsMode
    } <-
    parseArgs =<< getArgs

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

    runUnderHyprland outPath cssPath mode wsMode

  exitWith ec

runUnderHyprland ::
  FilePath ->
  FilePath ->
  LayoutMode ->
  WorkspaceWidgetMode ->
  IO ExitCode
runUnderHyprland outPath cssPath mode wsMode = do
  ctxVar :: MVar Context <- newEmptyMVar
  resultVar :: MVar (Either String BL.ByteString) <- newEmptyMVar
  doneVar :: MVar ExitCode <- newEmptyMVar
  lastShotRef :: IORef (Maybe BL.ByteString) <- newIORef Nothing

  -- Writes the result out and requests bar shutdown.
  void $ forkIO $ finalizeThread ctxVar resultVar doneVar outPath

  -- Hard watchdog for CI stability: always tries to end the GTK loop.
  void $ forkIO $ watchdogThread ctxVar resultVar doneVar lastShotRef 30_000_000

  barUnique <- newUnique

  let barCfg = buildBarConfig barUnique mode wsMode

      cfg =
        def
          { dbusClientParam = Nothing,
            cssPaths = [cssPath],
            getBarConfigsParam = pure [barCfg],
            startupHook = scheduleSnapshot ctxVar resultVar lastShotRef mode,
            errorMsg = Nothing
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

buildBarConfig :: Unique -> LayoutMode -> WorkspaceWidgetMode -> BarConfig
buildBarConfig barUnique mode wsMode =
  case mode of
    LayoutLegacy ->
      BarConfig
        { strutConfig = baseStrutConfig 40,
          widgetSpacing = 8,
          startWidgets = [startWidget],
          centerWidgets = [testBoxWidget "test-center-box" 200 20],
          endWidgets =
            [ testPillBoxWidget "test-pill" 52 20,
              testPillBoxWidget "test-pill" 46 20,
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
                  { levelStartWidgets = [startWidget],
                    levelCenterWidgets = [testBoxWidget "test-center-box" 200 20],
                    levelEndWidgets =
                      [ testPillBoxWidget "test-pill" 52 20,
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
  where
    baseStrutConfig h =
      defaultStrutConfig
        { strutHeight = ExactSize h,
          strutMonitor = Just 0,
          strutPosition = TopPos
        }
    startWidget =
      case wsMode of
        UseLegacyWorkspaces -> testPillBoxWidget "test-pill" 56 20
        UseChannelWorkspaces ->
          ChannelWorkspaces.workspacesNew $
            ChannelWorkspaces.defaultWorkspacesConfig
              { ChannelWorkspaces.labelSetter = const (pure ""),
                ChannelWorkspaces.maxIcons = Just 1,
                ChannelWorkspaces.minIcons = 1,
                ChannelWorkspaces.showWorkspaceFn = const True
              }

scheduleSnapshot :: MVar Context -> MVar (Either String BL.ByteString) -> IORef (Maybe BL.ByteString) -> LayoutMode -> TaffyIO ()
scheduleSnapshot ctxVar resultVar lastShotRef mode = do
  ctx <- ask
  liftIO $ void (tryPutMVar ctxVar ctx)

  -- Delay slightly to give the compositor time to map the layer-surface and
  -- for widgets to render.
  liftIO $ void $ forkIO $ do
    threadDelay 2_000_000
    -- Require two consecutive identical frames to avoid capturing during
    -- initial GTK/compositor settling (animations, late mapping, etc).
    takeSnapshotWithRetries lastShotRef resultVar mode 60

takeSnapshotWithRetries :: IORef (Maybe BL.ByteString) -> MVar (Either String BL.ByteString) -> LayoutMode -> Int -> IO ()
takeSnapshotWithRetries _ resultVar _ 0 =
  void $ tryPutMVar resultVar (Left "Failed to capture Hyprland appearance snapshot")
takeSnapshotWithRetries lastShotRef resultVar mode n = do
  done <- tryReadMVar resultVar
  case done of
    Just _ -> pure ()
    Nothing -> do
      shot <- takeSnapshot mode
      case shot of
        Left _ -> do
          threadDelay 200_000
          takeSnapshotWithRetries lastShotRef resultVar mode (n - 1)
        Right encoded -> do
          prev <- readIORef lastShotRef
          writeIORef lastShotRef (Just encoded)
          case prev of
            Just prevEncoded
              | prevEncoded == encoded ->
                  void $ tryPutMVar resultVar (Right encoded)
            _ -> do
              threadDelay 200_000
              takeSnapshotWithRetries lastShotRef resultVar mode (n - 1)

takeSnapshot :: LayoutMode -> IO (Either String BL.ByteString)
takeSnapshot mode =
  withSystemTempDirectory "tbshot" $ \tmp -> do
    let shotPath = tmp </> "shot.png"
        shotHeight :: Int
        shotHeight =
          case mode of
            LayoutLegacy -> 40
            LayoutLevels -> 72
    e <-
      try (readProcess (proc "grim" ["-s", "1", "-l", "1", "-g", "0,0 1024x" ++ show shotHeight, shotPath])) ::
        IO (Either SomeException (ExitCode, BL.ByteString, BL.ByteString))
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
                 in if hasExpectedMarkers mode img
                      then pure (Right (JP.encodePng img))
                      else pure (Left "Expected marker colors not present (bar likely not rendered yet)")

hasExpectedMarkers :: LayoutMode -> JP.Image JP.PixelRGBA8 -> Bool
hasExpectedMarkers mode img =
  -- These solid colors come from test/data/appearance-test.css and are chosen
  -- specifically so we can detect when the bar has actually rendered.
  let centerBox = JP.PixelRGBA8 0x3a 0x3a 0x3a 0xff
      rightBox = JP.PixelRGBA8 0x3a 0x5a 0x7a 0xff
      level2Center = JP.PixelRGBA8 0x3a 0x7a 0x5a 0xff
      centerCount = countColor img centerBox
      rightCount = countColor img rightBox
      level2Count = countColor img level2Center
   in case mode of
        LayoutLegacy -> centerCount >= 200 && rightCount >= 50
        LayoutLevels -> centerCount >= 200 && rightCount >= 50 && level2Count >= 100

countColor :: JP.Image JP.PixelRGBA8 -> JP.PixelRGBA8 -> Int
countColor img needle =
  let w = JP.imageWidth img
      h = JP.imageHeight img
   in length
        [ ()
        | y <- [0 .. h - 1],
          x <- [0 .. w - 1],
          JP.pixelAt img x y == needle
        ]

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
  MVar Context ->
  MVar (Either String BL.ByteString) ->
  MVar ExitCode ->
  IORef (Maybe BL.ByteString) ->
  Int ->
  IO ()
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
  let layoutMode =
        if "--levels" `elem` args
          then LayoutLevels
          else LayoutLegacy
      workspaceWidgetMode =
        if "--channel-workspaces" `elem` args
          then UseChannelWorkspaces
          else UseLegacyWorkspaces
      argsSansFlags =
        filter
          (`notElem` ["--levels", "--channel-workspaces"])
          args
   in case argsSansFlags of
        ["--out", outPath, "--css", cssPath] ->
          pure
            Args
              { outFile = outPath,
                cssFile = cssPath,
                layoutMode = layoutMode,
                workspaceWidgetMode = workspaceWidgetMode
              }
        ["--css", cssPath, "--out", outPath] ->
          pure
            Args
              { outFile = outPath,
                cssFile = cssPath,
                layoutMode = layoutMode,
                workspaceWidgetMode = workspaceWidgetMode
              }
        _ ->
          fail
            "usage: taffybar-appearance-snap-hyprland --out OUT.png --css appearance-test.css [--levels] [--channel-workspaces]"

die :: String -> IO a
die msg = do
  hPutStrLn stderr msg
  fail msg
