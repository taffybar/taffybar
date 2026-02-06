{-# LANGUAGE OverloadedStrings #-}

-- Helper executable for the Hyprland (Wayland) appearance golden test.
--
-- This is intentionally a separate process from the Hspec runner because
-- 'startTaffybar' enters the GTK main loop (an FFI call) and is not reliably
-- interruptible by async exceptions. The Hspec test can always timeout/kill
-- this executable in CI.
module Main (main) where

import Control.Concurrent (MVar, forkIO, newEmptyMVar, threadDelay, takeMVar)
import Control.Concurrent.MVar (tryPutMVar, tryReadMVar)
import Control.Exception (SomeException, bracket, try)
import Control.Monad (unless, void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ask)
import Data.Default (def)
import Data.List (isInfixOf, isPrefixOf, sortOn)
import Data.Unique (newUnique)
import Data.Word (Word32)
import System.Directory
  ( createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  , findExecutable
  , listDirectory
  , makeAbsolute
  , removePathForcibly
  )
import System.Environment (getArgs, setEnv, unsetEnv)
import System.Exit (ExitCode (..), exitWith)
import System.FilePath (takeDirectory, (</>))
import System.IO (hPutStrLn, stderr)
import System.Posix.Files (getFileStatus, isSocket, setFileMode)
import System.Posix.Process (exitImmediately)
import System.Posix.Signals (signalProcess, sigKILL, sigTERM)
import System.Posix.Types (FileMode)
import System.Posix.Temp (mkdtemp)

import qualified Codec.Picture as JP
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import UnliftIO.Temporary (withSystemTempDirectory)

import System.Process.Typed
  ( Process
  , getPid
  , inherit
  , proc
  , readProcess
  , setStderr
  , setStdout
  , startProcess
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
import System.Taffybar.Util (postGUIASync)
import System.Taffybar.Widget.HyprlandWorkspaces
  ( HyprlandWindow (..)
  , HyprlandWorkspacesConfig (..)
  , hyprlandWorkspacesNew
  )
import System.Taffybar.WindowIcon (pixBufFromColor)

data Args = Args
  { outFile :: FilePath
  , cssFile :: FilePath
  }

main :: IO ()
main = do
  Args { outFile = outPath, cssFile = cssPath } <- parseArgs =<< getArgs

  -- Keep host session variables from leaking into this nested compositor run.
  unsetEnv "DISPLAY"
  unsetEnv "WAYLAND_DISPLAY"
  unsetEnv "HYPRLAND_INSTANCE_SIGNATURE"
  unsetEnv "XDG_RUNTIME_DIR"

  -- Reduce variability.
  setEnv "GDK_BACKEND" "wayland"
  setEnv "XDG_SESSION_TYPE" "wayland"
  setEnv "GDK_SCALE" "1"
  setEnv "GDK_DPI_SCALE" "1"
  setEnv "GTK_CSD" "0"
  setEnv "NO_AT_BRIDGE" "1"
  setEnv "GSETTINGS_BACKEND" "memory"

  ec <- withSystemTempDirectory "tb" $ \tmp -> do
    let homeDir = tmp </> "h"
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

    -- Hyprland's IPC sockets have a hard path length limit. TMPDIR is often long
    -- in CI / nix builds, so we always create a short XDG_RUNTIME_DIR under /tmp.
    withShortRuntimeDir $ \runtimeDir -> do
      setEnv "XDG_RUNTIME_DIR" runtimeDir

      let westonSock = "weston"
          westonSockPath = runtimeDir </> westonSock

      -- Start Weston headless as the parent Wayland compositor.
      westonExe <- requireExe "weston"
      let westonLog = tmp </> "weston.log"
          westonCfg =
            setStdout inherit $
              setStderr inherit $
                proc
                  westonExe
                  [ "--backend=headless"
                  , "--renderer=pixman"
                  , "--width=1024"
                  , "--height=200"
                  , "--socket=" ++ westonSock
                  , "--idle-time=0"
                  , "--no-config"
                  , "--log=" ++ westonLog
                  ]
      bracket
        (startProcess westonCfg)
        killProcessNoWait
        $ \_westonProc -> do
          waitForSocket 10_000_000 westonSockPath

          -- Start Hyprland nested on the parent compositor.
          hyprExe <- requireExe "Hyprland"
          hyprConf <- writeHyprlandConfig tmp
          setEnv "WAYLAND_DISPLAY" westonSock
          let hyprCfg =
                setStdout inherit $
                  setStderr inherit $
                    proc hyprExe ["--config", hyprConf]
          bracket
            (startProcess hyprCfg)
            killProcessNoWait
            $ \hyprProc -> do

              -- Wait for Hyprland runtime directory and IPC sockets.
              sig <- waitForHyprlandSignature 15_000_000 runtimeDir
              waitForSocket 15_000_000 (runtimeDir </> "hypr" </> sig </> ".socket.sock")
              waitForSocket 15_000_000 (runtimeDir </> "hypr" </> sig </> ".socket2.sock")

              -- Find Hyprland's display socket. It should create a new wayland-* socket in
              -- XDG_RUNTIME_DIR (weston uses a non-standard socket name to avoid collisions).
              hyprSock <- waitForWaylandSocket 15_000_000 runtimeDir
              setEnv "WAYLAND_DISPLAY" hyprSock
              setEnv "HYPRLAND_INSTANCE_SIGNATURE" sig

              runUnderHyprland hyprProc outPath cssPath

  exitWith ec

runUnderHyprland :: Process () () () -> FilePath -> FilePath -> IO ExitCode
runUnderHyprland hyprProc outPath cssPath = do
  ctxVar :: MVar Context <- newEmptyMVar
  resultVar :: MVar (Either String BL.ByteString) <- newEmptyMVar
  doneVar :: MVar ExitCode <- newEmptyMVar

  -- Writes the result out and requests bar shutdown.
  void $ forkIO $ finalizeThread ctxVar resultVar doneVar outPath

  -- Hard watchdog for CI stability: always tries to end the GTK loop.
  void $ forkIO $ watchdogThread hyprProc ctxVar resultVar doneVar 45_000_000

  barUnique <- newUnique

  let testRed :: Word32
      testRed = 0xFFE03A3A
      testGreen :: Word32
      testGreen = 0xFF3AE06A

      windowIconGetter size win =
        let t = map toLowerAscii (windowTitle win)
         in if "red" `isInfixOf` t
              then Just <$> pixBufFromColor size testRed
            else if "green" `isInfixOf` t
              then Just <$> pixBufFromColor size testGreen
              else pure Nothing

      sortByTitle = pure . sortOn (map toLowerAscii . windowTitle)

      wsCfg =
        (def :: HyprlandWorkspacesConfig)
          { labelSetter = const (pure "")
          , maxIcons = Just 2
          , minIcons = 2
          , iconSize = 16
          , getWindowIconPixbuf = windowIconGetter
          , iconSort = sortByTitle
          , widgetGap = 0
          , urgentWorkspaceState = False
          -- Show everything to avoid a blank widget if Hyprland reports only empty workspaces initially.
          , showWorkspaceFn = const True
          }

      barCfg =
        BarConfig
          { strutConfig =
              defaultStrutConfig
                { strutHeight = ExactSize 40
                , strutMonitor = Just 0
                , strutPosition = TopPos
                }
          , widgetSpacing = 0
          , startWidgets = [hyprlandWorkspacesNew wsCfg]
          , centerWidgets = []
          , endWidgets = []
          , barId = barUnique
          }

      cfg =
        def
          { dbusClientParam = Nothing
          , cssPaths = [cssPath]
          , getBarConfigsParam = pure [barCfg]
          , startupHook = startup ctxVar resultVar
          , errorMsg = Nothing
          }

  -- Blocks in Gtk.main until we request shutdown.
  startTaffybar cfg

  -- Should have been set by finalizeThread or watchdogThread.
  takeMVar doneVar

startup :: MVar Context -> MVar (Either String BL.ByteString) -> TaffyIO ()
startup ctxVar resultVar = do
  ctx <- ask
  liftIO $ void (tryPutMVar ctxVar ctx)

  -- Create a couple of deterministic test windows so the workspaces widget has
  -- something to render. These windows are outside the screenshot region (bar
  -- is 40px tall).
  liftIO $ postGUIASync createTestWindows

  -- Poll for a screenshot until the expected test icons appear.
  liftIO $ void $ forkIO pollLoop
  where
    pollLoop = do
      done <- tryReadMVar resultVar
      case done of
        Just _ -> pure ()
        Nothing -> do
          trySnapshot resultVar
          threadDelay 100_000
          pollLoop

createTestWindows :: IO ()
createTestWindows = do
  w1 <- Gtk.windowNew Gtk.WindowTypeToplevel
  Gtk.windowSetTitle w1 ("taffybar-test-red" :: T.Text)
  Gtk.windowSetDefaultSize w1 150 100
  Gtk.widgetShowAll w1

  w2 <- Gtk.windowNew Gtk.WindowTypeToplevel
  Gtk.windowSetTitle w2 ("taffybar-test-green" :: T.Text)
  Gtk.windowSetDefaultSize w2 150 100
  Gtk.widgetShowAll w2

trySnapshot :: MVar (Either String BL.ByteString) -> IO ()
trySnapshot resultVar = do
  -- Screenshot the bar region. We keep this constant to make goldens stable.
  out <- (withSystemTempDirectory "tbshot" $ \tmp -> do
    let shotPath = tmp </> "shot.png"
    e <-
      try (readProcess (proc "grim" ["-s", "1", "-l", "1", "-g", "0,0 1024x40", shotPath]))
        :: IO (Either SomeException (ExitCode, BL.ByteString, BL.ByteString))
    case e of
      Left _ -> pure (Left ("grim failed" :: String))
      Right (ec, _stdout, _stderr) ->
        case ec of
          ExitFailure _ -> pure (Left ("grim failed" :: String))
          ExitSuccess -> Right <$> BL.readFile shotPath
    ) :: IO (Either String BL.ByteString)
  case out of
    Left _ -> pure ()
    Right png ->
      case JP.decodePng (BL.toStrict png) of
        Left _ -> pure ()
        Right dyn ->
          let img = JP.convertRGBA8 dyn
           in when (imageHasTestIcons img) $
                void (tryPutMVar resultVar (Right (JP.encodePng img)))

imageHasTestIcons :: JP.Image JP.PixelRGBA8 -> Bool
imageHasTestIcons img =
  let tol :: Int
      tol = 40
      near (JP.PixelRGBA8 r g b a) (tr, tg, tb) =
        a > 200 &&
        abs (fromIntegral r - tr) <= tol &&
        abs (fromIntegral g - tg) <= tol &&
        abs (fromIntegral b - tb) <= tol
      redTarget :: (Int, Int, Int)
      redTarget = (224, 58, 58)
      greenTarget :: (Int, Int, Int)
      greenTarget = (58, 224, 106)
      w = JP.imageWidth img
      h = JP.imageHeight img
      go y seenR seenG
        | y >= h = seenR && seenG
        | otherwise =
            let goX x sr sg
                  | x >= w = go (y + 1) sr sg
                  | otherwise =
                      let px = JP.pixelAt img x y
                          sr' = sr || near px redTarget
                          sg' = sg || near px greenTarget
                       in if sr' && sg'
                            then True
                            else goX (x + 1) sr' sg'
             in goX 0 seenR seenG
   in go 0 False False

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
watchdogThread hyprProc ctxVar resultVar doneVar usec = do
  threadDelay usec
  didSet <- tryPutMVar resultVar (Left "Timed out waiting for Hyprland appearance snapshot")
  when didSet $ void $ tryPutMVar doneVar (ExitFailure 124)
  mCtx <- tryReadMVar ctxVar
  case mCtx of
    Nothing -> pure ()
    Just ctx -> void (try (exitTaffybar ctx) :: IO (Either SomeException ()))
  killProcessNoWait hyprProc
  -- If the GTK loop doesn't exit promptly, force the process to end so the
  -- Hspec runner never hangs.
  threadDelay 2_000_000
  exitImmediately (ExitFailure 124)

killProcessNoWait :: Process () () () -> IO ()
killProcessNoWait p = do
  mpid <- getPid p
  case mpid of
    Nothing -> pure ()
    Just pid -> do
      void (try (signalProcess sigTERM pid) :: IO (Either SomeException ()))
      threadDelay 250_000
      void (try (signalProcess sigKILL pid) :: IO (Either SomeException ()))

requireExe :: String -> IO FilePath
requireExe name = do
  mexe <- findExecutable name
  maybe (fail (name ++ " not found on PATH")) makeAbsolute mexe

writeHyprlandConfig :: FilePath -> IO FilePath
writeHyprlandConfig dir = do
  let path = dir </> "hyprland.conf"
  writeFile path $
    unlines
      [ "monitor=,1024x200@60,0x0,1"
      , "misc {"
      , "  disable_hyprland_logo = true"
      , "  disable_splash_rendering = true"
      , "}"
      , "animations {"
      , "  enabled = false"
      , "}"
      , "decoration {"
      , "  rounding = 0"
      , "  blur { enabled = false }"
      , "}"
      , "debug {"
      , "  disable_logs = false"
      , "}"
      ]
  pure path

waitForSocket :: Int -> FilePath -> IO ()
waitForSocket maxUsec path = do
  let stepUsec = 50_000
      maxSteps = maxUsec `div` stepUsec
  ok <- go 0 maxSteps
  unless ok $ fail ("Timed out waiting for socket: " ++ path)
  where
    go n maxSteps' = do
      exists <- doesFileExist path
      sock <- if exists then isSocket <$> getFileStatus path else pure False
      if sock
        then pure True
        else if n >= maxSteps'
          then pure False
          else threadDelay 50_000 >> go (n + 1) maxSteps'

waitForWaylandSocket :: Int -> FilePath -> IO FilePath
waitForWaylandSocket maxUsec runtimeDir = do
  let maxSteps = maxUsec `div` 50_000
  ok <- go 0 maxSteps
  maybe (fail "Timed out waiting for Hyprland wayland socket") pure ok
  where
    go n maxSteps' = do
      sockets <- listWaylandSockets runtimeDir
      case sockets of
        (s:_) -> pure (Just s)
        [] ->
          if n >= maxSteps'
            then pure Nothing
            else threadDelay 50_000 >> go (n + 1) maxSteps'

listWaylandSockets :: FilePath -> IO [FilePath]
listWaylandSockets runtimeDir = do
  entries <- listDirectory runtimeDir
  let candidates = filter ("wayland-" `isPrefixOf`) entries
  existing <- filterMIO (\n -> pathIsSocket (runtimeDir </> n)) candidates
  pure (sortOn id existing)

waitForHyprlandSignature :: Int -> FilePath -> IO String
waitForHyprlandSignature maxUsec runtimeDir = do
  let maxSteps = maxUsec `div` 50_000
      hyprDir = runtimeDir </> "hypr"
  ok <- go hyprDir 0 maxSteps
  case ok of
    Nothing -> fail "Timed out waiting for HYPRLAND_INSTANCE_SIGNATURE directory"
    Just sig -> pure sig
  where
    go hyprDir n maxSteps' = do
      exists <- doesDirectoryExist hyprDir
      if not exists
        then retry hyprDir n maxSteps'
        else do
          sigs <- listDirectory hyprDir
          case sigs of
            [] -> retry hyprDir n maxSteps'
            _ ->
              case sortOn id sigs of
                (sig:_) -> pure (Just sig)
                [] -> retry hyprDir n maxSteps'

    retry hyprDir n maxSteps' =
      if n >= maxSteps'
        then pure Nothing
        else threadDelay 50_000 >> go hyprDir (n + 1) maxSteps'

setPrivateDir :: FilePath -> IO ()
setPrivateDir dir = do
  setFileMode dir privateDirMode
  where
    privateDirMode :: FileMode
    privateDirMode = 0o700

withShortRuntimeDir :: (FilePath -> IO a) -> IO a
withShortRuntimeDir action = bracket acquire removePathForcibly action
  where
    acquire = do
      dir <- mkdtemp "/tmp/tb-rt-XXXXXX"
      setPrivateDir dir
      pure dir

pathIsSocket :: FilePath -> IO Bool
pathIsSocket p = do
  exists <- doesFileExist p
  if not exists then pure False else isSocket <$> getFileStatus p

filterMIO :: (a -> IO Bool) -> [a] -> IO [a]
filterMIO _ [] = pure []
filterMIO p (x:xs) = do
  ok <- p x
  rest <- filterMIO p xs
  pure (if ok then x : rest else rest)

toLowerAscii :: Char -> Char
toLowerAscii c
  | 'A' <= c && c <= 'Z' = toEnum (fromEnum c + 32)
  | otherwise = c

parseArgs :: [String] -> IO Args
parseArgs args =
  case args of
    ["--out", outPath, "--css", cssPath] -> pure Args { outFile = outPath, cssFile = cssPath }
    ["--css", cssPath, "--out", outPath] -> pure Args { outFile = outPath, cssFile = cssPath }
    _ -> fail "usage: taffybar-appearance-snap-hyprland --out OUT.png --css appearance-test.css"
