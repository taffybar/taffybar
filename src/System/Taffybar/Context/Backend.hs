-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      : System.Taffybar.Context.Backend
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan A. Malison
-- Stability   : unstable
-- Portability : unportable
--
-- Display-server backend detection for taffybar.
--
-- The 'detectBackend' function probes the runtime environment to decide
-- whether to use an X11 or Wayland backend, compensating for stale or
-- missing environment variables that are common when the @systemd
-- --user@ manager persists across login sessions.
module System.Taffybar.Context.Backend
  ( Backend (..),
    detectBackend,

    -- * Discovery helpers
    discoverWaylandSocket,
    discoverHyprlandSignature,
  )
where

import Control.Exception.Enclosed (catchAny)
import Control.Monad
import Data.List (isPrefixOf, isSuffixOf)
import System.Directory (doesPathExist, listDirectory)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath ((</>))
import System.Log.Logger (Priority (..), logM)
import System.Posix.Files (getFileStatus, isSocket)

logIO :: Priority -> String -> IO ()
logIO = logM "System.Taffybar.Context.Backend"

-- | Backend selected for the current taffybar process.
data Backend
  = -- | Use the X11 backend.
    BackendX11
  | -- | Use the Wayland backend.
    BackendWayland
  deriving (Eq, Show)

-- | Try to find a @wayland-*@ socket in the given runtime directory.
--
-- When the systemd user manager environment has a stale or empty
-- @WAYLAND_DISPLAY@, the actual socket may still exist.  This function
-- scans @XDG_RUNTIME_DIR@ for candidate sockets.
discoverWaylandSocket :: FilePath -> IO (Maybe String)
discoverWaylandSocket runtime = do
  entries <- listDirectory runtime
  let candidates =
        [ e
        | e <- entries,
          "wayland-" `isPrefixOf` e,
          not (".lock" `isSuffixOf` e)
        ]
  go candidates
  where
    go [] = pure Nothing
    go (c : cs) = do
      ok <-
        catchAny
          (isSocket <$> getFileStatus (runtime </> c))
          (const $ pure False)
      if ok then pure (Just c) else go cs

-- | Try to find a Hyprland instance signature in @XDG_RUNTIME_DIR/hypr/@.
--
-- Hyprland creates a directory named after its instance signature under
-- @$XDG_RUNTIME_DIR/hypr/@ containing @hyprland.lock@.
discoverHyprlandSignature :: FilePath -> IO (Maybe String)
discoverHyprlandSignature runtime = do
  let hyprDir = runtime </> "hypr"
  exists <- doesPathExist hyprDir
  if not exists
    then pure Nothing
    else do
      entries <- listDirectory hyprDir
      go hyprDir entries
  where
    go _ [] = pure Nothing
    go hyprDir (e : es) = do
      isSig <- doesPathExist (hyprDir </> e </> "hyprland.lock")
      if isSig then pure (Just e) else go hyprDir es

-- | Detect the display-server backend, compensating for stale or missing
-- environment variables.
--
-- The @systemd --user@ manager persists across login sessions, so its
-- environment can be stale in two ways:
--
-- * A leftover @WAYLAND_DISPLAY@ from a previous Wayland session points at a
--   socket that no longer exists (the original problem the socket check
--   addresses).
--
-- * @WAYLAND_DISPLAY@ and @HYPRLAND_INSTANCE_SIGNATURE@ are completely absent
--   or empty even though a Wayland compositor is running (e.g. after switching
--   from an X11 session).
--
-- This function discovers the real state by probing @XDG_RUNTIME_DIR@, fixes
-- up the process environment so downstream code sees consistent values, and
-- returns the appropriate 'Backend'.
detectBackend :: IO Backend
detectBackend = do
  mRuntime <- lookupEnv "XDG_RUNTIME_DIR"
  mDisplay <- lookupEnv "DISPLAY"
  mSessionType <- lookupEnv "XDG_SESSION_TYPE"

  -- Discover and fix up WAYLAND_DISPLAY if it is missing or empty.
  mWaylandDisplay <- do
    raw <- lookupEnv "WAYLAND_DISPLAY"
    case (mRuntime, raw) of
      (Just runtime, val) | maybe True null val -> do
        mSock <- discoverWaylandSocket runtime
        case mSock of
          Just sock -> do
            logIO INFO $ "Discovered wayland socket: " ++ sock
            setEnv "WAYLAND_DISPLAY" sock
            pure (Just sock)
          Nothing -> pure raw
      _ -> pure raw

  -- Discover and fix up HYPRLAND_INSTANCE_SIGNATURE if it is missing or empty.
  do
    raw <- lookupEnv "HYPRLAND_INSTANCE_SIGNATURE"
    case (mRuntime, raw) of
      (Just runtime, val) | maybe True null val -> do
        mSig <- discoverHyprlandSignature runtime
        case mSig of
          Just sig -> do
            logIO INFO $ "Discovered Hyprland signature: " ++ sig
            setEnv "HYPRLAND_INSTANCE_SIGNATURE" sig
          Nothing -> pure ()
      _ -> pure ()

  -- Validate the wayland socket.
  let mWaylandPath = do
        runtime <- mRuntime
        wl <- mWaylandDisplay
        guard (not (null runtime) && not (null wl))
        pure (runtime </> wl)

  waylandOk <- case mWaylandPath of
    Nothing -> pure False
    Just wlPath ->
      catchAny
        (isSocket <$> getFileStatus wlPath)
        (const $ pure False)

  -- Clean up the environment when falling back to X11.
  when (not waylandOk && maybe False (not . null) mDisplay) $ do
    unsetEnv "WAYLAND_DISPLAY"
    unsetEnv "HYPRLAND_INSTANCE_SIGNATURE"
    when (mSessionType == Just "wayland") $ setEnv "XDG_SESSION_TYPE" "x11"
    logIO DEBUG "Wayland socket not available; cleaned up environment for X11 backend"

  -- Fix XDG_SESSION_TYPE when selecting Wayland.
  when (waylandOk && mSessionType /= Just "wayland") $
    setEnv "XDG_SESSION_TYPE" "wayland"

  let selected = if waylandOk then BackendWayland else BackendX11
  logIO INFO $ "Detected backend: " ++ show selected
  pure selected
