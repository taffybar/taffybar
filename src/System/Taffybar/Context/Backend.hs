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
    detectBackendFromGdk,
    prepareBackendEnvironment,

    -- * Discovery helpers
    discoverWaylandSocket,
    discoverHyprlandSignature,
  )
where

import Control.Exception.Enclosed (catchAny)
import Control.Monad
import Data.Char (toLower)
import Data.GI.Base (castTo)
import Data.List (isInfixOf, isPrefixOf, isSuffixOf, sortOn)
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import Data.Ord (Down (..))
import qualified Data.Text as T
import qualified GI.Gdk as Gdk
import qualified GI.GdkX11.Objects.X11Display as GdkX11
import qualified Network.Socket as NS
import System.Directory (doesPathExist, listDirectory)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath ((</>))
import System.Log.Logger (Priority (..), logM)
import System.Posix.Files (getFileStatus, isSocket)
import Text.Read (readMaybe)

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
      ok <- isSocketPath (runtime </> c)
      if ok then pure (Just c) else go cs

-- | Try to find a Hyprland instance signature in @XDG_RUNTIME_DIR/hypr/@.
--
-- Hyprland creates a directory named after its instance signature under
-- @$XDG_RUNTIME_DIR/hypr/@ containing live command/event sockets. Stale
-- instance directories can remain after a compositor exits, so lock files are
-- not sufficient evidence that Hyprland is the active session.
discoverHyprlandSignature :: FilePath -> IO (Maybe String)
discoverHyprlandSignature runtime = do
  let hyprDir = runtime </> "hypr"
  exists <- doesPathExist hyprDir
  if not exists
    then pure Nothing
    else do
      entries <- listDirectory hyprDir
      liveEntries <- filterM (isLiveHyprlandSignature . (hyprDir </>)) entries
      pure $ listToMaybe $ sortOn hyprlandSignatureSortKey liveEntries

hyprlandSignatureSortKey :: String -> (Down Integer, Down String)
hyprlandSignatureSortKey signature =
  ( Down $ fromMaybe (-1) $ hyprlandSignatureStartTime signature,
    Down signature
  )

hyprlandSignatureStartTime :: String -> Maybe Integer
hyprlandSignatureStartTime signature =
  case drop 1 $ dropWhile (/= '_') signature of
    "" -> Nothing
    rest -> readMaybe $ takeWhile (/= '_') rest

isSocketPath :: FilePath -> IO Bool
isSocketPath path =
  catchAny
    (isSocket <$> getFileStatus path)
    (const $ pure False)

isLiveHyprlandSignature :: FilePath -> IO Bool
isLiveHyprlandSignature dir =
  (||)
    <$> canConnectUnixSocket (dir </> ".socket.sock")
    <*> canConnectUnixSocket (dir </> ".socket2.sock")

canConnectUnixSocket :: FilePath -> IO Bool
canConnectUnixSocket path = do
  sock <- NS.socket NS.AF_UNIX NS.Stream NS.defaultProtocol
  ( do
      NS.connect sock (NS.SockAddrUnix path)
      NS.close sock
      pure True
    )
    `catchAny` \_ -> do
      void $ NS.close sock `catchAny` \_ -> pure ()
      pure False

envIsNonEmpty :: Maybe String -> Bool
envIsNonEmpty = maybe False (not . null)

envIsX11GdkBackend :: Maybe String -> Bool
envIsX11GdkBackend = maybe False ((== "x11") . map toLower)

envContainsWaylandDesktop :: Maybe String -> Bool
envContainsWaylandDesktop =
  maybe False $
    \value ->
      let lowered = map toLower value
       in "hyprland" `isInfixOf` lowered

waylandSocketAvailable :: FilePath -> Maybe String -> IO Bool
waylandSocketAvailable runtime mWaylandDisplay =
  case mWaylandDisplay of
    Just wl | not (null wl) -> isSocketPath (runtime </> wl)
    _ -> pure False

hyprlandSignatureAvailable :: FilePath -> Maybe String -> IO Bool
hyprlandSignatureAvailable runtime mSignature =
  case mSignature of
    Just sig | not (null sig) -> isLiveHyprlandSignature (runtime </> "hypr" </> sig)
    _ -> pure False

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
-- This function probes @XDG_RUNTIME_DIR@ only when the current process
-- context already points at Wayland, or when there is no explicit X display to
-- compete with. This avoids choosing a Wayland compositor merely because one
-- exists somewhere in the user's runtime directory.
prepareBackendEnvironment :: IO ()
prepareBackendEnvironment = do
  mRuntime <- lookupEnv "XDG_RUNTIME_DIR"
  mDisplay <- lookupEnv "DISPLAY"
  mSessionType <- lookupEnv "XDG_SESSION_TYPE"
  mGdkBackend <- lookupEnv "GDK_BACKEND"
  mCurrentDesktop <- lookupEnv "XDG_CURRENT_DESKTOP"
  mDesktopSession <- lookupEnv "DESKTOP_SESSION"
  rawHyprlandSignature <- lookupEnv "HYPRLAND_INSTANCE_SIGNATURE"
  rawWaylandDisplay <- lookupEnv "WAYLAND_DISPLAY"

  let hasDisplay = envIsNonEmpty mDisplay
      explicitlyRequestedX11 =
        envIsX11GdkBackend mGdkBackend

  currentWaylandOk <- case mRuntime of
    Just runtime -> waylandSocketAvailable runtime rawWaylandDisplay
    Nothing -> pure False
  currentHyprlandOk <- case mRuntime of
    Just runtime -> hyprlandSignatureAvailable runtime rawHyprlandSignature
    Nothing -> pure False
  discoveredHyprlandSignature <- case mRuntime of
    Just runtime | not explicitlyRequestedX11 -> discoverHyprlandSignature runtime
    _ -> pure Nothing

  let hasHyprlandEvidence =
        currentHyprlandOk
          || envIsNonEmpty rawHyprlandSignature
          || envIsNonEmpty discoveredHyprlandSignature
          || envContainsWaylandDesktop mCurrentDesktop
          || envContainsWaylandDesktop mDesktopSession
      staleSessionTypeClaimsX11 =
        mSessionType == Just "x11"
          && hasDisplay
          && not currentWaylandOk
          && not hasHyprlandEvidence
      explicitX11Session =
        explicitlyRequestedX11 || staleSessionTypeClaimsX11
      processContextExpectsWayland =
        currentWaylandOk
          || mSessionType == Just "wayland"
          || hasHyprlandEvidence
      shouldDiscoverAmbientWayland =
        not explicitX11Session && (processContextExpectsWayland || not hasDisplay)

  -- If the process environment identifies the active session as X11, trust it
  -- over ambient Wayland sockets in XDG_RUNTIME_DIR. A live WAYLAND_DISPLAY
  -- from the process environment is stronger evidence than XDG_SESSION_TYPE,
  -- because user shells can retain a stale session type.
  when explicitX11Session $ do
    unsetEnv "WAYLAND_DISPLAY"
    unsetEnv "HYPRLAND_INSTANCE_SIGNATURE"
    setEnv "GDK_BACKEND" "x11"
    logIO DEBUG "X11 session detected; ignoring ambient Wayland sockets"

  -- Discover and fix up WAYLAND_DISPLAY if it is missing, empty, or stale.
  repairedWaylandDisplay <- do
    case (mRuntime, rawWaylandDisplay) of
      _ | explicitX11Session -> pure Nothing
      (Just _, val) | currentWaylandOk -> pure val
      (Just runtime, _) | shouldDiscoverAmbientWayland -> do
        mSock <- discoverWaylandSocket runtime
        case mSock of
          Just sock -> do
            logIO INFO $ "Discovered wayland socket: " ++ sock
            setEnv "WAYLAND_DISPLAY" sock
            pure (Just sock)
          Nothing -> pure Nothing
      _ -> pure Nothing

  when (not explicitX11Session && envIsNonEmpty repairedWaylandDisplay) $ do
    setEnv "GDK_BACKEND" "wayland"
    when (mSessionType /= Just "wayland") $
      setEnv "XDG_SESSION_TYPE" "wayland"

  -- Discover and fix up HYPRLAND_INSTANCE_SIGNATURE if it is missing, empty, or stale.
  when (processContextExpectsWayland || envIsNonEmpty repairedWaylandDisplay) $ do
    raw <- lookupEnv "HYPRLAND_INSTANCE_SIGNATURE"
    case (mRuntime, raw) of
      (Just runtime, val) -> do
        currentOk <- hyprlandSignatureAvailable runtime val
        unless currentOk $ do
          case discoveredHyprlandSignature of
            Just sig -> do
              logIO INFO $ "Discovered Hyprland signature: " ++ sig
              setEnv "HYPRLAND_INSTANCE_SIGNATURE" sig
            Nothing -> pure ()
      _ -> pure ()

-- | Detect the backend from the display that GDK actually opened.
--
-- This should be preferred after @Gtk.init@. Before GTK/GDK initialization,
-- 'Gdk.displayGetDefault' usually returns 'Nothing', so callers still need
-- 'prepareBackendEnvironment' to steer GDK toward the intended display.
detectBackendFromGdk :: IO (Maybe Backend)
detectBackendFromGdk = do
  mDisplay <- Gdk.displayGetDefault
  case mDisplay of
    Nothing -> pure Nothing
    Just display -> do
      displayName <- Gdk.displayGetName display
      isX11 <- isJust <$> castTo GdkX11.X11Display display
      let selected =
            if isX11
              then BackendX11
              else BackendWayland
      logIO INFO $
        "Detected backend from GDK display "
          ++ T.unpack displayName
          ++ ": "
          ++ show selected
      pure $ Just selected

detectBackendFromEnvironment :: IO Backend
detectBackendFromEnvironment = do
  mRuntime <- lookupEnv "XDG_RUNTIME_DIR"
  mDisplay <- lookupEnv "DISPLAY"
  mSessionType <- lookupEnv "XDG_SESSION_TYPE"
  mWaylandDisplay <- lookupEnv "WAYLAND_DISPLAY"

  -- Validate the wayland socket.
  let mWaylandPath = do
        runtime <- mRuntime
        wl <- mWaylandDisplay
        guard (not (null runtime) && not (null wl))
        pure (runtime </> wl)

  waylandOk <- case mWaylandPath of
    Nothing -> pure False
    Just wlPath -> isSocketPath wlPath

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
  logIO INFO $ "Detected backend from environment: " ++ show selected
  pure selected

detectBackend :: IO Backend
detectBackend = do
  prepareBackendEnvironment
  mGdkBackend <- detectBackendFromGdk
  maybe detectBackendFromEnvironment pure mGdkBackend
