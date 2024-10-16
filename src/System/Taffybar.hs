{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
-----------------------------------------------------------------------------
-- |
-- Module      : System.Taffybar
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan A. Malison
-- Stability   : unstable
-- Portability : unportable
-----------------------------------------------------------------------------
module System.Taffybar
  (
  -- | Taffybar is a system status bar meant for use with window managers like
  -- "XMonad" and i3wm. Taffybar is somewhat similar to xmobar, but it opts to use
  -- more heavy weight GUI in the form of GTK rather than the mostly textual
  -- approach favored by the latter. This allows it to provide features like an
  -- SNI system tray, and a workspace widget with window icons.
  --

  -- * Configuration
  -- |
  -- The interface that Taffybar provides to the end user is roughly as follows:
  -- you give Taffybar a list of ('TaffyIO' actions that build) GTK widgets and
  -- it renders them in a horizontal bar for you (taking care of ugly details
  -- like reserving strut space so that window managers don't put windows over
  -- it).
  --
  -- The config file in which you specify the GTK widgets to render is just a
  -- Haskell source file which is used to produce a custom executable with the
  -- desired set of widgets. This approach requires that Taffybar be installed
  -- as a Haskell library (not merely as an executable), and that the GHC
  -- compiler be available for recompiling the configuration. The upshot of this
  -- approach is that Taffybar's behavior and widget set are not limited to the
  -- set of widgets provided by the library, because custom code and widgets can
  -- be provided to Taffybar for instantiation and execution.
  --
  -- The following code snippet is a simple example of what a Taffybar
  -- configuration might look like (also see "System.Taffybar.Example"):
  --
  -- > {-# LANGUAGE OverloadedStrings #-}
  -- > import Data.Default (def)
  -- > import System.Taffybar
  -- > import System.Taffybar.Information.CPU
  -- > import System.Taffybar.SimpleConfig
  -- > import System.Taffybar.Widget
  -- > import System.Taffybar.Widget.Generic.Graph
  -- > import System.Taffybar.Widget.Generic.PollingGraph
  -- >
  -- > cpuCallback = do
  -- >   (_, systemLoad, totalLoad) <- cpuLoad
  -- >   return [ totalLoad, systemLoad ]
  -- >
  -- > main = do
  -- >   let cpuCfg = def
  -- >                  { graphDataColors = [ (0, 1, 0, 1), (1, 0, 1, 0.5)]
  -- >                  , graphLabel = Just "cpu"
  -- >                  }
  -- >       clock = textClockNewWith def
  -- >       cpu = pollingGraphNew cpuCfg 0.5 cpuCallback
  -- >       workspaces = workspacesNew def
  -- >       simpleConfig = def
  -- >                        { startWidgets = [ workspaces ]
  -- >                        , endWidgets = [ sniTrayNew, clock, cpu ]
  -- >                        }
  -- >   simpleTaffybar simpleConfig
  --
  -- This configuration creates a bar with four widgets. On the left is a widget
  -- that shows information about the workspace configuration. The rightmost
  -- widget is the system tray, with a clock and then a CPU graph.
  --
  -- The CPU widget plots two graphs on the same widget: total CPU use in green
  -- and then system CPU use in a kind of semi-transparent purple on top of the
  -- green.
  --
  -- It is important to note that the widget lists are __not__ @'GI.Gtk.Widget'@. They are
  -- actually @'TaffyIO' 'GI.Gtk.Widget'@ since the bar needs to construct them after
  -- performing some GTK initialization.

    getTaffyFile

  -- ** Colors
  --
  -- | Although Taffybar is based on GTK, it ignores your GTK theme. The default
  -- theme that it uses lives at
  -- https://github.com/taffybar/taffybar/blob/master/taffybar.css You can alter
  -- this theme by editing @~\/.config\/taffybar\/taffybar.css@ to your liking.
  -- For an idea of the customizations you can make, see
  -- <https://live.gnome.org/GnomeArt/Tutorials/GtkThemes>.


  -- * Taffybar and DBus
  --
  -- | Taffybar has a strict dependency on "DBus", so you must ensure that the DBus daemon is
  -- started before starting Taffybar.
  --
  -- * If you start your window manager using a graphical login manager like @gdm@
  -- or @kdm@, DBus should be started automatically for you.
  --
  -- * If you start xmonad with a different graphical login manager that does
  -- not start DBus for you automatically, put the line
  -- @eval \`dbus-launch --auto-syntax\`@ into your @~\/.xsession@ *before* xmonad and taffybar are
  -- started. This command sets some environment variables that the two must
  -- agree on.
  --
  -- * If you start xmonad via @startx@ or a similar command, add the
  -- above command to @~\/.xinitrc@
  --
  -- * System tray compatability
  --
  -- "System.Taffybar.Widget.SNITray" only supports the newer
  -- StatusNotifierItem (SNI) protocol; older xembed applets will not work.
  -- AppIndicator is also a valid implementation of SNI.
  --
  -- Additionally, this module does not handle recognising new tray applets.
  -- Instead it is necessary to run status-notifier-watcher from the
  -- [status-notifier-item](https://github.com/taffybar/status-notifier-item)
  -- package early on system startup.
  -- In case this is not possible, the alternative widget
  -- 'System.Taffybar.Widget.SNITray.sniTrayThatStartsWatcherEvenThoughThisIsABadWayToDoIt' is available, but
  -- this may not necessarily be able to pick up everything.

  -- * Starting
  ,  startTaffybar

  -- ** Using Dyre
  , dyreTaffybar
  , dyreTaffybarMain
  , taffybarDyreParams
  ) where

import qualified Config.Dyre as Dyre
import qualified Config.Dyre.Params as Dyre
import           Control.Exception ( finally )
import           Control.Monad
import qualified Data.GI.Gtk.Threading as GIThreading
import qualified Data.Text as T
import qualified GI.Gdk as Gdk
import qualified GI.Gtk as Gtk
import           Graphics.X11.Xlib.Misc ( initThreads )
import           System.Directory
import           System.Environment.XDG.BaseDir ( getUserConfigFile )
import           System.Exit ( exitFailure )
import           System.FilePath ( (</>) )
import qualified System.IO as IO
import           System.Log.Logger
import           System.Taffybar.Context
import           System.Taffybar.Hooks
import           System.Taffybar.Util ( onSigINT )

import           Paths_taffybar ( getDataDir )

-- | The parameters that are passed to Dyre when taffybar is invoked with
-- 'dyreTaffybar'.
taffybarDyreParams =
  (Dyre.newParams "taffybar" dyreTaffybarMain showError)
  { Dyre.ghcOpts = ["-threaded", "-rtsopts"]
  , Dyre.rtsOptsHandling = Dyre.RTSAppend ["-I0", "-V0"]
  }

-- | Use Dyre to configure and start Taffybar. This will automatically recompile
-- Taffybar whenever there are changes to your @taffybar.hs@ configuration file.
dyreTaffybar :: TaffybarConfig -> IO ()
dyreTaffybar = Dyre.wrapMain taffybarDyreParams

showError :: TaffybarConfig -> String -> TaffybarConfig
showError cfg msg = cfg { errorMsg = Just msg }

-- | The main function that Dyre should run. This is used in 'taffybarDyreParams'.
dyreTaffybarMain :: TaffybarConfig -> IO ()
dyreTaffybarMain cfg =
  case errorMsg cfg of
    Nothing -> startTaffybar cfg
    Just err -> do
      IO.hPutStrLn IO.stderr ("Error: " ++ err)
      exitFailure

getDataFile :: String -> IO FilePath
getDataFile name = do
  dataDir <- getDataDir
  return (dataDir </> name)

startCSS :: [FilePath] -> IO Gtk.CssProvider
startCSS cssFilePaths = do
  -- Override the default GTK theme path settings.  This causes the
  -- bar (by design) to ignore the real GTK theme and just use the
  -- provided minimal theme to set the background and text colors.
  -- Users can override this default.
  taffybarProvider <- Gtk.cssProviderNew

  let loadIfExists filePath =
        doesFileExist filePath >>=
        flip when (Gtk.cssProviderLoadFromPath taffybarProvider (T.pack filePath))

  mapM_ loadIfExists cssFilePaths

  Just scr <- Gdk.screenGetDefault
  Gtk.styleContextAddProviderForScreen scr taffybarProvider 800
  return taffybarProvider

-- | Locates full the 'FilePath' of the given Taffybar config file.
-- The [XDG Base Directory](https://specifications.freedesktop.org/basedir-spec/latest/) convention is used, meaning that config files are usually in @~\/.config\/taffybar@.
getTaffyFile :: String -> IO FilePath
getTaffyFile = getUserConfigFile "taffybar"

getDefaultCSSPaths :: IO [FilePath]
getDefaultCSSPaths = do
  defaultUserConfig <- getTaffyFile "taffybar.css"
  return [defaultUserConfig]

-- | Start Taffybar with the provided 'TaffybarConfig'. This function will not
-- handle recompiling taffybar automatically when @taffybar.hs@ is updated. If you
-- would like this feature, use 'dyreTaffybar' instead. If automatic
-- recompilation is handled by another mechanism such as stack or a custom user
-- script or not desired for some reason, it is perfectly fine (and probably
-- better) to use this function.
startTaffybar :: TaffybarConfig -> IO ()
startTaffybar config = do
  updateGlobalLogger "" removeHandler
  setTaffyLogFormatter "System.Taffybar"
  setTaffyLogFormatter "StatusNotifier"
  _ <- initThreads
  _ <- Gtk.init Nothing
  GIThreading.setCurrentThreadAsGUIThread
  defaultCSS <- getDataFile "taffybar.css"
  cssPathsToLoad <-
    if null $ cssPaths config
    then getDefaultCSSPaths
    else return $ cssPaths config
  _ <- startCSS $ defaultCSS:cssPathsToLoad
  context <- buildContext config

  Gtk.main
    `finally` logTaffy DEBUG "Finished main loop"
    `onSigINT` do
      logTaffy INFO "Interrupted"
      exitTaffybar context

  logTaffy DEBUG "Exited normally"

logTaffy :: Priority -> String -> IO ()
logTaffy = logM "System.Taffybar"
