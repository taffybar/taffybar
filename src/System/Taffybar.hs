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
  -- XMonad and i3wm. Taffybar is somewhat similar to xmobar, but it opts to use
  -- more heavy weight GUI in the form of gtk+ rather than the mostly textual
  -- approach favored by the latter. This allows it to provide features like an
  -- SNI system tray, and a workspace widget with window icons.
  --

  -- * Config File
  -- |
  -- The interface that taffybar provides to the end user is roughly as follows:
  -- you give Taffybar a list of ([Taffy]IO actions that build) gtk+ widgets and
  -- it renders them in a horizontal bar for you (taking care of ugly details
  -- like reserving strut space so that window managers don't put windows over
  -- it).
  --
  -- The config file in which you specify the gtk+ widgets to render is just a
  -- Haskell source file which is used to produce a custom executable with the
  -- desired set of widgets. This approach requires that taffybar be installed
  -- as a haskell library (not merely as an executable), and that the ghc
  -- compiler be available for recompiling the configuration. The upshot of this
  -- approach is that taffybar's behavior and widget set are not limited to the
  -- set of widgets provided by the library, because custom code and widgets can
  -- be provided to taffybar for instantiation and execution.
  --
  -- The following code snippet is a simple example of what a taffybar
  -- configuration might look like (also see "System.Taffybar.Example"):
  --
  -- > {-# LANGUAGE OverloadedStrings #-}
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
  -- >   let cpuCfg = defaultGraphConfig { graphDataColors = [ (0, 1, 0, 1), (1, 0, 1, 0.5)]
  -- >                                   , graphLabel = Just "cpu"
  -- >                                   }
  -- >       clock = textClockNewWith defaultClockConfig
  -- >       cpu = pollingGraphNew cpuCfg 0.5 cpuCallback
  -- >       workspaces = workspacesNew defaultWorkspacesConfig
  -- >       simpleConfig = defaultSimpleTaffyConfig
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
  -- It is important to note that the widget lists are *not* [Widget]. They are
  -- actually [TaffyIO Widget] since the bar needs to construct them after
  -- performing some GTK initialization.
  --
  -- * Taffybar and DBus
  --
  -- | Taffybar has a strict dependency on dbus, so you must ensure that it is
  -- started before starting taffybar.
  --
  -- * If you start your window manager using a graphical login manager like gdm
  -- or kdm, DBus should be started automatically for you.
  --
  -- * If you start xmonad with a different graphical login manager that does
  -- not start DBus for you automatically, put the line @eval \`dbus-launch
  -- --auto-syntax\`@ into your ~\/.xsession *before* xmonad and taffybar are
  -- started. This command sets some environment variables that the two must
  -- agree on.
  --
  -- * If you start xmonad via @startx@ or a similar command, add the
  -- above command to ~\/.xinitrc
  --
  -- * System tray compatability
  --
  -- | "System.Taffybar.Widget.SNITray" only supports the newer
  -- StatusNotifierItem (SNI) protocol; older xembed applets will not work.
  -- AppIndicator is also a valid implementation of SNI.
  --
  -- Additionally, this module does not handle recognising new tray applets.
  -- Instead it is necessary to run status-notifier-watcher from the
  -- [status-notifier-item](https://github.com/taffybar/status-notifier-item)
  -- package early on system startup.
  -- In case this is not possible, the alternative widget
  -- sniTrayThatStartsWatcherEvenThoughThisIsABadWayToDoIt is available, but
  -- this may not necessarily be able to pick up everything.

  -- * Colors
  --
  -- | While taffybar is based on GTK+, it ignores your GTK+ theme. The default
  -- theme that it uses lives at
  -- https://github.com/taffybar/taffybar/blob/master/taffybar.css You can alter
  -- this theme by editing @~\/.config\/taffybar\/taffybar.css@ to your liking.
  -- For an idea of the customizations you can make, see
  -- <https://live.gnome.org/GnomeArt/Tutorials/GtkThemes>.
    dyreTaffybar
  , dyreTaffybarMain
  , getTaffyFile
  , startTaffybar
  , taffybarDyreParams
  ) where

import qualified Config.Dyre as Dyre
import qualified Config.Dyre.Params as Dyre
import           Control.Monad
import qualified Data.GI.Gtk.Threading as GIThreading
import qualified Data.Text as T
import qualified GI.Gdk as Gdk
import qualified GI.Gtk as Gtk
import           Graphics.X11.Xlib.Misc
import           System.Directory
import           System.Environment.XDG.BaseDir ( getUserConfigFile )
import           System.Exit ( exitFailure )
import           System.FilePath ( (</>) )
import qualified System.IO as IO
import           System.Log.Logger
import           System.Taffybar.Context
import           System.Taffybar.Hooks

import           Paths_taffybar ( getDataDir )

-- | The parameters that are passed to Dyre when taffybar is invoked with
-- 'dyreTaffybar'.
taffybarDyreParams =
  Dyre.defaultParams
  { Dyre.projectName = "taffybar"
  , Dyre.realMain = dyreTaffybarMain
  , Dyre.showError = showError
  , Dyre.ghcOpts = ["-threaded", "-rtsopts"]
  , Dyre.rtsOptsHandling = Dyre.RTSAppend ["-I0", "-V0"]
  }

-- | Use Dyre to configure and start taffybar. This will automatically recompile
-- taffybar whenever there are changes to your taffybar.hs configuration file.
dyreTaffybar :: TaffybarConfig -> IO ()
dyreTaffybar = Dyre.wrapMain taffybarDyreParams

showError :: TaffybarConfig -> String -> TaffybarConfig
showError cfg msg = cfg { errorMsg = Just msg }

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
startCSS cssPaths = do
  -- Override the default GTK theme path settings.  This causes the
  -- bar (by design) to ignore the real GTK theme and just use the
  -- provided minimal theme to set the background and text colors.
  -- Users can override this default.
  taffybarProvider <- Gtk.cssProviderNew

  let loadIfExists filePath =
        doesFileExist filePath >>=
        flip when (Gtk.cssProviderLoadFromPath taffybarProvider (T.pack filePath))

  mapM_ loadIfExists cssPaths

  Just scr <- Gdk.screenGetDefault
  Gtk.styleContextAddProviderForScreen scr taffybarProvider 800
  return taffybarProvider

getTaffyFile :: String -> IO FilePath
getTaffyFile = getUserConfigFile "taffybar"

getDefaultCSSPaths :: IO [FilePath]
getDefaultCSSPaths = do
  defaultUserConfig <- getTaffyFile "taffybar.css"
  return [defaultUserConfig]

-- | Start taffybar with the provided 'TaffybarConfig'. Because this function
-- will not handle recompiling taffybar automatically when taffybar.hs is
-- updated, it is generally recommended that end users use 'dyreTaffybar'
-- instead. If automatic recompilation is handled by another mechanism such as
-- stack or a custom user script or not desired for some reason, it is
-- perfectly fine to use this function.
startTaffybar :: TaffybarConfig -> IO ()
startTaffybar config = do
  updateGlobalLogger "" $ removeHandler
  setTaffyLogFormatter "System.Taffybar"
  setTaffyLogFormatter "StatusNotifier"
  _ <- initThreads
  _ <- Gtk.init Nothing
  GIThreading.setCurrentThreadAsGUIThread
  defaultCSS <- getDataFile "taffybar.css"
  cssPaths <- maybe getDefaultCSSPaths (return . return) $ cssPath config
  _ <- startCSS $ defaultCSS:cssPaths
  _ <- buildContext config

  Gtk.main
  return ()
