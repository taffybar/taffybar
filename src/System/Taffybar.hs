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
  -- * Detail
  --
  -- | This is a system status bar meant for use with window managers like
  -- XMonad. It is similar to xmobar, but with more visual flare and a different
  -- widget set. Contributed widgets are more than welcome. The bar is drawn
  -- using gtk and cairo. It is actually the simplest possible thing that could
  -- plausibly work: you give Taffybar a list of GTK widgets and it will render
  -- them in a horizontal bar for you (taking care of ugly details like
  -- reserving strut space so that window managers don't put windows over it).
  --
  -- This is the real main module. The default bar should be customized to taste
  -- in the config file (~/.config/taffybar/taffybar.hs). Typically, this means
  -- adding widgets to the default config. A default configuration file is
  -- included in the distribution, but the essentials are covered here.

  -- * Config File
  --
  -- | The config file is just a Haskell source file that is compiled at startup
  -- (if it has changed) to produce a custom executable with the desired set of
  -- widgets. You will want to import this module along with the modules of any
  -- widgets you want to add to the bar. Note, you can define any widgets that
  -- you want in your config file or other libraries. Taffybar only cares that
  -- you give it some GTK widgets to display.
  --
  -- Below is a fairly typical example:
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
  -- >       clock = textClockNew Nothing "<span fgcolor='orange'>%a %b %_d %H:%M</span>" 1
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
  -- widget is the system tray, with a clock and then a CPU graph. The clock is
  -- formatted using standard strftime-style format strings (see the clock
  -- module). Note that the clock is colored using Pango markup (again, see the
  -- clock module).
  --
  -- The CPU widget plots two graphs on the same widget: total CPU use in green
  -- and then system CPU use in a kind of semi-transparent purple on top of the
  -- green.
  --
  -- It is important to note that the widget lists are *not* [Widget]. They are
  -- actually [TaffyIO Widget] since the bar needs to construct them after performing
  -- some GTK initialization.
  --
  -- ** A note about taffybar's dependency on DBus:
  -- |
  -- * If you start your window manager using a graphical login manager like gdm
  --   or kdm, DBus should be started automatically for you.
  --
  -- * If you start xmonad with a different graphical login manager that does
  --   not start DBus for you automatically, put the line @eval \`dbus-launch
  --   --auto-syntax\`@ into your ~\/.xsession *before* xmonad and taffybar are
  --   started. This command sets some environment variables that the two must
  --   agree on.
  --
  -- * If you start xmonad via @startx@ or a similar command, add the
  --   above command to ~\/.xinitrc

  -- * Colors
  --
  -- | While taffybar is based on GTK+, it ignores your GTK+ theme. The default
  -- theme that it uses lives at
  -- https://github.com/taffybar/taffybar/blob/master/taffybar.css You can alter
  -- this theme by editing @~\/.config\/taffybar\/taffybar.css@ to your liking.
  -- For an idea of the customizations you can make, see
  -- <https://live.gnome.org/GnomeArt/Tutorials/GtkThemes>.
    taffybarDyreParams
  , dyreTaffybar
  , startTaffybar
  , dyreTaffybarMain
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
import           System.Taffybar.Context

import           Paths_taffybar ( getDataDir )

-- | The parameters that are passed to Dyre when taffybar is invoked with
-- 'dyreTaffybar'.
taffybarDyreParams :: Dyre.Params TaffybarConfig
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

getDefaultConfigFile :: String -> IO FilePath
getDefaultConfigFile name = do
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

getDefaultCSSPaths :: IO [FilePath]
getDefaultCSSPaths = do
  defaultUserConfig <- getUserConfigFile "taffybar" "taffybar.css"
  return [defaultUserConfig]

-- | Start taffybar with the provided 'TaffybarConfig'. Because this function
-- will not handle recompiling taffybar automatically when taffybar.hs is
-- updated, it is generally recommended that end users use 'dyreTaffybar'
-- instead. If automatic recompilation is handled by another mechanism such as
-- stack or a custom user script or not desired for some reason, it is
-- perfectly fine to use this function.
startTaffybar :: TaffybarConfig -> IO ()
startTaffybar config = do
  _ <- initThreads
  _ <- Gtk.init Nothing
  GIThreading.setCurrentThreadAsGUIThread
  defaultConfig <- getDefaultConfigFile "taffybar.css"
  cssPaths <- maybe getDefaultCSSPaths (return . return) $ cssPath config
  _ <- startCSS $ defaultConfig:cssPaths
  _ <- buildContext config

  Gtk.main
  return ()
