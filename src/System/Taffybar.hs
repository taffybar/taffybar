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
  -- | This is a system status bar meant for use with window manager
  -- like XMonad.  It is similar to xmobar, but with more visual flare
  -- and a different widget set.  Contributed widgets are more than
  -- welcome.  The bar is drawn using gtk and cairo.  It is actually
  -- the simplest possible thing that could plausibly work: you give
  -- Taffybar a list of GTK widgets and it will render them in a
  -- horizontal bar for you (taking care of ugly details like
  -- reserving strut space so that window managers don't put windows
  -- over it).
  --
  -- This is the real main module.  The default bar should be
  -- customized to taste in the config file
  -- (~/.config/taffybar/taffybar.hs).  Typically, this means adding
  -- widgets to the default config.  A default configuration file is
  -- included in the distribution, but the essentials are covered
  -- here.

  -- * Config File
  --
  -- | The config file is just a Haskell source file that is compiled
  -- at startup (if it has changed) to produce a custom executable
  -- with the desired set of widgets.  You will want to import this
  -- module along with the modules of any widgets you want to add to
  -- the bar.  Note, you can define any widgets that you want in your
  -- config file or other libraries.  Taffybar only cares that you
  -- give it some GTK widgets to display.
  --
  -- Below is a fairly typical example:
  --
  -- > import System.Taffybar
  -- > import System.Taffybar.Systray
  -- > import System.Taffybar.XMonadLog
  -- > import System.Taffybar.SimpleClock
  -- > import System.Taffybar.Widgets.PollingGraph
  -- > import System.Information.CPU
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
  -- >       tray = systrayNew
  -- >       cpu = pollingGraphNew cpuCfg 0.5 cpuCallback
  -- >   defaultTaffybar defaultTaffybarConfig { startWidgets = [ ]
  -- >                                         , endWidgets = [ tray, clock, cpu ]
  -- >                                         }
  --
  -- This configuration creates a bar with four widgets.  On the left is
  -- the XMonad log.  The rightmost widget is the system tray, with a
  -- clock and then a CPU graph.  The clock is formatted using standard
  -- strftime-style format strings (see the clock module).  Note that
  -- the clock is colored using Pango markup (again, see the clock
  -- module).
  --
  -- The CPU widget plots two graphs on the same widget: total CPU use
  -- in green and then system CPU use in a kind of semi-transparent
  -- purple on top of the green.
  --
  -- It is important to note that the widget lists are *not* [Widget].
  -- They are actually [IO Widget] since the bar needs to construct them
  -- after performing some GTK initialization.

  -- * XMonad Integration (via DBus)
  --
  -- | The XMonadLog widget differs from its counterpart in xmobar: it
  -- listens for updates over DBus instead of reading from stdin.
  -- This makes it easy to restart Taffybar independently of XMonad.
  -- XMonad does not come with a DBus logger, so here is an example of
  -- how to make it work.  Note: this requires the dbus-core (>0.9)
  -- package, which is installed as a dependency of Taffybar.
  --
  -- > import XMonad.Hooks.DynamicLog
  -- > import XMonad.Hooks.ManageDocks
  -- > import DBus.Client
  -- > import System.Taffybar.XMonadLog ( dbusLog )
  -- >
  -- > main = do
  -- >   client <- connectSession
  -- >   let pp = defaultPP
  -- >   xmonad $ docks defaultConfig { logHook = dbusLog client pp }
  --
  -- The complexity is handled in the System.Taffybar.XMonadLog module. Note
  -- that the docks wrapper from ManageDocks is required to have XMonad put
  -- taffybar in the strut space that it reserves. If you have problems with
  -- taffybar appearing almost fullscreen, check to see if you are using this
  -- wrapper. Note that the manageDocks hook that previous used to be sufficient
  -- for this is no longer so (see
  -- https://github.com/travitch/taffybar/issues/185).

  -- ** A note about DBus:
  -- |
  -- * If you start xmonad using a graphical login manager like gdm or
  --   kdm, DBus should be started automatically for you.
  --
  -- * If you start xmonad with a different graphical login manager that
  --   does not start DBus for you automatically, put the line @eval
  --   \`dbus-launch --auto-syntax\`@ into your ~\/.xsession *before*
  --   xmonad and taffybar are started.  This command sets some
  --   environment variables that the two must agree on.
  --
  -- * If you start xmonad via @startx@ or a similar command, add the
  --   above command to ~\/.xinitrc

  -- * Colors
  --
  -- | While taffybar is based on GTK+, it ignores your GTK+ theme.
  -- The default theme that it uses is in
  -- @~\/.cabal\/share\/taffybar-\<version\>\/taffybar.rc@.  You can
  -- customize this theme by copying it to
  -- @~\/.config\/taffybar\/taffybar.rc@.  For an idea of the customizations you can make,
  -- see <https://live.gnome.org/GnomeArt/Tutorials/GtkThemes>.

  -- * Advanced Widget Example
  --
  -- | The following is an example leveraging GTK+ features that are not exposed
  -- by the normal Taffybar widget hooks.
  --
  -- > import qualified Graphics.UI.Gtk as Gtk
  -- > import System.Taffybar.Widgets.PollingGraph
  -- > import System.Information.CPU
  -- > import XMonad.Util.Run
  -- >
  -- > main = do
  -- >   let
  -- >     cpuReader widget = do
  -- >       (userLoad, systemLoad, totalLoad) <- cpuLoad
  -- >       Gtk.postGUIAsync $ do
  -- >         let
  -- >           user    = round $ 100 * userLoad   :: Int
  -- >           system  = round $ 100 * systemLoad :: Int
  -- >           tooltip = printf "%02i%% User\n%02i%% System" user system :: String
  -- >         _ <- Gtk.widgetSetTooltipText widget $ Just tooltip
  -- >         return ()
  -- >       return [totalLoad, systemLoad]
  -- >
  -- >     cpuButtons = do
  -- >       e <- Gtk.eventButton
  -- >       case e of
  -- >         Gtk.LeftButton   -> unsafeSpawn "terminator -e glances"
  -- >         Gtk.RightButton  -> unsafeSpawn "terminator -e top"
  -- >         Gtk.MiddleButton -> unsafeSpawn "gnome-system-monitor"
  -- >         _ -> return ()
  -- >       return True
  -- >
  -- >     cpuCfg = defaultGraphConfig { graphDataColors = [ (0, 1, 0, 1)
  -- >                                                     , (1, 0, 1, 0.5)
  -- >                                                     ]
  -- >                                 }
  -- >
  -- >
  -- >     cpu = do
  -- >       ebox <- Gtk.eventBoxNew
  -- >       btn <- pollingGraphNew cpuCfg 0.5 $ cpuReader $ Gtk.toWidget ebox
  -- >       Gtk.containerAdd ebox btn
  -- >       _ <- Gtk.on ebox Gtk.buttonPressEvent systemEvents
  -- >       Gtk.widgetShowAll ebox
  -- >       return $ Gtk.toWidget ebox
  --
  -- The resulting widget can be used like normal widgets, but you can use
  -- different mouse buttons to run various programs and it has a useful tooltip
  -- which shows the concrete numbers, which may not be clear in the graph
  -- itself.
    taffybarDyreParams
  , dyreTaffybar
  , startTaffybar
  , dyreTaffybarMain
  ) where

import qualified Config.Dyre as Dyre
import qualified Config.Dyre.Params as Dyre
import Control.Monad
import Graphics.UI.Gtk as Gtk
import Graphics.UI.Gtk.General.CssProvider
import qualified Graphics.UI.Gtk.General.StyleContext as Gtk
import Graphics.X11.Xlib.Misc
import System.Directory
import System.Environment.XDG.BaseDir ( getUserConfigFile )
import System.Exit ( exitFailure )
import System.FilePath ( (</>) )
import qualified System.IO as IO
import System.Taffybar.Context

import Paths_taffybar ( getDataDir )

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

startCSS :: IO CssProvider
startCSS = do
  -- Override the default GTK theme path settings.  This causes the
  -- bar (by design) to ignore the real GTK theme and just use the
  -- provided minimal theme to set the background and text colors.
  -- Users can override this default.
  taffybarProvider <- cssProviderNew
  let loadIfExists filePath =
        doesFileExist filePath >>=
        flip when (cssProviderLoadFromPath taffybarProvider filePath)
  loadIfExists =<< getDefaultConfigFile "taffybar.css"
  loadIfExists =<< getUserConfigFile "taffybar" "taffybar.css"
  Just scr <- screenGetDefault
  Gtk.styleContextAddProviderForScreen scr taffybarProvider 800
  return taffybarProvider

-- | Start taffybar with the provided 'TaffybarConfig'. Because this function
-- will not handle recompiling taffybar automatically when taffybar.hs is
-- updated, it is generally recommended that end users use 'dyreTaffybar'
-- instead. If automatic recompilation is handled by another mechanism, or not
-- desired for some reason, it is perfectly fine to use this function.
startTaffybar :: TaffybarConfig -> IO ()
startTaffybar config = do
  _ <- initThreads
  _ <- initGUI
  _ <- startCSS
  _ <- buildContext config

  mainGUI
  return ()
