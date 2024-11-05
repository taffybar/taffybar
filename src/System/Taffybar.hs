{-# LANGUAGE NamedFieldPuns #-}
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
import           Data.Word (Word32)
import qualified GI.Gdk as Gdk
import qualified GI.Gtk as Gtk
import           Graphics.X11.Xlib.Misc ( initThreads )
import           System.Directory
import           System.Environment.XDG.BaseDir ( getUserConfigFile )
import           System.Exit ( exitFailure )
import           System.FilePath ( (</>), normalise )
import qualified System.IO as IO
import           System.Log.Logger
import           System.Taffybar.Context
import           System.Taffybar.Hooks
import           System.Taffybar.Util ( onSigINT, maybeHandleSigHUP, rebracket )

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

-- | Locate installed vendor data file.
getDataFile :: String -> IO FilePath
getDataFile name = do
  dataDir <- getDataDir
  return (normalise (dataDir </> name))

-- | Locates full the 'FilePath' of the given Taffybar config file.
-- The [XDG Base Directory](https://specifications.freedesktop.org/basedir-spec/latest/) convention is used, meaning that config files are usually in @~\/.config\/taffybar@.
getTaffyFile :: String -> IO FilePath
getTaffyFile = getUserConfigFile "taffybar"

-- | Return CSS files which should be loaded for the given config.
getCSSPaths :: TaffybarConfig -> IO [FilePath]
getCSSPaths TaffybarConfig{cssPaths} = sequence (defaultCSS:userCSS)
  where
    -- Vendor CSS file, which is always loaded before user's CSS.
    defaultCSS = getDataFile "taffybar.css"
    -- User's configured CSS files, with XDG config file being the default.
    userCSS | null cssPaths = [getTaffyFile "taffybar.css"]
            | otherwise     = map return cssPaths

-- | Overrides the default GTK theme and settings with CSS styles from
-- the given files (if they exist).
--
-- This causes the bar (by design) to ignore the real GTK theme and
-- just use the provided minimal theme to set the background and text
-- colors.
startCSS :: [FilePath] -> IO (IO (), Gtk.CssProvider)
startCSS = startCSS' 800

-- | Installs a GTK style provider at a certain priority and loads it
-- with styles from a list of CSS files (if they exist).
--
-- This will return the 'Gtk.CssProvider' object, paired with a
-- cleanup function which can be used later to uninstall the style
-- provider.
--
-- The priority defines how the Taffybar CSS cascades with the GTK theme, etc.
-- For your information, these are the GTK defined priorities:
--  * @GTK_STYLE_PROVIDER_PRIORITY_FALLBACK@ = 1
--  * @GTK_STYLE_PROVIDER_PRIORITY_THEME@ = 100
--  * @GTK_STYLE_PROVIDER_PRIORITY_SETTINGS@ = 400
--  * @GTK_STYLE_PROVIDER_PRIORITY_APPLICATION@ = 600
--  * @GTK_STYLE_PROVIDER_PRIORITY_USER@ = 800
--
-- The file @XDG_CONFIG_HOME/gtk-3.0/gtk.css@ uses priority 800.
startCSS' :: Word32  -> [FilePath] -> IO (IO (), Gtk.CssProvider)
startCSS' prio cssFilePaths = do
  provider <- Gtk.cssProviderNew
  mapM_ (logLoadCSSFile provider) =<< filterM doesFileExist cssFilePaths
  uninstall <- install provider =<< Gdk.screenGetDefault
  pure (uninstall, provider)
  where
    logLoadCSSFile p f = logTaffy INFO ("Loading stylesheet " ++ f) >> loadCSSFile p f
    loadCSSFile p = Gtk.cssProviderLoadFromPath p . T.pack
    install provider (Just scr) = do
      Gtk.styleContextAddProviderForScreen scr provider prio
      pure (Gtk.styleContextRemoveProviderForScreen scr provider)
    install _ Nothing = pure (pure ())

-- | Uses 'startCSS' in a 'bracket' block to ensure that the CSS
-- provider is removed when Taffybar finishes.
--
-- If Taffybar is running as a daemon, then this also installs a
-- handler on @SIGHUP@ which triggers reloading of the CSS files.
withCSSReloadable :: [FilePath] -> IO () -> IO ()
withCSSReloadable css action = rebracket (startCSS css) $ \reload -> do
  void reload
  let notice = logTaffy NOTICE "Received SIGHUP reloading CSS..."
  maybeHandleSigHUP (notice <* reload) action

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

  cssPathsToLoad <- getCSSPaths config
  context <- buildContext config

  withCSSReloadable cssPathsToLoad $ Gtk.main
    `finally` logTaffy DEBUG "Finished main loop"
    `onSigINT` do
      logTaffy INFO "Interrupted"
      exitTaffybar context

  logTaffy DEBUG "Exited normally"

logTaffy :: Priority -> String -> IO ()
logTaffy = logM "System.Taffybar"
