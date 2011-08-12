-- | This is a system status bar meant for use with window manager
-- like XMonad.  It is similar to xmobar, but with more visual flare
-- and a different widget set.  Contributed widgets are more than
-- welcome.  The bar is drawn using gtk and cairo.  It is actually the
-- simplest possible thing that could plausibly work: you give
-- Taffybar a list of GTK widgets and it will render them in a
-- horizontal bar for you (taking care of ugly details like reserving
-- strut space so that window managers don't put windows over it).
--
-- This is the real main module.  The default bar should be
-- customized to taste in the config file
-- (~/.config/taffybar/taffybar.hs).  Typically, this means adding
-- widgets to the default config.  A default configuration file is
-- included in the distribution, but the essentials are covered here.
--
-- The config file is just a Haskell source file that is compiled at
-- startup (if it has changed) to produce a custom executable with the
-- desired set of widgets.  You will want to import this module along
-- with the modules of any widgets you want to add to the bar.  Note,
-- you can define any widgets that you want in your config file or
-- other libraries.  Taffybar only cares that you give it some GTK
-- widgets to display.
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
-- >       log = xmonadLogNew
-- >       tray = systrayNew
-- >       cpu = pollingGraphNew cpuCfg 0.5 cpuCallback
-- >   defaultTaffybar defaultTaffybarConfig { startWidgets = [ log ]
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
--
-- The XMonadLog widget differs from its counterpart in xmobar: it
-- listens for updates over DBus instead of reading from stdin.  This
-- makes it easy to restart Taffybar independently of XMonad.  XMonad
-- does not come with a DBus logger, so here is an example of how to
-- make it work.  Note: this requires the dbus-core (>0.9) package,
-- which is installed as a dependency of Taffybar.
--
-- > import XMonad.Hooks.DynamicLog
-- > import DBus.Client.Simple
-- > import System.Taffybar.XMonadLog ( dbusLog )
-- >
-- > main = do
-- >   client <- connectSession
-- >   let pp = defaultPP
-- >   xmonad defaultConfig { logHook = dbusLog client pp }
--
-- The complexity is handled in the System.Tafftbar.XMonadLog module.
module System.Taffybar (
  TaffybarConfig(..),
  defaultTaffybar,
  defaultTaffybarConfig
  ) where

import qualified Config.Dyre as Dyre

import Graphics.UI.Gtk
import Text.Printf

import System.Taffybar.StrutProperties

data TaffybarConfig =
  TaffybarConfig { screenNumber :: Int -- ^ The screen number to run the bar on (default is almost always fine)
                 , monitorNumber :: Int -- ^ The xinerama/xrandr monitor number to put the bar on (default: 0)
                 , barHeight :: Int -- ^ Number of pixels to reserve for the bar (default: 25 pixels)
                 , errorMsg :: Maybe String -- ^ Used by the application
                 , startWidgets :: [IO Widget] -- ^ Widgets that are packed in order at the left end of the bar
                 , endWidgets :: [IO Widget] -- ^ Widgets that are packed from right-to-left in the bar
                 }

-- | The default configuration gives an empty bar 25 pixels high on monitor 0.
defaultTaffybarConfig :: TaffybarConfig
defaultTaffybarConfig =
  TaffybarConfig { screenNumber = 0
                 , monitorNumber = 0
                 , barHeight = 25
                 , errorMsg = Nothing
                 , startWidgets = []
                 , endWidgets = []
                 }

showError :: TaffybarConfig -> String -> TaffybarConfig
showError cfg msg = cfg { errorMsg = Just msg }

-- | The default parameters need to tell GHC to compile using
-- -threaded so that the GTK event loops doesn't block all of the
-- widgets
defaultParams :: Dyre.Params TaffybarConfig
defaultParams = Dyre.defaultParams { Dyre.projectName = "taffybar"
                                   , Dyre.realMain = realMain
                                   , Dyre.showError = showError
                                   , Dyre.ghcOpts = ["-threaded"]
                                   }

-- | The entry point of the application.  Feed it a custom config.
defaultTaffybar :: TaffybarConfig -> IO ()
defaultTaffybar = Dyre.wrapMain defaultParams

realMain :: TaffybarConfig -> IO ()
realMain cfg = do
  case errorMsg cfg of
    Nothing -> taffybarMain cfg
    Just err -> error ("Error: " ++ err)

taffybarMain :: TaffybarConfig -> IO ()
taffybarMain cfg = do
  _ <- initGUI
  Just disp <- displayGetDefault
  nscreens <- displayGetNScreens disp
  screen <- case screenNumber cfg < nscreens of
    False -> error $ printf "Screen %d is not available in the default display" (screenNumber cfg)
    True -> displayGetScreen disp (screenNumber cfg)
  nmonitors <- screenGetNMonitors screen
  monitorSize <- case monitorNumber cfg < nmonitors of
    False -> error $ printf "Monitor %d is not available in the selected screen" (monitorNumber cfg)
    True -> screenGetMonitorGeometry screen (monitorNumber cfg)

  window <- windowNew
  let Rectangle x y w _ = monitorSize
  windowSetTypeHint window WindowTypeHintDock
  windowSetScreen window screen
  windowSetDefaultSize window w (barHeight cfg)
  windowMove window x y
  widgetModifyBg window StateNormal (Color 0 0 0)
  _ <- onRealize window $ setStrutProperties window (0, 0, barHeight cfg, 0,
                             0, 0,
                             0, 0,
                             x, x + w - 10,
                             0, 0)
  box <- hBoxNew False 10
  containerAdd window box

  mapM_ (\io -> do
            wid <- io
            widgetSetSizeRequest wid (-1) (barHeight cfg)
            boxPackStart box wid PackNatural 0) (startWidgets cfg)
  mapM_ (\io -> do
            wid <- io
            widgetSetSizeRequest wid (-1) (barHeight cfg)
            boxPackEnd box wid PackNatural 0) (endWidgets cfg)
  widgetShow window
  widgetShow box
  mainGUI
  return ()