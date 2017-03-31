{-# LANGUAGE TupleSections #-}
-- | The main module of Taffybar
module System.Taffybar (
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
  -- >   xmonad defaultConfig { logHook = dbusLog client pp
  -- >                        , manageHook = manageDocks
  -- >                        }
  --
  -- The complexity is handled in the System.Taffybar.XMonadLog
  -- module.  Note that manageDocks is required to have XMonad put
  -- taffybar in the strut space that it reserves.  If you have
  -- problems with taffybar appearing almost fullscreen, check to
  -- see if you have manageDocks in your manageHook.

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
  TaffybarConfig(..),
  defaultTaffybar,
  defaultTaffybarConfig,
  Position(..),
  taffybarMain,
  allMonitors,
  ) where

import qualified Config.Dyre as Dyre
import qualified Config.Dyre.Params as Dyre
import Control.Monad ( when, filterM )
import Data.Maybe ( fromMaybe )
import System.Environment.XDG.BaseDir ( getUserConfigFile )
import System.FilePath ( (</>) )
import Graphics.UI.Gtk
import Safe ( atMay )
import System.Exit ( exitFailure )
import qualified System.IO as IO
import Text.Printf ( printf )

import Paths_taffybar ( getDataDir )
import System.Taffybar.StrutProperties

data Position = Top | Bottom
  deriving (Show, Eq)


strutProperties :: Position  -- ^ Bar position
                -> Int       -- ^ Bar height
                -> Rectangle -- ^ Current monitor rectangle
                -> [Rectangle] -- ^ All monitors
                -> StrutProperties
strutProperties pos bh (Rectangle mX mY mW mH) monitors =
    propertize pos sX sW sH
    where sX = mX
          sW = mW - 1
          sH = case pos of Top    -> bh + mY
                           Bottom -> bh + totalH - mY - mH
          totalH = maximum $ map bottomY monitors
          bottomY (Rectangle _ y _ h) = y + h
          propertize p x w h = case p of
              Top    -> (0, 0, h, 0, 0, 0, 0, 0, x, x+w, 0,   0)
              Bottom -> (0, 0, 0, h, 0, 0, 0, 0, 0,   0, x, x+w)

data TaffybarConfig =
  TaffybarConfig { screenNumber :: Int -- ^ The screen number to run the bar on (default is almost always fine)
                 , monitorNumber :: Int -- ^ The xinerama/xrandr monitor number to put the bar on (default: 0)
                 , monitorFilter :: Maybe (Int -> IO Bool)
                 , barHeight :: Int -- ^ Number of pixels to reserve for the bar (default: 25 pixels)
                 , barPosition :: Position -- ^ The position of the bar on the screen (default: Top)
                 , widgetSpacing :: Int -- ^ The number of pixels between widgets
                 , errorMsg :: Maybe String -- ^ Used by the application
                 , startWidgets :: [IO Widget] -- ^ Widgets that are packed in order at the left end of the bar
                 , endWidgets :: [IO Widget] -- ^ Widgets that are packed from right-to-left in the bar
                 }

-- | The default configuration gives an empty bar 25 pixels high on monitor 0.
defaultTaffybarConfig :: TaffybarConfig
defaultTaffybarConfig =
  TaffybarConfig { screenNumber = 0
                 , monitorNumber = 0
                 , monitorFilter = Nothing
                 , barHeight = 25
                 , barPosition = Top
                 , widgetSpacing = 10
                 , errorMsg = Nothing
                 , startWidgets = []
                 , endWidgets = []
                 }

allMonitors :: Maybe (Int -> IO Bool)
allMonitors = Just $ const $ return True

showError :: TaffybarConfig -> String -> TaffybarConfig
showError cfg msg = cfg { errorMsg = Just msg }

-- | The default parameters need to tell GHC to compile using
-- -threaded so that the GTK event loops doesn't block all of the
-- widgets
defaultParams :: Dyre.Params TaffybarConfig
defaultParams = Dyre.defaultParams { Dyre.projectName = "taffybar"
                                   , Dyre.realMain = realMain
                                   , Dyre.showError = showError
                                   , Dyre.ghcOpts = ["-threaded", "-rtsopts"]
                                   , Dyre.rtsOptsHandling = Dyre.RTSAppend ["-I0", "-V0"]
                                   }

-- | The entry point of the application.  Feed it a custom config.
defaultTaffybar :: TaffybarConfig -> IO ()
defaultTaffybar = Dyre.wrapMain defaultParams

realMain :: TaffybarConfig -> IO ()
realMain cfg = do
  case errorMsg cfg of
    Nothing -> taffybarMain cfg
    Just err -> do
      IO.hPutStrLn IO.stderr ("Error: " ++ err)
      exitFailure

getDefaultConfigFile :: String -> IO FilePath
getDefaultConfigFile name = do
  dataDir <- getDataDir
  return (dataDir </> name)

-- | Given a Taffybar configuration and the Taffybar window, this
-- action sets up the window size and strut properties. May be called
-- multiple times, e.g., when the monitor resolution changes.
setTaffybarSize :: TaffybarConfig -> Window -> Int -> IO ()
setTaffybarSize cfg window monNumber = do
  screen <- windowGetScreen window
  nmonitors <- screenGetNMonitors screen
  allMonitorSizes <-
    mapM (screenGetMonitorGeometry screen) [0 .. (nmonitors - 1)]
  when (monNumber >= nmonitors) $ do
    IO.hPutStrLn IO.stderr $
      printf
        "Monitor %d is not available in the selected screen"
        (monNumber)
  let monitorSize =
        fromMaybe (allMonitorSizes !! 0) $ do
          allMonitorSizes `atMay` monNumber
  let Rectangle x y w h = monitorSize
      yoff =
        case barPosition cfg of
          Top -> 0
          Bottom -> h - barHeight cfg
  windowMove window x (y + yoff)
  -- Set up the window size using fixed min and max sizes. This
  -- prevents the contained horizontal box from affecting the window
  -- size.
  windowSetGeometryHints
    window
    (Nothing :: Maybe Widget)
    (Just (w, barHeight cfg)) -- Min size.
    (Just (w, barHeight cfg)) -- Max size.
    Nothing
    Nothing
    Nothing
  let setStrutProps =
        setStrutProperties window $
        strutProperties
          (barPosition cfg)
          (barHeight cfg)
          monitorSize
          allMonitorSizes
  winRealized <- widgetGetRealized window
  if winRealized
    then setStrutProps
    else onRealize window setStrutProps >> return ()

taffybarMain :: TaffybarConfig -> IO ()
taffybarMain cfg = do

  _ <- initGUI

  -- Load default and user gtk resources
  defaultGtkConfig <- getDefaultConfigFile "taffybar.rc"
  userGtkConfig <- getUserConfigFile "taffybar" "taffybar.rc"
  rcParse defaultGtkConfig
  rcParse userGtkConfig

  Just disp <- displayGetDefault
  nscreens <- displayGetNScreens disp
  screen <- case screenNumber cfg < nscreens of
    False -> error $ printf "Screen %d is not available in the default display" (screenNumber cfg)
    True -> displayGetScreen disp (screenNumber cfg)

  nmonitors <- screenGetNMonitors screen
  let monFilter = fromMaybe (return . ((monitorNumber cfg) ==)) (monitorFilter cfg)
  activeMonitors <- filterM monFilter [0 .. (nmonitors -1)]

  let makeTaffyWindow monNumber = do
        window <- windowNew
        let windowName = printf "Taffybar-%s" $ show monNumber :: String

        widgetSetName window windowName
        windowSetTypeHint window WindowTypeHintDock
        windowSetScreen window screen
        setTaffybarSize cfg window monNumber

        box <- hBoxNew False $ widgetSpacing cfg
        containerAdd window box

        mapM_
          (\io -> do
             wid <- io
             widgetSetSizeRequest wid (-1) (barHeight cfg)
             boxPackStart box wid PackNatural 0)
          (startWidgets cfg)

        mapM_
          (\io -> do
             wid <- io
             widgetSetSizeRequest wid (-1) (barHeight cfg)
             boxPackEnd box wid PackNatural 0)
          (endWidgets cfg)

        _ <- on screen screenMonitorsChanged (setTaffybarSize cfg window monNumber)

        widgetShow window
        widgetShow box

  mapM_ makeTaffyWindow activeMonitors
  -- Reset the size of the Taffybar window if the monitor setup has
  -- changed, e.g., after a laptop user has attached an external
  -- monitor.
  mainGUI
  return ()
