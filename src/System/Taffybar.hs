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

  -- * XMonad Integration (via EWMH hints and X11 events)
  --
  -- | The TaffyPager widget differs from its xounterpart in xmobar: it listens
  -- for updates using EWMH hints and X11 events instead of reading from stdin.
  -- See [System.Taffybar.TaffyPager] for more details.  First, you'll need to
  -- configure XMonad to advetize the EWMH hints and X11 events:
  --
  -- > import XMonad.Hooks.EwmhDesktops (ewmh)
  -- > import XMonad.Hooks.ManageDocks
  -- > import System.Taffybar.Hooks.PagerHints (pagerHints)
  -- >
  -- > main = do
  -- >   xmonad $ ewmh $ pagerHints $ defaultConfig { manageHook = manageDocks
  -- >                                              }
  --
  -- And then your taffybar configuration can simply be:
  --
  -- > import System.Taffybar
  -- > import System.Taffybar.Systray
  -- > import System.Taffybar.TaffyPager
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
  -- >       pager = taffyPagerNew defaultPagerConfig
  -- >       tray = systrayNew
  -- >       cpu = pollingGraphNew cpuCfg 0.5 cpuCallback
  -- >   defaultTaffybar defaultTaffybarConfig { startWidgets = [ pager ]
  -- >                                         , endWidgets = [ tray, clock, cpu ]
  -- >                                         }
  --

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

  TaffybarConfig(..),
  TaffybarConfigEQ,
  defaultTaffybar,
  defaultTaffybarConfig,
  Position(..),
  taffybarMain,
  allMonitors,
  useMonitorNumber,
  realMain,
  usePrimaryMonitor,
  ) where

import qualified Config.Dyre as Dyre
import qualified Config.Dyre.Params as Dyre
import qualified Control.Concurrent.MVar as MV
import Control.Monad ( when, foldM, void )
import Data.List
import qualified Data.Map as M
import Data.Maybe ( fromMaybe )
import Graphics.UI.Gtk as Gtk
import Graphics.UI.Gtk.General.StyleContext
import Graphics.X11.Xlib.Misc
import Safe ( atMay )
import System.Directory
import System.Environment.XDG.BaseDir ( getUserConfigFile )
import System.Exit ( exitFailure )
import System.FilePath ( (</>) )
import System.Information.X11DesktopInfo
import qualified System.IO as IO
import System.Mem.StableName
import Text.Printf ( printf )

import Graphics.UI.Gtk.General.CssProvider
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
  TaffybarConfig { -- | The screen number to run the bar on (default is almost always fine)
                   screenNumber :: Int
                 -- | The xinerama/xrandr monitor number to put the bar on (default: 0)
                 , monitorNumber :: Int
                 -- | Provides a way to specify which screens taffybar should appear on.
                 , getMonitorConfig :: TaffybarConfigEQ -> IO (Int -> Maybe TaffybarConfigEQ)
                 -- | A function providing a way to call back in to taffybar to
                 -- refresh its configs/open closed state on each monitor.
                 , startRefresher :: IO () -> IO ()
                 -- | Number of pixels to reserve for the bar (default: 25 pixels)
                 , barHeight :: Int
                 -- | Number of additional pixels to reserve for the bar strut (default: 0)
                 , barPadding :: Int
                 -- | The position of the bar on the screen (default: Top)
                 , barPosition :: Position
                 -- | The number of pixels between widgets
                 , widgetSpacing :: Int
                 -- | Used by the application
                 , errorMsg :: Maybe String
                 -- | Widgets that are packed in order at the left end of the bar
                 , startWidgets :: [IO Widget]
                 -- | Widgets that are packed from right-to-left in the bar
                 , endWidgets :: [IO Widget]
                 }

type TaffybarConfigEQ = (TaffybarConfig, StableName TaffybarConfig)

-- | The default configuration gives an empty bar 25 pixels high on monitor 0.
defaultTaffybarConfig :: TaffybarConfig
defaultTaffybarConfig =
  TaffybarConfig { screenNumber = 0
                 , monitorNumber = 0
                 , getMonitorConfig = useMonitorNumber
                 , startRefresher = const $ return ()
                 , barHeight = 25
                 , barPadding = 0
                 , barPosition = Top
                 , widgetSpacing = 10
                 , errorMsg = Nothing
                 , startWidgets = []
                 , endWidgets = []
                 }

useMonitorNumber :: TaffybarConfigEQ -> IO (Int -> Maybe TaffybarConfigEQ)
useMonitorNumber c@(cfg, _) = return umn
  where umn mnumber
            | mnumber == monitorNumber cfg = Just c
            | otherwise = Nothing

-- | Use the primary monitor as set by Xrandr.
usePrimaryMonitor :: TaffybarConfigEQ -> IO (Int -> Maybe TaffybarConfigEQ)
usePrimaryMonitor c@(cfg, _) = do
  maybePrimary <- withDefaultCtx getPrimaryOutputNumber
  let primary = maybe (monitorNumber cfg) id maybePrimary
  return $ \mnumber -> if mnumber == primary then Just c else Nothing

allMonitors :: TaffybarConfigEQ -> IO (Int -> Maybe TaffybarConfigEQ)
allMonitors cfg = return $ const $ Just cfg

showError :: TaffybarConfig -> String -> TaffybarConfig
showError cfg msg = cfg { errorMsg = Just msg }

-- | The default parameters need to tell GHC to compile using
-- -threaded so that the GTK event loops doesn't block all of the
-- widgets
defaultParams :: Dyre.Params TaffybarConfig
defaultParams =
  Dyre.defaultParams
  { Dyre.projectName = "taffybar"
  , Dyre.realMain = realMain
  , Dyre.showError = showError
  , Dyre.ghcOpts = ["-threaded", "-rtsopts"]
  , Dyre.rtsOptsHandling = Dyre.RTSAppend ["-I0", "-V0"]
  }

-- | The entry point of the application.  Feed it a custom config.
defaultTaffybar :: TaffybarConfig -> IO ()
defaultTaffybar = Dyre.wrapMain defaultParams

realMain :: TaffybarConfig -> IO ()
realMain cfg =
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
  when (monNumber >= nmonitors) $
    IO.hPutStrLn IO.stderr $
      printf
        "Monitor %d is not available in the selected screen"
        monNumber
  let monitorSize =
        fromMaybe (head allMonitorSizes) $
          allMonitorSizes `atMay` monNumber
  let Rectangle x y w h = monitorSize
      strutHeight = barHeight cfg + (2 * barPadding cfg)
      yoff =
        case barPosition cfg of
          Top -> barPadding cfg
          Bottom -> h - strutHeight
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
          strutHeight
          monitorSize
          allMonitorSizes

  winRealized <- widgetGetRealized window
  if winRealized
    then setStrutProps
  else void $ on window realize setStrutProps

startCSS :: IO CssProvider
startCSS = do
  -- Override the default GTK theme path settings.  This causes the
  -- bar (by design) to ignore the real GTK theme and just use the
  -- provided minimal theme to set the background and text colors.
  -- Users can override this default.
  taffybarProvider <- cssProviderNew
  let loadIfExists filePath =
        doesFileExist filePath >>= flip when (cssProviderLoadFromPath taffybarProvider filePath)
  loadIfExists =<< getDefaultConfigFile "taffybar.css"
  loadIfExists =<< getUserConfigFile "taffybar" "taffybar.css"
  Just scr <- screenGetDefault
  styleContextAddProviderForScreen scr taffybarProvider 800
  return taffybarProvider

taffybarMain :: TaffybarConfig -> IO ()
taffybarMain cfg = do

  _ <- initThreads
  _ <- initGUI
  _ <- startCSS

  Just disp <- displayGetDefault
  nscreens <- displayGetNScreens disp
  screen <- if screenNumber cfg < nscreens
            then displayGetScreen disp (screenNumber cfg)
            else error $ printf "Screen %d is not available in the default display"
           (screenNumber cfg)

  cfgEq <- makeStableName cfg
  taffyWindowsVar <- MV.newMVar M.empty

  let refreshTaffyWindows = do
        nmonitors <- screenGetNMonitors screen
        getConfig <- getMonitorConfig cfg (cfg, cfgEq)
        MV.modifyMVar_ taffyWindowsVar $ \monitorToWindow ->
          do
            let monitors = union [0 .. (nmonitors - 1)] $ M.keys monitorToWindow
                updateBarOnWindow mapToUpdate monNum
                  | monNum >= nmonitors = maybeDeleteWindow
                  | otherwise = case M.lookup monNum monitorToWindow of
                                  Just (currentConfig, window) ->
                                    case getConfig monNum of
                                      Just configEq@(_, newConfigEq) ->
                                        if currentConfig == newConfigEq
                                        then
                                          return mapToUpdate
                                        else
                                          widgetDestroy window >>
                                          makeAndAddWindow configEq
                                      Nothing -> maybeDeleteWindow
                                  Nothing ->
                                    case getConfig monNum of
                                      Just configEq -> makeAndAddWindow configEq
                                      Nothing -> return mapToUpdate
                  where makeAndAddWindow (newConfig, eqcfg) =
                          do
                            window <- makeTaffyWindow newConfig monNum
                            return $ M.insert monNum (eqcfg, window) mapToUpdate
                        deleteWindow (_, window) =
                          widgetDestroy window >> return (M.delete monNum mapToUpdate)
                        maybeDeleteWindow = maybe (return mapToUpdate) deleteWindow $
                                            M.lookup monNum mapToUpdate
            foldM updateBarOnWindow monitorToWindow monitors

      makeTaffyWindow wcfg monNumber = do
        window <- windowNew
        let windowName = printf "Taffybar-%s" $ show monNumber :: String

        styleContext <- Gtk.widgetGetStyleContext window
        styleContextAddClass styleContext "Taffybar"
        widgetSetName window windowName

        windowSetTypeHint window WindowTypeHintDock
        windowSetScreen window screen
        setTaffybarSize wcfg window monNumber

        box <- hBoxNew False $ widgetSpacing wcfg
        containerAdd window box

        mapM_
          (\io -> do
             wid <- io
             widgetSetSizeRequest wid (-1) (barHeight wcfg)
             boxPackStart box wid PackNatural 0)
          (startWidgets wcfg)

        mapM_
          (\io -> do
             wid <- io
             widgetSetSizeRequest wid (-1) (barHeight wcfg)
             boxPackEnd box wid PackNatural 0)
          (endWidgets wcfg)

        widgetShow window
        widgetShow box
        return window

  _ <- on screen screenMonitorsChanged refreshTaffyWindows

  startRefresher cfg $ postGUIAsync refreshTaffyWindows

  refreshTaffyWindows
  -- Reset the size of the Taffybar window if the monitor setup has
  -- changed, e.g., after a laptop user has attached an external
  -- monitor.
  mainGUI
  return ()
