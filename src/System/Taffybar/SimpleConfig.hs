-----------------------------------------------------------------------------
-- |
-- Module      : System.Taffybar.SimpleConfig
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan A. Malison
-- Stability   : unstable
-- Portability : unportable
--
-- This module defines a simpler, but less flexible config system than the one
-- offered in "System.Taffybar.Context".
-----------------------------------------------------------------------------
module System.Taffybar.SimpleConfig
  ( SimpleTaffyConfig(..)
  , Position(..)
  , defaultSimpleTaffyConfig
  , simpleDyreTaffybar
  , simpleTaffybar
  , toTaffyConfig
  , useAllMonitors
  , usePrimaryMonitor
  , StrutSize(..)
  ) where

import qualified Control.Concurrent.MVar as MV
import           Control.Monad
import           Control.Monad.Trans.Class
import           Data.Default (Default(..))
import           Data.List
import           Data.Maybe
import           Data.Unique
import qualified GI.Gtk as Gtk
import           GI.Gdk
import           Graphics.UI.GIGtkStrut
import           System.Taffybar.Information.X11DesktopInfo
import           System.Taffybar
import qualified System.Taffybar.Context as BC (BarConfig(..), TaffybarConfig(..))
import           System.Taffybar.Context hiding (TaffybarConfig(..), BarConfig(..))
import           System.Taffybar.Util

-- | An ADT representing the edge of the monitor along which taffybar should be
-- displayed.
data Position = Top | Bottom deriving (Show, Eq)

-- | A configuration object whose interface is simpler than that of
-- 'TaffybarConfig'. Unless you have a good reason to use taffybar's more
-- advanced interface, you should stick to using this one.
data SimpleTaffyConfig = SimpleTaffyConfig
  {
  -- | The monitor number to put the bar on (default: 'usePrimaryMonitor')
    monitorsAction :: TaffyIO [Int]
  -- | Number of pixels to reserve for the bar (default: 30)
  , barHeight :: StrutSize
  -- | Number of additional pixels to reserve for the bar strut (default: 0)
  , barPadding :: Int
  -- | The position of the bar on the screen (default: 'Top')
  , barPosition :: Position
  -- | The number of pixels between widgets (default: 5)
  , widgetSpacing :: Int
  -- | Widget constructors whose output are placed at the beginning of the bar
  , startWidgets :: [TaffyIO Gtk.Widget]
  -- | Widget constructors whose output are placed in the center of the bar
  , centerWidgets :: [TaffyIO Gtk.Widget]
  -- | Widget constructors whose output are placed at the end of the bar
  , endWidgets :: [TaffyIO Gtk.Widget]
  -- | List of paths to CSS stylesheets that should be loaded at startup.
  , cssPaths :: [FilePath]
  -- | Hook to run at taffybar startup.
  , startupHook :: TaffyIO ()
  }

-- | Sensible defaults for most of the fields of 'SimpleTaffyConfig'. You'll
-- need to specify the widgets you want in the bar with 'startWidgets',
-- 'centerWidgets' and 'endWidgets'.
defaultSimpleTaffyConfig :: SimpleTaffyConfig
defaultSimpleTaffyConfig = SimpleTaffyConfig
  { monitorsAction = useAllMonitors
  , barHeight = ScreenRatio $ (1 / 27)
  , barPadding = 0
  , barPosition = Top
  , widgetSpacing = 5
  , startWidgets = []
  , centerWidgets = []
  , endWidgets = []
  , cssPaths = []
  , startupHook = return ()
  }

instance Default SimpleTaffyConfig where
  def = defaultSimpleTaffyConfig

-- | Convert a 'SimpleTaffyConfig' into a 'StrutConfig' that can be used with
-- gtk-strut.
toStrutConfig :: SimpleTaffyConfig -> Int -> StrutConfig
toStrutConfig SimpleTaffyConfig { barHeight = height
                                , barPadding = padding
                                , barPosition = pos
                                } monitor =
  defaultStrutConfig
  { strutHeight = height
  , strutYPadding = fromIntegral padding
  , strutXPadding = fromIntegral padding
  , strutAlignment = Center
  , strutMonitor = Just $ fromIntegral monitor
  , strutPosition =
      case pos of
        Top -> TopPos
        Bottom -> BottomPos
  }

toBarConfig :: SimpleTaffyConfig -> Int -> IO BC.BarConfig
toBarConfig config monitor = do
  let strutConfig = toStrutConfig config monitor
  barId <- newUnique
  return
    BC.BarConfig
    { BC.strutConfig = strutConfig
    , BC.widgetSpacing = fromIntegral $ widgetSpacing config
    , BC.startWidgets = startWidgets config
    , BC.centerWidgets = centerWidgets config
    , BC.endWidgets = endWidgets config
    , BC.barId = barId
    }

newtype SimpleBarConfigs = SimpleBarConfigs (MV.MVar [(Int, BC.BarConfig)])

-- | Convert a 'SimpleTaffyConfig' into a 'BC.TaffybarConfig' that can be used
-- with 'startTaffybar' or 'dyreTaffybar'.
toTaffyConfig :: SimpleTaffyConfig -> BC.TaffybarConfig
toTaffyConfig conf =
    def
    { BC.getBarConfigsParam = configGetter
    , BC.cssPaths = cssPaths conf
    , BC.startupHook = startupHook conf
    }
  where
    configGetter = do
      SimpleBarConfigs configsVar <-
        getStateDefault $ lift (SimpleBarConfigs <$> MV.newMVar [])
      monitorNumbers <- monitorsAction conf

      let lookupWithIndex barConfigs monitorNumber =
            (monitorNumber, lookup monitorNumber barConfigs)

          lookupAndUpdate barConfigs = do

            let (alreadyPresent, toCreate) =
                  partition (isJust . snd) $
                  map (lookupWithIndex barConfigs) monitorNumbers
                alreadyPresentConfigs = mapMaybe snd alreadyPresent

            newlyCreated <-
              mapM (forkM return (toBarConfig conf) . fst) toCreate
            let result = map snd newlyCreated ++ alreadyPresentConfigs
            return (barConfigs ++ newlyCreated, result)

      lift $ MV.modifyMVar configsVar lookupAndUpdate

-- | Start taffybar using dyre with a 'SimpleTaffybarConfig'.
simpleDyreTaffybar :: SimpleTaffyConfig -> IO ()
simpleDyreTaffybar conf = dyreTaffybar $ toTaffyConfig conf

-- | Start taffybar with a 'SimpleTaffybarConfig'.
simpleTaffybar :: SimpleTaffyConfig -> IO ()
simpleTaffybar conf = startTaffybar $ toTaffyConfig conf

getMonitorCount :: IO Int
getMonitorCount =
  fromIntegral <$> (screenGetDefault >>= maybe (return 0)
                    (screenGetDisplay >=> displayGetNMonitors))

-- | Supply this value for 'monitorsAction' to display the taffybar window on
-- all monitors.
useAllMonitors :: TaffyIO [Int]
useAllMonitors = lift $ do
  count <- getMonitorCount
  return [0..count-1]

-- | Supply this value for 'monitorsAction' to display the taffybar window only
-- on the primary monitor.
usePrimaryMonitor :: TaffyIO [Int]
usePrimaryMonitor =
  return . fromMaybe 0 <$> lift (withDefaultCtx getPrimaryOutputNumber)
