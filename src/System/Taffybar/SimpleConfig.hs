-----------------------------------------------------------------------------
-- |
-- Module      : System.Taffybar.SimpleConfig
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan A. Malison
-- Stability   : unstable
-- Portability : unportable
-----------------------------------------------------------------------------
module System.Taffybar.SimpleConfig
  ( SimpleTaffyConfig(..)
  , Position(..)
  , defaultSimpleTaffyConfig
  , simpleTaffybar
  , toTaffyConfig
  , useAllMonitors
  , usePrimaryMonitor
  ) where

import qualified Control.Concurrent.MVar as MV
import           Control.Monad
import           Control.Monad.Trans.Class
import           Data.List
import           Data.Maybe
import           Data.Unique
import qualified GI.Gtk as Gtk
import           GI.Gdk
import           Graphics.UI.GIGtkStrut
import           System.Taffybar.Information.X11DesktopInfo
import           System.Taffybar
import qualified System.Taffybar.Context as BC (BarConfig(..), TaffybarConfig(..))
import           System.Taffybar.Context hiding (BarConfig(..), cssPath)
import           System.Taffybar.Util

-- | The side of the monitor at which taffybar should be displayed.
data Position = Top | Bottom deriving (Show, Eq)

-- | A configuration object whose interface is simpler than that of
-- 'TaffybarConfig'. Unless you have a good reason to use taffybar's more
-- advanced interface, you should stick to this one.
data SimpleTaffyConfig = SimpleTaffyConfig
  {
  -- | The xinerama/xrandr monitor number to put the bar on (default: PrimaryMonitor)
    monitorsAction :: TaffyIO [Int]
  -- | Number of pixels to reserve for the bar
  , barHeight :: Int
  -- | Number of additional pixels to reserve for the bar strut (default: 0)
  , barPadding :: Int
  -- | The position of the bar on the screen (default: Top)
  , barPosition :: Position
  -- | The number of pixels between widgets
  , widgetSpacing :: Int
  -- | Widget constructors whose output are placed at the beginning of the bar
  , startWidgets :: [TaffyIO Gtk.Widget]
  -- | Widget constructors whose output are placed in the center of the bar
  , centerWidgets :: [TaffyIO Gtk.Widget]
  -- | Widget constructors whose output are placed at the end of the bar
  , endWidgets :: [TaffyIO Gtk.Widget]
  -- | Optional path to CSS stylesheet (loaded in addition to stylesheet found
  -- in XDG data directory).
  , cssPath :: Maybe FilePath
  }

-- | Sensible defaults for most of the fields of 'SimpleTaffyConfig'. You'll
-- need to specify the widgets you want in the bar with 'startWidgets',
-- 'centerWidgets' and 'endWidgets'.
defaultSimpleTaffyConfig :: SimpleTaffyConfig
defaultSimpleTaffyConfig = SimpleTaffyConfig
  { monitorsAction = useAllMonitors
  , barHeight = 30
  , barPadding = 0
  , barPosition = Top
  , widgetSpacing = 5
  , startWidgets = []
  , centerWidgets = []
  , endWidgets = []
  , cssPath = Nothing
  }

toStrutConfig :: SimpleTaffyConfig -> Int -> StrutConfig
toStrutConfig SimpleTaffyConfig { barHeight = size
                                , barPadding = padding
                                , barPosition = pos
                                } monitor =
  defaultStrutConfig
  { strutHeight = ExactSize $ fromIntegral size
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

toTaffyConfig :: SimpleTaffyConfig -> TaffybarConfig
toTaffyConfig conf =
    defaultTaffybarConfig { getBarConfigsParam = configGetter
                          , BC.cssPath = cssPath conf
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

-- | Start taffybar using 'SimpleTaffybarConfig'.
simpleTaffybar :: SimpleTaffyConfig -> IO ()
simpleTaffybar conf = dyreTaffybar $ toTaffyConfig conf

getMonitorCount :: IO Int
getMonitorCount =
  fromIntegral <$> (screenGetDefault >>= maybe (return 0) (screenGetDisplay >=> displayGetNMonitors))

-- | Display a taffybar window on all monitors.
useAllMonitors :: TaffyIO [Int]
useAllMonitors = lift $ do
  count <- getMonitorCount
  return [0..count-1]

-- | Display the taffybar window on the primary monitor.
usePrimaryMonitor :: TaffyIO [Int]
usePrimaryMonitor =
  return . fromMaybe 0 <$> lift (withDefaultCtx getPrimaryOutputNumber)
