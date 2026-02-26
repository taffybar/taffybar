module Graphics.UI.GIGtkStrut
  ( defaultStrutConfig
  , StrutPosition(..)
  , StrutSize(..)
  , StrutAlignment(..)
  , StrutConfig(..)
  , buildStrutWindow
  , setupStrutWindow
  ) where

import           Control.Monad
import           Control.Monad.Fail (MonadFail)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Data.Default
import           Data.Int
import           Data.Maybe
import qualified Data.Text as T
import qualified GI.Gdk as Gdk
import qualified GI.Gtk as Gtk
import           Graphics.UI.EWMHStrut
import           System.Log.Logger
import           Text.Printf

strutLog :: MonadIO m => Priority -> String -> m ()
strutLog p s = liftIO $ logM "Graphics.UI.GIGtkStrut" p s

data StrutPosition
  = TopPos | BottomPos | LeftPos | RightPos
    deriving (Show, Read, Eq)

data StrutAlignment
  = Beginning | Center | End
    deriving (Show, Read, Eq)

data StrutSize
  = ExactSize Int32 | ScreenRatio Rational
    deriving (Show, Read, Eq)

data StrutConfig = StrutConfig
  { strutWidth :: StrutSize
  , strutHeight :: StrutSize
  , strutXPadding :: Int32
  , strutYPadding :: Int32
  , strutMonitor :: Maybe Int32
  , strutPosition :: StrutPosition
  , strutAlignment :: StrutAlignment
  , strutDisplayName :: Maybe T.Text
  } deriving (Show, Eq)

defaultStrutConfig = StrutConfig
  { strutWidth = ScreenRatio 1
  , strutHeight = ScreenRatio 1
  , strutXPadding = 0
  , strutYPadding = 0
  , strutMonitor = Nothing
  , strutPosition = TopPos
  , strutAlignment = Beginning
  , strutDisplayName = Nothing
  }

instance Default StrutConfig where
  def =
    StrutConfig
    { strutWidth = ScreenRatio 1
    , strutHeight = ScreenRatio 1
    , strutXPadding = 0
    , strutYPadding = 0
    , strutMonitor = Nothing
    , strutPosition = TopPos
    , strutAlignment = Beginning
    , strutDisplayName = Nothing
    }


-- | Build a strut window to the specifications provided by the 'StrutConfig'
-- argument.
buildStrutWindow :: (MonadFail m, MonadIO m) => StrutConfig -> m Gtk.Window
buildStrutWindow config = do
  window <- Gtk.windowNew Gtk.WindowTypeToplevel
  setupStrutWindow config window
  return window

-- | Configure the provided 'Gtk.Window' so that it has the properties specified
-- by the 'StrutConfig' argument.
setupStrutWindow :: (MonadFail m, MonadIO m) => StrutConfig -> Gtk.Window -> m ()
setupStrutWindow StrutConfig
                   { strutWidth = widthSize
                   , strutHeight = heightSize
                   , strutXPadding = xpadding
                   , strutYPadding = ypadding
                   , strutMonitor = monitorNumber
                   , strutPosition = position
                   , strutAlignment = alignment
                   , strutDisplayName = displayName
                   } window = do
  strutLog DEBUG "Starting strut window setup"
  Just display <- maybe Gdk.displayGetDefault Gdk.displayOpen displayName
  Just monitor <- maybe (Gdk.displayGetPrimaryMonitor display)
                  (Gdk.displayGetMonitor display) monitorNumber

  screen <- Gdk.displayGetDefaultScreen display

  monitorCount <- Gdk.displayGetNMonitors display
  allMonitors <- catMaybes <$> mapM (Gdk.displayGetMonitor display)
                 [0..(monitorCount-1)]
  allGeometries <- mapM Gdk.monitorGetGeometry allMonitors

  let getFullY geometry = (+) <$> Gdk.getRectangleY geometry
                              <*> Gdk.getRectangleHeight geometry
      getFullX geometry = (+) <$> Gdk.getRectangleX geometry
                              <*> Gdk.getRectangleWidth geometry

  -- The screen concept actually encapsulates things displayed across multiple
  -- monitors, which is why we take the maximum here -- what we really want to
  -- know is what is the farthest we can go in each direction on any monitor.
  screenWidth <- maximum <$> mapM getFullX allGeometries
  screenHeight <- maximum <$> mapM getFullY allGeometries

  geometry <- Gdk.newZeroGeometry

  monitorGeometry <- Gdk.monitorGetGeometry monitor
  monitorScaleFactor <- Gdk.monitorGetScaleFactor monitor
  monitorWidth <- Gdk.getRectangleWidth monitorGeometry
  monitorHeight <- Gdk.getRectangleHeight monitorGeometry
  monitorX <- Gdk.getRectangleX monitorGeometry
  monitorY <- Gdk.getRectangleY monitorGeometry

  let width =
        case widthSize of
          ExactSize w -> w
          ScreenRatio p ->
            floor $ p * fromIntegral (monitorWidth - (2 * xpadding))
      height =
        case heightSize of
          ExactSize h -> h
          ScreenRatio p ->
            floor $ p * fromIntegral (monitorHeight - (2 * ypadding))

  Gdk.setGeometryBaseWidth geometry width
  Gdk.setGeometryBaseHeight geometry height
  Gdk.setGeometryMinWidth geometry width
  Gdk.setGeometryMinHeight geometry height
  Gdk.setGeometryMaxWidth geometry width
  Gdk.setGeometryMaxHeight geometry height
  Gtk.windowSetGeometryHints window (Nothing :: Maybe Gtk.Window)
       (Just geometry) allHints

  let paddedHeight = height + 2 * ypadding
      paddedWidth = width + 2 * xpadding
      getAlignedPos dimensionPos dpadding monitorSize barSize =
        dimensionPos +
        case alignment of
          Beginning -> dpadding
          Center -> (monitorSize - barSize) `div` 2
          End -> monitorSize - barSize - dpadding
      xAligned = getAlignedPos monitorX xpadding monitorWidth width
      yAligned = getAlignedPos monitorY ypadding monitorHeight height
      (xPos, yPos) =
        case position of
          TopPos -> (xAligned, monitorY + ypadding)
          BottomPos -> (xAligned, monitorY + monitorHeight - height - ypadding)
          LeftPos -> (monitorX + xpadding, yAligned)
          RightPos -> (monitorX + monitorWidth - width - xpadding, yAligned)

  Gtk.windowSetTypeHint window Gdk.WindowTypeHintDock
  Gtk.windowSetScreen window screen
  Gtk.windowMove window xPos yPos
  Gtk.windowSetKeepBelow window True

  let ewmhSettings =
        case position of
          TopPos ->
            zeroStrutSettings
            { _top = monitorY + paddedHeight
            , _top_start_x = xPos - xpadding
            , _top_end_x = xPos + width + xpadding - 1
            }
          BottomPos ->
            zeroStrutSettings
            { _bottom = screenHeight - monitorY - monitorHeight + paddedHeight
            , _bottom_start_x = xPos - xpadding
            , _bottom_end_x = xPos + width + xpadding - 1
            }
          LeftPos ->
            zeroStrutSettings
            { _left = monitorX + paddedWidth
            , _left_start_y = yPos - ypadding
            , _left_end_y = yPos + height + ypadding - 1
            }
          RightPos ->
            zeroStrutSettings
            { _right = screenWidth - monitorX - monitorWidth + paddedWidth
            , _right_start_y = yPos - ypadding
            , _right_end_y = yPos + height + ypadding - 1
            }
      scaledStrutSettings = scaleStrutSettings monitorScaleFactor ewmhSettings
      setStrutProperties =
        void $ runMaybeT $ do
          gdkWindow <- MaybeT $ Gtk.widgetGetWindow window
          lift $ setStrut gdkWindow scaledStrutSettings
      logPairs =
        [ ("width", show width)
        , ("height", show height)
        , ("xPos", show xPos)
        , ("yPos", show yPos)
        , ("paddedWidth", show paddedWidth)
        , ("paddedHeight", show paddedHeight)
        , ("monitorWidth", show monitorWidth)
        , ("monitorHeight", show monitorHeight)
        , ("monitorX", show monitorX)
        , ("monitorY", show monitorY)
        , ("strutSettings", show ewmhSettings)
        , ("scaledStrutSettings", show scaledStrutSettings)
        ]

  strutLog DEBUG "Properties:"
  mapM_ (\(name, value) -> strutLog WARNING $ printf "%s: %s" name value) logPairs

  void $ Gtk.onWidgetRealize window setStrutProperties

allHints :: [Gdk.WindowHints]
allHints =
  [ Gdk.WindowHintsMinSize
  , Gdk.WindowHintsMaxSize
  , Gdk.WindowHintsBaseSize
  , Gdk.WindowHintsUserPos
  , Gdk.WindowHintsUserSize
  ]
