-- | This module implements a very simple text-based clock widget.
-- The widget also toggles a calendar widget when clicked.  This
-- calendar is not fancy at all and has no data backend.
module System.Taffybar.SimpleClock (
  textClockNew,
  textClockNewWith,
  defaultClockConfig,
  ClockConfig(..)
  ) where

import Control.Monad.Trans ( MonadIO, liftIO )
import Data.Maybe ( fromMaybe )
import qualified Data.Time.Clock as Clock
import Data.Time.Format
import Data.Time.LocalTime
import Graphics.UI.Gtk
import System.Locale

import System.Taffybar.Widgets.PollingLabel

makeCalendar :: IO Window
makeCalendar = do
  container <- windowNew
  cal <- calendarNew
  containerAdd container cal
  -- prevent calendar from being destroyed, it can be only hidden:
  _ <- on container deleteEvent $ do
    liftIO (widgetHideAll container)
    return True
  return container

toggleCalendar :: (MonadIO m, WindowClass self, WidgetClass widget)
                  => widget -> self -> m Bool
toggleCalendar w c = liftIO $ do
  isVis <- get c widgetVisible
  case isVis of
    True -> widgetHideAll c
    False -> do
      windowSetKeepAbove c True
      windowStick c
      windowSetTypeHint c WindowTypeHintTooltip
      windowSetSkipTaskbarHint c True
      windowSetSkipPagerHint c True

      Just topLevel <- widgetGetAncestor w gTypeWindow
      let topLevelWindow = castToWindow topLevel
      windowSetTransientFor c topLevelWindow

      windowSetPosition c WinPosMouse
      (x,  y) <- windowGetPosition c
      (_, y') <- widgetGetSize w
      widgetShowAll c
      if y > y'
          then windowMove c x (y - y')
          else windowMove c x y'

  return True

-- | Create the widget.  I recommend passing @Nothing@ for the
-- TimeLocale parameter.  The format string can include Pango markup
-- (http://developer.gnome.org/pango/stable/PangoMarkupFormat.html).
textClockNew :: Maybe TimeLocale -> String -> Double -> IO Widget
textClockNew userLocale fmt updateSeconds =
  textClockNewWith cfg fmt updateSeconds
  where
    cfg = defaultClockConfig { clockTimeLocale = userLocale }

data ClockConfig = ClockConfig { clockTimeZone :: Maybe TimeZone
                               , clockTimeLocale :: Maybe TimeLocale
                               }
                               deriving (Eq, Ord, Show)

-- | A clock configuration that defaults to the current locale
defaultClockConfig :: ClockConfig
defaultClockConfig = ClockConfig Nothing Nothing

-- | A configurable text-based clock widget.  It currently allows for
-- a configurable time zone through the 'ClockConfig'.
--
-- See also 'textClockNew'.
textClockNewWith :: ClockConfig -> String -> Double -> IO Widget
textClockNewWith cfg fmt updateSeconds = do
  defaultTimeZone <- getCurrentTimeZone
  let timeLocale = fromMaybe defaultTimeLocale userLocale
      timeZone = fromMaybe defaultTimeZone userZone
  l    <- pollingLabelNew "" updateSeconds (getCurrentTime' timeLocale fmt timeZone)
  ebox <- eventBoxNew
  containerAdd ebox l
  eventBoxSetVisibleWindow ebox False
  cal <- makeCalendar
  _   <- on ebox buttonPressEvent (toggleCalendar l cal)
  widgetShowAll ebox
  return (toWidget ebox)
  where
    userZone = clockTimeZone cfg
    userLocale = clockTimeLocale cfg
    -- alternate getCurrentTime that takes a specific TZ
    getCurrentTime' :: TimeLocale -> String -> TimeZone -> IO String
    getCurrentTime' l f z =
      return . formatTime l f . utcToZonedTime z =<< Clock.getCurrentTime
