{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
-- | This module implements a very simple text-based clock widget.
-- The widget also toggles a calendar widget when clicked.  This
-- calendar is not fancy at all and has no data backend.
module System.Taffybar.Widget.SimpleClock
  ( textClockNew
  , textClockNewWith
  , defaultClockConfig
  , ClockConfig(..)
  ) where

import           Control.Monad.IO.Class
import           Data.Time.Calendar ( toGregorian )
import qualified Data.Time.Clock as Clock
import           Data.Time.Format
import           Data.Time.LocalTime
import qualified Data.Time.Locale.Compat as L
import qualified GI.Gtk
import System.Taffybar.Compat.GtkLibs
import           Graphics.UI.Gtk

import           System.Taffybar.Widget.Generic.PollingLabel
import           System.Taffybar.Widget.Util

makeCalendar :: IO TimeZone -> IO Window
makeCalendar tzfn = do
  container <- windowNew
  cal <- calendarNew
  containerAdd container cal
  -- update the date on show
  _ <- on container showSignal $ resetCalendarDate cal tzfn
  -- prevent calendar from being destroyed, it can be only hidden:
  _ <- on container deleteEvent $ do
    liftIO (widgetHide container)
    return True
  return container

resetCalendarDate :: Calendar -> IO TimeZone -> IO ()
resetCalendarDate cal tzfn = do
  tz <- tzfn
  current <- Clock.getCurrentTime
  let (y,m,d) = toGregorian $ localDay $ utcToLocalTime tz current
  calendarSelectMonth cal (fromIntegral m - 1) (fromIntegral y)
  calendarSelectDay cal (fromIntegral d)

toggleCalendar :: WidgetClass w => w -> Window -> IO Bool
toggleCalendar w c = do
  isVis <- get c widgetVisible
  if isVis
    then widgetHide c
    else do
      attachPopup w "Calendar" c
      displayPopup w c
  return True

-- | Create the widget. I recommend passing @Nothing@ for the TimeLocale
-- parameter. The format string can include Pango markup
-- (http://developer.gnome.org/pango/stable/PangoMarkupFormat.html).
textClockNew :: MonadIO m => Maybe L.TimeLocale -> String -> Double -> m GI.Gtk.Widget
textClockNew userLocale s t =
  textClockNewWith cfg s t >>= toGIWidget
  where
    cfg = defaultClockConfig { clockTimeLocale = userLocale }

data ClockConfig = ClockConfig { clockTimeZone :: Maybe TimeZone
                               , clockTimeLocale :: Maybe L.TimeLocale
                               }
                               deriving (Eq, Ord, Show)

-- | A clock configuration that defaults to the current locale
defaultClockConfig :: ClockConfig
defaultClockConfig = ClockConfig Nothing Nothing

data TimeInfo = TimeInfo { getTZ :: IO TimeZone
                         , getLocale :: IO L.TimeLocale
                         }

systemGetTZ :: IO TimeZone
systemGetTZ = setTZ >> getCurrentTimeZone

-- | Old versions of time do not call localtime_r properly.  We set
-- the time zone manually, if required.
setTZ :: IO ()
#if MIN_VERSION_time(1, 4, 2)
setTZ = return ()
#else
setTZ = c_tzset

foreign import ccall unsafe "time.h tzset"
  c_tzset :: IO ()
#endif

-- | A configurable text-based clock widget.  It currently allows for
-- a configurable time zone through the 'ClockConfig'.
--
-- See also 'textClockNew'.
textClockNewWith :: MonadIO m => ClockConfig -> String -> Double -> m Widget
textClockNewWith cfg fmt updateSeconds = liftIO $ do
  let ti = TimeInfo { getTZ = maybe systemGetTZ return userZone
                    , getLocale = maybe (return L.defaultTimeLocale) return userLocale
                    }
  l    <- pollingLabelNew "" updateSeconds (getCurrentTime' ti fmt) >>= fromGIWidget
  ebox <- eventBoxNew
  containerAdd ebox l
  eventBoxSetVisibleWindow ebox False
  cal <- makeCalendar $ getTZ ti
  _ <- on ebox buttonPressEvent $ onClick [SingleClick] (toggleCalendar l cal)
  widgetShowAll ebox
  return (toWidget ebox)
  where
    userZone = clockTimeZone cfg
    userLocale = clockTimeLocale cfg
    -- alternate getCurrentTime that takes a specific TZ
    getCurrentTime' :: TimeInfo -> String -> IO String
    getCurrentTime' ti f = do
      l <- getLocale ti
      z <- getTZ ti
      t <- Clock.getCurrentTime
      return $ formatTime l f $ utcToZonedTime z t

