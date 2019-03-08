{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
module System.Taffybar.Widget.SimpleClock
  ( textClockNew
  , textClockNewWith
  , defaultClockConfig
  , ClockConfig(..)
  ) where

import           Control.Monad.IO.Class
import           Data.Maybe
import qualified Data.Text as T
import           Data.Time.Calendar ( toGregorian )
import qualified Data.Time.Clock as Clock
import           Data.Time.Format
import           Data.Time.LocalTime
import qualified Data.Time.Locale.Compat as L
import qualified GI.Gdk as Gdk
import           GI.Gtk
import           System.Taffybar.Widget.Generic.PollingLabel
import           System.Taffybar.Widget.Util

diffLocalTime :: LocalTime -> LocalTime -> Clock.NominalDiffTime
diffLocalTime a b = Clock.diffUTCTime (localTimeToUTC utc a) (localTimeToUTC utc b)

-- | addLocalTime a b = a + b
addLocalTime :: Clock.NominalDiffTime -> LocalTime -> LocalTime
addLocalTime x = utcToLocalTime utc . Clock.addUTCTime x . localTimeToUTC utc

-- | This module implements a very simple text-based clock widget. The widget
-- also toggles a calendar widget when clicked. This calendar is not fancy at
-- all and has no data backend.

makeCalendar :: IO TimeZone -> IO Window
makeCalendar tzfn = do
  container <- windowNew WindowTypeToplevel
  cal <- calendarNew
  containerAdd container cal
  _ <- onWidgetShow container $ resetCalendarDate cal tzfn
  -- Hide the calendar instead of destroying it
  _ <- onWidgetDeleteEvent container $ \_ -> widgetHide container >> return True
  return container

resetCalendarDate :: Calendar -> IO TimeZone -> IO ()
resetCalendarDate cal tzfn = do
  tz <- tzfn
  current <- Clock.getCurrentTime
  let (y,m,d) = toGregorian $ localDay $ utcToLocalTime tz current
  calendarSelectMonth cal (fromIntegral m - 1) (fromIntegral y)
  calendarSelectDay cal (fromIntegral d)

toggleCalendar :: IsWidget w => w -> Window -> IO Bool
toggleCalendar w c = do
  isVis <- widgetGetVisible c
  if isVis
    then widgetHide c
    else do
      attachPopup w "Calendar" c
      displayPopup w c
  return True

-- | Create the widget. I recommend passing @Nothing@ for the TimeLocale
-- parameter. The format string can include Pango markup
-- (<http://developer.gnome.org/pango/stable/PangoMarkupFormat.html>).
textClockNew ::
  MonadIO m => Maybe L.TimeLocale -> String -> Double -> m GI.Gtk.Widget
textClockNew userLocale format interval =
  textClockNewWith cfg
  where
    cfg = defaultClockConfig { clockTimeLocale = userLocale
                             , clockFormatString = format
                             , clockUpdateStrategy = ConstantInterval interval
                             }

data ClockUpdateStrategy
  = ConstantInterval Double
  | RoundedTargetInterval Int Double
  deriving (Eq, Ord, Show)

data ClockConfig = ClockConfig
  { clockTimeZone :: Maybe TimeZone
  , clockTimeLocale :: Maybe L.TimeLocale
  , clockFormatString :: String
  , clockUpdateStrategy :: ClockUpdateStrategy
  } deriving (Eq, Ord, Show)

-- | A clock configuration that defaults to the current locale
defaultClockConfig :: ClockConfig
defaultClockConfig = ClockConfig
  { clockTimeZone = Nothing
  , clockTimeLocale = Nothing
  , clockFormatString = "%a %b %_d %r"
  , clockUpdateStrategy = RoundedTargetInterval 5 0.0
  }

systemGetTZ :: IO TimeZone
systemGetTZ = setTZ >> getCurrentTimeZone

-- | Old versions of time do not call localtime_r properly. We set the time zone
-- manually, if required.
setTZ :: IO ()
#if MIN_VERSION_time(1, 4, 2)
setTZ = return ()
#else
setTZ = c_tzsetp

foreign import ccall unsafe "time.h tzset"
  c_tzset :: IO ()
#endif

-- | A configurable text-based clock widget.  It currently allows for
-- a configurable time zone through the 'ClockConfig'.
--
-- See also 'textClockNew'.
textClockNewWith :: MonadIO m => ClockConfig -> m Widget
textClockNewWith ClockConfig
                   { clockTimeZone = userZone
                   , clockTimeLocale = userLocale
                   , clockFormatString = formatString
                   , clockUpdateStrategy = updateStrategy
                   } = liftIO $ do
  let getTZ = maybe systemGetTZ return userZone
      locale = fromMaybe (L.defaultTimeLocale) userLocale

  let getUserZonedTime = do
        zone <- getTZ
        currentTime <- Clock.getCurrentTime
        return $ utcToZonedTime zone currentTime

      doTimeFormat zonedTime = T.pack $ formatTime locale formatString $ zonedTime

      getRoundedTimeAndNextTarget = do
        zonedTime <- getUserZonedTime
        return $ case updateStrategy of
          ConstantInterval interval ->
            (doTimeFormat zonedTime, Nothing, interval)
          RoundedTargetInterval roundSeconds offset ->
            let roundSecondsDiffTime = fromIntegral roundSeconds
                addTheRound = addLocalTime roundSecondsDiffTime
                localTime = zonedTimeToLocalTime zonedTime
                ourLocalTimeOfDay = localTimeOfDay localTime
                seconds = round $ todSec ourLocalTimeOfDay
                secondsFactor = seconds `div` roundSeconds
                displaySeconds = secondsFactor * roundSeconds
                baseLocalTimeOfDay =
                  ourLocalTimeOfDay { todSec = fromIntegral displaySeconds }
                ourLocalTime =
                  localTime { localTimeOfDay = baseLocalTimeOfDay }
                roundedLocalTime =
                  if seconds `mod` roundSeconds > roundSeconds `div` 2
                  then addTheRound ourLocalTime
                  else ourLocalTime
                roundedZonedTime =
                  zonedTime { zonedTimeToLocalTime = roundedLocalTime }
                nextTarget = addTheRound ourLocalTime
                amountToWait = realToFrac $ diffLocalTime nextTarget localTime
            in (doTimeFormat roundedZonedTime, Nothing, amountToWait - offset)

  label <- pollingLabelWithVariableDelay getRoundedTimeAndNextTarget
  ebox <- eventBoxNew
  containerAdd ebox label
  eventBoxSetVisibleWindow ebox False
  cal <- makeCalendar getTZ
  _ <- onWidgetButtonPressEvent ebox $ onClick [Gdk.EventTypeButtonPress] $
       toggleCalendar label cal
  widgetShowAll ebox
  toWidget ebox

