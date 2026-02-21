{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Text clock widget driven by coordinated wakeup channels.
module System.Taffybar.Widget.CoordinatedClock
  ( coordinatedTextClockNew,
    coordinatedTextClockNewWith,
    ClockConfig (..),
    ClockUpdateStrategy (..),
    defaultClockConfig,
  )
where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan (dupTChan, readTChan)
import Control.Exception.Enclosed (catchAny)
import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Time.Calendar (toGregorian)
import qualified Data.Time.Clock as Clock
import Data.Time.Format (formatTime)
import Data.Time.LocalTime
import qualified Data.Time.Locale.Compat as L
import qualified GI.Gdk as Gdk
import GI.Gtk
import System.Log.Logger (Priority (WARNING))
import System.Taffybar.Context (TaffyIO)
import System.Taffybar.Information.Wakeup (getWakeupChannelForDelay)
import System.Taffybar.Util (logPrintF, postGUIASync)
import System.Taffybar.Widget.SimpleClock
  ( ClockConfig (..),
    ClockUpdateStrategy (..),
    defaultClockConfig,
    textClockNewWith,
  )
import System.Taffybar.Widget.Util

-- | Create a coordinated-wakeup clock with fixed-interval updates.
coordinatedTextClockNew ::
  Maybe L.TimeLocale ->
  String ->
  Double ->
  TaffyIO Widget
coordinatedTextClockNew userLocale format interval =
  coordinatedTextClockNewWith $
    defaultClockConfig
      { clockTimeLocale = userLocale,
        clockFormatString = format,
        clockUpdateStrategy = ConstantInterval interval
      }

-- | Create a coordinated-wakeup clock using the same 'ClockConfig' as
-- 'textClockNewWith'. If the strategy cannot be represented as a coordinated
-- fixed interval, falls back to 'textClockNewWith'.
coordinatedTextClockNewWith :: ClockConfig -> TaffyIO Widget
coordinatedTextClockNewWith cfg@ClockConfig {clockUpdateStrategy = updateStrategy} =
  case coordinatedIntervalSeconds updateStrategy of
    Nothing -> textClockNewWith cfg
    Just intervalSeconds -> do
      wakeupChan <- getWakeupChannelForDelay intervalSeconds
      liftIO $ do
        let getTZ = maybe getCurrentTimeZone return (clockTimeZone cfg)
            locale = fromMaybe L.defaultTimeLocale (clockTimeLocale cfg)

            getUserZonedTime =
              utcToZonedTime <$> getTZ <*> Clock.getCurrentTime

            doTimeFormat zonedTime =
              T.pack $ formatTime locale (clockFormatString cfg) zonedTime

            getDisplayText = do
              zonedTime <- getUserZonedTime
              pure $ case updateStrategy of
                ConstantInterval _ -> doTimeFormat zonedTime
                RoundedTargetInterval roundSeconds _ ->
                  doTimeFormat $ roundedZonedTime roundSeconds zonedTime

            refreshClockLabel label =
              catchAny
                ( do
                    labelText <- getDisplayText
                    postGUIASync $ labelSetMarkup label labelText
                )
                ( logPrintF
                    logPath
                    WARNING
                    "Coordinated clock update failed: %s"
                )

        label <- labelNew (Nothing :: Maybe T.Text)
        _ <- widgetSetClassGI label "text-clock-label"
        void $ refreshClockLabel label

        _ <- onWidgetRealize label $ do
          ourWakeupChan <- atomically $ dupTChan wakeupChan
          threadId <- forkIO $ forever $ do
            void $ atomically $ readTChan ourWakeupChan
            void $ refreshClockLabel label
          void $ onWidgetUnrealize label $ killThread threadId

        ebox <- eventBoxNew
        _ <- widgetSetClassGI ebox "text-clock"
        containerAdd ebox label
        eventBoxSetVisibleWindow ebox False
        cal <- makeCalendar getTZ
        _ <-
          onWidgetButtonPressEvent ebox $
            onClick [Gdk.EventTypeButtonPress] $
              toggleCalendar label cal
        widgetShowAll ebox
        toWidget ebox

coordinatedIntervalSeconds :: ClockUpdateStrategy -> Maybe Double
coordinatedIntervalSeconds = \case
  ConstantInterval interval
    | interval > 0 -> Just interval
    | otherwise -> Nothing
  RoundedTargetInterval roundSeconds offset
    | roundSeconds > 0 && offset == 0 -> Just (fromIntegral roundSeconds)
    | otherwise -> Nothing

roundedZonedTime :: Int -> ZonedTime -> ZonedTime
roundedZonedTime roundSeconds zonedTime
  | roundSeconds <= 0 = zonedTime
  | otherwise =
      zonedTime
        { zonedTimeToLocalTime =
            if seconds `mod` roundSeconds > roundSeconds `div` 2
              then addLocalTime roundSecondsDiffTime baseLocalTime
              else baseLocalTime
        }
  where
    roundSecondsDiffTime = fromIntegral roundSeconds
    localTime = zonedTimeToLocalTime zonedTime
    ourLocalTimeOfDay = localTimeOfDay localTime
    seconds = round $ todSec ourLocalTimeOfDay
    secondsFactor = seconds `div` roundSeconds
    displaySeconds = secondsFactor * roundSeconds
    baseLocalTimeOfDay =
      ourLocalTimeOfDay {todSec = fromIntegral displaySeconds}
    baseLocalTime =
      localTime {localTimeOfDay = baseLocalTimeOfDay}

makeCalendar :: IO TimeZone -> IO Window
makeCalendar tzfn = do
  container <- windowNew WindowTypeToplevel
  cal <- calendarNew
  containerAdd container cal
  _ <- onWidgetShow container $ resetCalendarDate cal tzfn
  _ <- onWidgetDeleteEvent container $ \_ -> widgetHide container >> return True
  return container

resetCalendarDate :: Calendar -> IO TimeZone -> IO ()
resetCalendarDate cal tzfn = do
  tz <- tzfn
  current <- Clock.getCurrentTime
  let (y, m, d) = toGregorian $ localDay $ utcToLocalTime tz current
  calendarSelectMonth cal (fromIntegral m - 1) (fromIntegral y)
  calendarSelectDay cal (fromIntegral d)

toggleCalendar :: (IsWidget w) => w -> Window -> IO Bool
toggleCalendar w c = do
  isVis <- widgetGetVisible c
  if isVis
    then widgetHide c
    else do
      attachPopup w "Calendar" c
      displayPopup w c
  return True

logPath :: String
logPath = "System.Taffybar.Widget.CoordinatedClock"
