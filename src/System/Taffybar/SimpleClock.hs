-- | This module implements a very simple text-based clock widget.
-- The widget also toggles a calendar widget when clicked.  This
-- calendar is not fancy at all and has no data backend.
module System.Taffybar.SimpleClock ( textClockNew ) where

import Control.Monad.Trans ( MonadIO, liftIO )
import Data.Time.Format
import Data.Time.LocalTime
import Graphics.UI.Gtk
import System.Locale

import System.Taffybar.Widgets.PollingLabel

getCurrentTime :: TimeLocale -> String -> IO String
getCurrentTime timeLocale fmt = do
  zt <- getZonedTime
  return $ formatTime timeLocale fmt zt

makeCalendar :: IO Window
makeCalendar = do
  container <- windowNew
  cal <- calendarNew
  containerAdd container cal
  return container

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

      widgetShowAll c

  return True

-- | Create the widget.  I recommend passing @Nothing@ for the
-- TimeLocale parameter.  The format string can include Pango markup
-- (http://developer.gnome.org/pango/stable/PangoMarkupFormat.html).
textClockNew :: Maybe TimeLocale -- ^ An TimeLocale - if not specified, the default is used.  This can be used to customize how different aspects of time are localized
                -> String -- ^ The time format string (see http://www.haskell.org/ghc/docs/6.12.2/html/libraries/time-1.1.4/Data-Time-Format.html)
                -> Double -- ^ The number of seconds to wait between clock updates
                -> IO Widget
textClockNew userLocale fmt updateSeconds = do
  let timeLocale = maybe defaultTimeLocale id userLocale

  -- Use a label to display the time.  Since we want to be able to
  -- click on it to show a calendar, we need an eventbox wrapper to
  -- actually receive events.
  l <- pollingLabelNew "" updateSeconds (getCurrentTime timeLocale fmt)

--  l <- labelNew Nothing
  ebox <- eventBoxNew
  containerAdd ebox l
  eventBoxSetVisibleWindow ebox False

  -- Allocate a hidden calendar and just show/hide it on clicks.
  cal <- makeCalendar

  _ <- on ebox buttonPressEvent (toggleCalendar l cal)
  widgetShowAll ebox

  -- The widget in the bar is actuall the eventbox
  return (toWidget ebox)
