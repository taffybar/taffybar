{-# LANGUAGE OverloadedStrings #-}

-- | Debug widget for coordinated wakeup scheduler visibility.
module System.Taffybar.Widget.WakeupDebug
  ( WakeupDebugWidgetConfig (..),
    defaultWakeupDebugWidgetConfig,
    wakeupDebugWidgetNew,
    wakeupDebugWidgetNewWithConfig,
  )
where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan (TChan, readTChan)
import Control.Monad (forever, void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ask, runReaderT)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.List (nub, sort)
import qualified Data.Text as T
import Data.Word (Word64)
import qualified GI.Gdk as Gdk
import qualified GI.Gtk as Gtk
import Numeric (showFFloat)
import System.Taffybar.Context (TaffyIO)
import System.Taffybar.Information.Wakeup
  ( WakeupEvent (..),
    WakeupSchedulerEvent (..),
    getRegisteredWakeupIntervalsNanoseconds,
    getWakeupSchedulerEvents,
  )
import System.Taffybar.Util (postGUIASync)
import System.Taffybar.Widget.Util (onClick, widgetSetClassGI)

-- | Configuration for 'wakeupDebugWidgetNewWithConfig'.
data WakeupDebugWidgetConfig = WakeupDebugWidgetConfig
  { wakeupDebugFlashDurationMilliseconds :: Int,
    wakeupDebugExpandedByDefault :: Bool
  }
  deriving (Eq, Ord, Show)

defaultWakeupDebugWidgetConfig :: WakeupDebugWidgetConfig
defaultWakeupDebugWidgetConfig =
  WakeupDebugWidgetConfig
    { wakeupDebugFlashDurationMilliseconds = 180,
      wakeupDebugExpandedByDefault = False
    }

data WakeupDebugState = WakeupDebugState
  { wakeupDebugLastEvents :: [WakeupEvent],
    wakeupDebugRegisteredIntervals :: [Word64],
    wakeupDebugExpanded :: Bool
  }

wakeupDebugWidgetNew :: TaffyIO Gtk.Widget
wakeupDebugWidgetNew =
  wakeupDebugWidgetNewWithConfig defaultWakeupDebugWidgetConfig

wakeupDebugWidgetNewWithConfig :: WakeupDebugWidgetConfig -> TaffyIO Gtk.Widget
wakeupDebugWidgetNewWithConfig config = do
  context <- ask
  schedulerEvents <- getWakeupSchedulerEvents
  registered <- getRegisteredWakeupIntervalsNanoseconds
  liftIO $ do
    label <- Gtk.labelNew (Nothing :: Maybe T.Text)
    eventBox <- Gtk.eventBoxNew
    _ <- widgetSetClassGI eventBox "wakeup-debug"
    _ <- widgetSetClassGI label "wakeup-debug-label"
    Gtk.eventBoxSetVisibleWindow eventBox False
    Gtk.containerAdd eventBox label

    flashTokenRef <- newIORef (0 :: Int)
    stateRef <-
      newIORef
        WakeupDebugState
          { wakeupDebugLastEvents = [],
            wakeupDebugRegisteredIntervals = registered,
            wakeupDebugExpanded = wakeupDebugExpandedByDefault config
          }

    refreshWidget stateRef label eventBox

    let refreshRegisteredIntervals = runReaderT getRegisteredWakeupIntervalsNanoseconds context

    _ <-
      Gtk.onWidgetButtonPressEvent eventBox $
        onClick [Gdk.EventTypeButtonPress] $ do
          intervals <- refreshRegisteredIntervals
          _ <-
            atomicModifyIORef' stateRef $ \state ->
              let nextState =
                    state
                      { wakeupDebugRegisteredIntervals = intervals,
                        wakeupDebugExpanded = not (wakeupDebugExpanded state)
                      }
               in (nextState, nextState)
          refreshWidget stateRef label eventBox

    _ <- Gtk.onWidgetRealize eventBox $ do
      listenerThread <- forkIO $ wakeupDebugLoop schedulerEvents refreshRegisteredIntervals stateRef label eventBox flashTokenRef config
      void $ Gtk.onWidgetUnrealize eventBox $ killThread listenerThread

    Gtk.widgetShowAll eventBox
    Gtk.toWidget eventBox

wakeupDebugLoop ::
  TChan WakeupSchedulerEvent ->
  IO [Word64] ->
  IORef WakeupDebugState ->
  Gtk.Label ->
  Gtk.EventBox ->
  IORef Int ->
  WakeupDebugWidgetConfig ->
  IO ()
wakeupDebugLoop schedulerEvents refreshRegisteredIntervals stateRef label eventBox flashTokenRef config =
  forever $ do
    wakeupEvent <- atomically $ readTChan schedulerEvents
    intervals <- refreshRegisteredIntervals
    _ <-
      atomicModifyIORef' stateRef $ \state ->
        let nextState =
              state
                { wakeupDebugLastEvents = wakeupDueEvents wakeupEvent,
                  wakeupDebugRegisteredIntervals = intervals
                }
         in (nextState, nextState)
    refreshWidget stateRef label eventBox
    triggerFlash flashTokenRef (wakeupDebugFlashDurationMilliseconds config) eventBox

refreshWidget :: IORef WakeupDebugState -> Gtk.Label -> Gtk.EventBox -> IO ()
refreshWidget stateRef label eventBox = do
  state <- readIORef stateRef
  postGUIASync $ do
    Gtk.labelSetText label (renderWakeupDebugLabel state)
    Gtk.widgetSetTooltipText eventBox (Just (renderWakeupDebugTooltip state))

triggerFlash :: IORef Int -> Int -> Gtk.EventBox -> IO ()
triggerFlash tokenRef flashDurationMs eventBox = do
  token <- atomicModifyIORef' tokenRef $ \cur -> let next = cur + 1 in (next, next)
  postGUIASync $ setFlashClass eventBox True
  _ <- forkIO $ do
    threadDelay (flashDurationMs * 1000)
    latestToken <- readIORef tokenRef
    when (latestToken == token) $
      postGUIASync $ setFlashClass eventBox False
  pure ()

setFlashClass :: Gtk.EventBox -> Bool -> IO ()
setFlashClass eventBox shouldSet = do
  context <- Gtk.widgetGetStyleContext eventBox
  if shouldSet
    then Gtk.styleContextAddClass context "wakeup-debug-hit"
    else Gtk.styleContextRemoveClass context "wakeup-debug-hit"

renderWakeupDebugLabel :: WakeupDebugState -> T.Text
renderWakeupDebugLabel state =
  if wakeupDebugExpanded state
    then
      T.intercalate
        "\n"
        [ "wake " <> formatEventIntervals (wakeupDebugLastEvents state),
          "reg " <> formatIntervals (wakeupDebugRegisteredIntervals state)
        ]
    else "wake " <> formatEventIntervals (wakeupDebugLastEvents state)

renderWakeupDebugTooltip :: WakeupDebugState -> T.Text
renderWakeupDebugTooltip state =
  T.intercalate
    "\n"
    [ "Click to toggle registered interval list",
      "Last wakeup intervals: " <> formatEventIntervals (wakeupDebugLastEvents state),
      "Last wakeup ticks: " <> formatEventTicks (wakeupDebugLastEvents state),
      "Registered intervals: " <> formatIntervals (wakeupDebugRegisteredIntervals state)
    ]

formatEventIntervals :: [WakeupEvent] -> T.Text
formatEventIntervals events =
  formatIntervals $ fmap wakeupIntervalNanoseconds events

formatEventTicks :: [WakeupEvent] -> T.Text
formatEventTicks events
  | null events = "none"
  | otherwise = T.intercalate ", " $ fmap renderEvent events
  where
    renderEvent event =
      formatIntervalNanoseconds (wakeupIntervalNanoseconds event)
        <> "#"
        <> T.pack (show (wakeupTickCount event))

formatIntervals :: [Word64] -> T.Text
formatIntervals intervals
  | null intervals = "none"
  | otherwise = T.intercalate ", " $ fmap formatIntervalNanoseconds (sort (nub intervals))

formatIntervalNanoseconds :: Word64 -> T.Text
formatIntervalNanoseconds nanoseconds
  | nanoseconds `mod` secondsToNanoseconds == 0 =
      T.pack (show (nanoseconds `div` secondsToNanoseconds)) <> "s"
  | nanoseconds `mod` millisecondsToNanoseconds == 0 =
      T.pack (show (nanoseconds `div` millisecondsToNanoseconds)) <> "ms"
  | nanoseconds `mod` microsecondsToNanoseconds == 0 =
      T.pack (show (nanoseconds `div` microsecondsToNanoseconds)) <> "us"
  | otherwise = T.pack (showFFloat (Just 3) secondsValue "s")
  where
    secondsValue :: Double
    secondsValue = fromIntegral nanoseconds / 1000000000

secondsToNanoseconds :: Word64
secondsToNanoseconds = 1000000000

millisecondsToNanoseconds :: Word64
millisecondsToNanoseconds = 1000000

microsecondsToNanoseconds :: Word64
microsecondsToNanoseconds = 1000
