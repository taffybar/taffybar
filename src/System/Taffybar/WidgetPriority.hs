{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      : System.Taffybar.WidgetPriority
-- Copyright   : (c) Ivan A. Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan A. Malison
-- Stability   : unstable
-- Portability : unportable
--
-- Priority-based dynamic hiding of bar widgets when the bar runs out of
-- horizontal space.
--
-- Wrap any widget constructor with 'withPriority' to make it hideable:
--
-- > endWidgets = [clock, withPriority 10 battery, withPriority 3 weather]
--
-- Widgets without a priority are never hidden. When the natural widths of
-- all widgets exceed the width allocated to the bar, prioritized widgets
-- are hidden lowest-priority-first until everything fits. Hidden widgets
-- are shown again when enough space (plus a small hysteresis margin)
-- becomes available. The mechanism is entirely opt-in: bars that contain
-- no prioritized widgets get no visibility controller at all.
module System.Taffybar.WidgetPriority
  ( -- * Assigning priorities
    withPriority,
    setWidgetPriority,
    getWidgetPriority,

    -- * Hiding configuration
    DynamicHidingConfig (..),
    defaultDynamicHidingConfig,

    -- * Bar wiring (used by "System.Taffybar.Context")
    setupDynamicHiding,
  )
where

import Control.Monad (filterM, forM, forM_, unless, void, when)
import Control.Monad.IO.Class (MonadIO)
import Data.Default (Default (..))
import Data.IORef
import Data.Int (Int32)
import Data.List (sortOn)
import Data.Maybe (isJust)
import Data.Ord (Down (..))
import qualified Data.Text as T
import Foreign.Ptr (Ptr, intPtrToPtr, ptrToIntPtr)
import qualified GI.GLib as GLib
import qualified GI.GObject as GObject
import qualified GI.Gtk as Gtk
import System.Log.Logger (Priority (DEBUG), logM)
import Text.Printf (printf)

priorityLog :: Priority -> String -> IO ()
priorityLog = logM "System.Taffybar.WidgetPriority"

-- | GObject data key under which a widget's priority is stored.
priorityDataKey :: T.Text
priorityDataKey = "taffy-widget-priority"

-- | GObject data key marking widgets that were hidden by the visibility
-- controller (as opposed to widgets that hid themselves).
hiddenByControllerKey :: T.Text
hiddenByControllerKey = "taffy-priority-hidden"

-- Priorities are smuggled through g_object_set_data's @Ptr ()@ payload.
-- 0 (the null pointer) means "no priority set", so non-negative priorities
-- are stored shifted up by one.
encodePriority :: Int -> Ptr ()
encodePriority p = intPtrToPtr $ fromIntegral $ if p >= 0 then p + 1 else p

decodePriority :: Ptr () -> Maybe Int
decodePriority ptr =
  case fromIntegral (ptrToIntPtr ptr) :: Int of
    0 -> Nothing
    v
      | v > 0 -> Just (v - 1)
      | otherwise -> Just v

-- | Annotate an already-built widget with a hiding priority. The annotation
-- mechanism works on any 'GObject.Object', though the bar only consults it on
-- top-level section widgets.
setWidgetPriority :: (MonadIO m, GObject.IsObject o) => Int -> o -> m ()
setWidgetPriority p widget = do
  object <- GObject.toObject widget
  GObject.objectSetData object priorityDataKey (encodePriority p)

-- | Read back a priority set with 'setWidgetPriority' or 'withPriority'.
getWidgetPriority :: (MonadIO m, GObject.IsObject o) => o -> m (Maybe Int)
getWidgetPriority widget = do
  object <- GObject.toObject widget
  decodePriority <$> GObject.objectGetData object priorityDataKey

-- | Give a hiding priority to the widget produced by the given constructor.
-- Lower priorities are hidden first when the bar runs out of space; widgets
-- built without 'withPriority' are never hidden. This combinator does not
-- change the constructor's type, so prioritized and raw widgets mix freely
-- in the same widget list. Apply it as the outermost wrapper, after any
-- combinators that rebuild the widget inside a new container.
withPriority :: (MonadIO m) => Int -> m Gtk.Widget -> m Gtk.Widget
withPriority p buildWidget = do
  widget <- buildWidget
  setWidgetPriority p widget
  return widget

setHiddenByController :: (MonadIO m, GObject.IsObject o) => Bool -> o -> m ()
setHiddenByController flag widget = do
  object <- GObject.toObject widget
  GObject.objectSetData object hiddenByControllerKey $
    intPtrToPtr (if flag then 1 else 0)

getHiddenByController :: (MonadIO m, GObject.IsObject o) => o -> m Bool
getHiddenByController widget = do
  object <- GObject.toObject widget
  (/= 0) . ptrToIntPtr <$> GObject.objectGetData object hiddenByControllerKey

-- | Configuration for the dynamic hiding controller.
data DynamicHidingConfig = DynamicHidingConfig
  { -- | Extra pixels that must be available before a hidden widget is shown
    -- again. This hysteresis margin prevents widgets from flapping between
    -- hidden and shown when the bar is right at the edge of fitting.
    reshowSlackPx :: Int32,
    -- | CSS class added to widgets while they are hidden by the controller.
    hiddenCssClass :: T.Text,
    -- | How often (in milliseconds) to re-evaluate visibility even without a
    -- size-allocation event. Hidden widgets do not trigger relayout when
    -- their natural size changes, so without this they could stay hidden
    -- after shrinking enough to fit. 'Nothing' disables the periodic check.
    recheckIntervalMs :: Maybe Word
  }

-- | 'DynamicHidingConfig' with a 20px reshow margin and a 1s periodic check.
defaultDynamicHidingConfig :: DynamicHidingConfig
defaultDynamicHidingConfig =
  DynamicHidingConfig
    { reshowSlackPx = 20,
      hiddenCssClass = "priority-hidden",
      recheckIntervalMs = Just 1000
    }

instance Default DynamicHidingConfig where
  def = defaultDynamicHidingConfig

-- | Install a visibility controller on a bar box. The widget list should
-- contain the box's top-level section widgets in visual order. Does nothing
-- when no widget in the list carries a priority, so it is safe to call
-- unconditionally.
setupDynamicHiding ::
  DynamicHidingConfig -> Int32 -> Gtk.Box -> [Gtk.Widget] -> IO ()
setupDynamicHiding config spacing box widgets = do
  anyPrioritized <- not . null <$> filterM (fmap isJust . getWidgetPriority) widgets
  when anyPrioritized $ do
    priorityLog DEBUG $
      printf "Enabling dynamic widget hiding for a bar with %d widgets" (length widgets)
    destroyedRef <- newIORef False
    recheckPendingRef <- newIORef False

    let recompute = do
          destroyed <- readIORef destroyedRef
          unless destroyed $ applyVisibilityPass config spacing box widgets

        scheduleRecompute = do
          pending <- atomicModifyIORef' recheckPendingRef (True,)
          unless pending $
            void $
              GLib.idleAdd GLib.PRIORITY_DEFAULT_IDLE $ do
                writeIORef recheckPendingRef False
                recompute
                return False

    void $ Gtk.onWidgetSizeAllocate box $ const scheduleRecompute
    scheduleRecompute

    timeoutId <- forM (recheckIntervalMs config) $ \intervalMs ->
      GLib.timeoutAdd GLib.PRIORITY_DEFAULT_IDLE (fromIntegral intervalMs) $ do
        recompute
        return True

    void $ Gtk.onWidgetDestroy box $ do
      writeIORef destroyedRef True
      forM_ timeoutId GLib.sourceRemove

data WidgetInfo = WidgetInfo
  { wiWidget :: Gtk.Widget,
    wiPriority :: Maybe Int,
    wiVisible :: Bool,
    wiHiddenByUs :: Bool,
    wiNaturalWidth :: Int32
  }

-- | A widget is under the controller's authority when it has a priority and
-- is either visible or was hidden by the controller itself. Prioritized
-- widgets that hid themselves are left alone until they reappear.
wiManaged :: WidgetInfo -> Bool
wiManaged info =
  isJust (wiPriority info) && (wiVisible info || wiHiddenByUs info)

applyVisibilityPass ::
  DynamicHidingConfig -> Int32 -> Gtk.Box -> [Gtk.Widget] -> IO ()
applyVisibilityPass config spacing box widgets = do
  available <- Gtk.widgetGetAllocatedWidth box
  when (available > 1) $ do
    infos <- forM widgets $ \widget -> do
      priority <- getWidgetPriority widget
      visible <- Gtk.widgetGetVisible widget
      hiddenByUs <- getHiddenByController widget
      natural <- snd <$> Gtk.widgetGetPreferredWidth widget
      return
        WidgetInfo
          { wiWidget = widget,
            wiPriority = priority,
            wiVisible = visible,
            wiHiddenByUs = hiddenByUs,
            wiNaturalWidth = natural
          }

    let (managed, unmanaged) = (filter wiManaged infos, filter (not . wiManaged) infos)
        unmanagedVisible = filter wiVisible unmanaged
        -- Every visible widget is charged one unit of box spacing. This
        -- slightly overestimates the gaps GTK actually inserts, which only
        -- errs toward hiding a touch early.
        widthWithGap info = wiNaturalWidth info + spacing
        baseline = sum (map widthWithGap unmanagedVisible)

        -- Strict priority order: walk from highest priority down and cut
        -- off at the first widget that does not fit. sortOn is stable, so
        -- widgets with equal priority keep their visual order.
        ordered = sortOn (Down . wiPriority) managed

        decide _ [] = []
        decide used (info : rest)
          | required <= available =
              (info, True) : decide (used + widthWithGap info) rest
          | otherwise = map (,False) (info : rest)
          where
            slack = if wiVisible info then 0 else reshowSlackPx config
            required = used + widthWithGap info + slack

        decisions = decide baseline ordered

    forM_ decisions $ \(info, shouldShow) ->
      when (wiVisible info /= shouldShow) $ do
        let widget = wiWidget info
        styleContext <- Gtk.widgetGetStyleContext widget
        if shouldShow
          then do
            priorityLog DEBUG $
              printf "Showing widget with priority %s" (show $ wiPriority info)
            setHiddenByController False widget
            Gtk.widgetSetNoShowAll widget False
            Gtk.styleContextRemoveClass styleContext (hiddenCssClass config)
            Gtk.widgetShow widget
          else do
            priorityLog DEBUG $
              printf "Hiding widget with priority %s" (show $ wiPriority info)
            setHiddenByController True widget
            -- Some widgets refresh themselves with widgetShowAll on a
            -- parent container; no-show-all keeps those refreshes from
            -- undoing the controller's decision.
            Gtk.widgetSetNoShowAll widget True
            Gtk.styleContextAddClass styleContext (hiddenCssClass config)
            Gtk.widgetHide widget
