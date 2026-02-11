{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      : System.Taffybar.Widget.Util
-- Copyright   : (c) Ivan Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan Malison <IvanMalison@gmail.com>
-- Stability   : unstable
-- Portability : unportable
--
-- Utility functions to facilitate building GTK interfaces.
module System.Taffybar.Widget.Util where

import Control.Concurrent (forkIO)
import qualified Control.Concurrent.MVar as MV
import Control.Exception.Enclosed (catchAny)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Bifunctor (first)
import Data.Functor (($>))
import Data.GI.Base.Overloading (IsDescendantOf)
import Data.Int
import qualified Data.Text as T
import qualified GI.Gdk as D
import qualified GI.GdkPixbuf.Objects.Pixbuf as GI
import qualified GI.GdkPixbuf.Objects.Pixbuf as PB
import GI.Gtk as Gtk
import Paths_taffybar (getDataDir)
import StatusNotifier.Tray (scalePixbufToSize)
import System.Environment.XDG.DesktopEntry
import System.FilePath.Posix
import System.Log.Logger (Priority (..))
import System.Taffybar.Util
import Text.Printf

-- | Common record used for window icon widgets in workspace switchers.
data WindowIconWidget a = WindowIconWidget
  { iconContainer :: Gtk.EventBox,
    iconImage :: Gtk.Widget,
    iconWindow :: MV.MVar (Maybe a),
    iconForceUpdate :: IO ()
  }

-- | Construct the GTK widgets and CSS classes for a window icon widget, leaving
-- 'iconForceUpdate' as a placeholder to be filled in after the image widget is
-- created by 'scalingImageNew'.
--
-- The caller is responsible for creating the image widget and adding it to the
-- event box.
mkWindowIconWidgetBase :: (MonadIO m) => Maybe Int32 -> m (WindowIconWidget a)
mkWindowIconWidgetBase _mSize = liftIO $ do
  windowVar <- MV.newMVar Nothing
  ebox <- Gtk.eventBoxNew
  _ <- widgetSetClassGI ebox "window-icon-container"
  placeholder <- Gtk.toWidget ebox
  return
    WindowIconWidget
      { iconContainer = ebox,
        iconImage = placeholder,
        iconWindow = windowVar,
        iconForceUpdate = return ()
      }

-- | List of possible status class names for window icon widgets.
--
-- This is used to keep the style context clean by removing stale classes.
possibleStatusStrings :: [T.Text]
possibleStatusStrings = ["active", "urgent", "minimized", "normal", "inactive"]

-- | Add/remove classes on a widget while removing stale classes.
updateWidgetClasses ::
  (Foldable t1, Foldable t, Gtk.IsWidget a, MonadIO m) =>
  a ->
  t1 T.Text ->
  t T.Text ->
  m ()
updateWidgetClasses widget toAdd toRemove = do
  context <- Gtk.widgetGetStyleContext widget
  let hasClass = Gtk.styleContextHasClass context
      addIfMissing klass =
        hasClass klass >>= (`when` Gtk.styleContextAddClass context klass) . not
      removeIfPresent klass =
        unless (klass `elem` toAdd) $
          hasClass klass >>= (`when` Gtk.styleContextRemoveClass context klass)
  mapM_ removeIfPresent toRemove
  mapM_ addIfMissing toAdd

-- | Update a 'WindowIconWidget' with new per-slot data.
updateWindowIconWidgetState ::
  (MonadIO m) =>
  WindowIconWidget a ->
  Maybe a ->
  (a -> T.Text) ->
  (a -> T.Text) ->
  m ()
updateWindowIconWidgetState iconWidget windowData titleFn statusFn = do
  _ <- liftIO $ MV.swapMVar (iconWindow iconWidget) windowData
  Gtk.widgetSetTooltipText (iconContainer iconWidget) (titleFn <$> windowData)
  liftIO $ iconForceUpdate iconWidget
  let statusString = maybe "inactive" statusFn windowData
  updateWidgetClasses
    (iconContainer iconWidget)
    [statusString]
    possibleStatusStrings

scaledPixbufGetter ::
  (MonadIO m) =>
  (Int32 -> a -> m (Maybe GI.Pixbuf)) ->
  (Int32 -> a -> m (Maybe GI.Pixbuf))
scaledPixbufGetter getter size windowData =
  getter size windowData
    >>= traverse (liftIO . scalePixbufToSize size Gtk.OrientationHorizontal)

handlePixbufGetterException ::
  (MonadBaseControl IO m, Show a) =>
  (Priority -> String -> m ()) ->
  (Int32 -> a -> m (Maybe GI.Pixbuf)) ->
  Int32 ->
  a ->
  m (Maybe GI.Pixbuf)
handlePixbufGetterException logFn getter size windowData =
  catchAny (getter size windowData) $ \e -> do
    _ <-
      logFn WARNING $
        printf
          "Failed to get window icon for %s: %s"
          (show windowData)
          (show e)
    return Nothing

-- | Execute the given action as a response to any of the given types
-- of mouse button clicks.
onClick ::
  -- | Types of button clicks to listen to.
  [D.EventType] ->
  -- | Action to execute.
  IO a ->
  D.EventButton ->
  IO Bool
onClick triggers action btn = do
  click <- D.getEventButtonType btn
  if click `elem` triggers
    then action >> return True
    else return False

-- | Attach the given widget as a popup with the given title to the
-- given window. The newly attached popup is not shown initially. Use
-- the 'displayPopup' function to display it.
attachPopup ::
  (Gtk.IsWidget w, Gtk.IsWindow wnd) =>
  -- | The widget to set as popup.
  w ->
  -- | The title of the popup.
  T.Text ->
  -- | The window to attach the popup to.
  wnd ->
  IO ()
attachPopup widget title window = do
  windowSetTitle window title
  windowSetTypeHint window D.WindowTypeHintTooltip
  windowSetSkipTaskbarHint window True
  windowSetSkipPagerHint window True
  transient <- getWindow
  windowSetTransientFor window transient
  windowSetKeepAbove window True
  windowStick window
  where
    getWindow :: IO (Maybe Window)
    getWindow = do
      windowGType <- glibType @Window
      Just ancestor <- Gtk.widgetGetAncestor widget windowGType
      castTo Window ancestor

-- | Display the given popup widget (previously prepared using the
-- 'attachPopup' function) immediately beneath (or above) the given
-- window.
displayPopup ::
  (Gtk.IsWidget w, Gtk.IsWidget wnd, Gtk.IsWindow wnd) =>
  -- | The popup widget.
  w ->
  -- | The window the widget was attached to.
  wnd ->
  IO ()
displayPopup widget window = do
  windowSetPosition window WindowPositionMouse
  (x, y) <- windowGetPosition window
  (_, natReq) <- widgetGetPreferredSize =<< widgetGetToplevel widget
  y' <- getRequisitionHeight natReq
  widgetShowAll window
  if y > y'
    then windowMove window x (y - y')
    else windowMove window x y'

widgetGetAllocatedSize ::
  (Gtk.IsWidget self, MonadIO m) =>
  self -> m (Int, Int)
widgetGetAllocatedSize widget = do
  w <- Gtk.widgetGetAllocatedWidth widget
  h <- Gtk.widgetGetAllocatedHeight widget
  return (fromIntegral w, fromIntegral h)

-- | Creates markup with the given foreground and background colors and the
-- given contents.
colorize ::
  -- | Foreground color.
  String ->
  -- | Background color.
  String ->
  -- | Contents.
  String ->
  String
colorize fg bg = printf "<span%s%s>%s</span>" (attr ("fg" :: String) fg :: String) (attr ("bg" :: String) bg :: String)
  where
    attr name value
      | null value = ""
      | otherwise = printf " %scolor=\"%s\"" name value

backgroundLoop :: IO a -> IO ()
backgroundLoop = void . forkIO . forever

drawOn :: (Gtk.IsWidget object) => object -> IO () -> IO object
drawOn drawArea action = Gtk.onWidgetRealize drawArea action $> drawArea

widgetSetClassGI :: (Gtk.IsWidget b, MonadIO m) => b -> T.Text -> m b
widgetSetClassGI widget klass =
  Gtk.widgetGetStyleContext widget
    >>= flip Gtk.styleContextAddClass klass
    >> return widget

themeLoadFlags :: [Gtk.IconLookupFlags]
themeLoadFlags =
  [ Gtk.IconLookupFlagsGenericFallback,
    Gtk.IconLookupFlagsUseBuiltin
  ]

getImageForDesktopEntry :: Int32 -> DesktopEntry -> IO (Maybe GI.Pixbuf)
getImageForDesktopEntry size de = getImageForMaybeIconName (T.pack <$> deIcon de) size

getImageForMaybeIconName :: Maybe T.Text -> Int32 -> IO (Maybe GI.Pixbuf)
getImageForMaybeIconName mIconName size =
  join <$> traverse (`getImageForIconName` size) mIconName

getImageForIconName :: T.Text -> Int32 -> IO (Maybe GI.Pixbuf)
getImageForIconName iconName size =
  maybeTCombine
    (loadPixbufByName size iconName)
    ( getPixbufFromFilePath (T.unpack iconName)
        >>= traverse (scalePixbufToSize size Gtk.OrientationHorizontal)
    )

loadPixbufByName :: Int32 -> T.Text -> IO (Maybe GI.Pixbuf)
loadPixbufByName size name = do
  iconTheme <- Gtk.iconThemeGetDefault
  hasIcon <- Gtk.iconThemeHasIcon iconTheme name
  if hasIcon
    then Gtk.iconThemeLoadIcon iconTheme name size themeLoadFlags
    else return Nothing

alignCenter :: (Gtk.IsWidget o, MonadIO m) => o -> m ()
alignCenter widget =
  Gtk.setWidgetValign widget Gtk.AlignCenter
    >> Gtk.setWidgetHalign widget Gtk.AlignCenter

vFillCenter :: (Gtk.IsWidget o, MonadIO m) => o -> m ()
vFillCenter widget =
  Gtk.widgetSetVexpand widget True
    >> Gtk.setWidgetValign widget Gtk.AlignFill
    >> Gtk.setWidgetHalign widget Gtk.AlignCenter

pixbufNewFromFileAtScaleByHeight :: Int32 -> String -> IO (Either String PB.Pixbuf)
pixbufNewFromFileAtScaleByHeight height name =
  fmap (handleResult . first show) $
    catchGErrorsAsLeft $
      PB.pixbufNewFromFileAtScale name (-1) height True
  where
    handleResult = (maybe (Left "gdk function returned NULL") Right =<<)

loadIcon :: Int32 -> String -> IO (Either String PB.Pixbuf)
loadIcon height name =
  getDataDir
    >>= pixbufNewFromFileAtScaleByHeight height . (</> "icons" </> name)

setMinWidth :: (Gtk.IsWidget w, MonadIO m) => Int -> w -> m w
setMinWidth width widget = liftIO $ do
  Gtk.widgetSetSizeRequest widget (fromIntegral width) (-1)
  return widget

addClassIfMissing ::
  (IsDescendantOf Widget a, MonadIO m, GObject a) => T.Text -> a -> m ()
addClassIfMissing klass widget = do
  context <- Gtk.widgetGetStyleContext widget
  Gtk.styleContextHasClass context klass
    >>= (`when` Gtk.styleContextAddClass context klass) . not

removeClassIfPresent ::
  (IsDescendantOf Widget a, MonadIO m, GObject a) => T.Text -> a -> m ()
removeClassIfPresent klass widget = do
  context <- Gtk.widgetGetStyleContext widget
  Gtk.styleContextHasClass context klass
    >>= (`when` Gtk.styleContextRemoveClass context klass)

-- | Wrap a widget with two container boxes. The inner box will have the class
-- "inner-pad", and the outer box will have the class "outer-pad". These boxes
-- can be used to add padding between the outline of the widget and its
-- contents, or for the purpose of displaying a different background behind the
-- widget.
buildPadBox :: (MonadIO m) => Gtk.Widget -> m Gtk.Widget
buildPadBox contents = liftIO $ do
  innerBox <- Gtk.boxNew Gtk.OrientationHorizontal 0
  outerBox <- Gtk.boxNew Gtk.OrientationHorizontal 0
  Gtk.setWidgetValign innerBox Gtk.AlignFill
  Gtk.setWidgetValign outerBox Gtk.AlignFill
  Gtk.containerAdd innerBox contents
  Gtk.containerAdd outerBox innerBox
  _ <- widgetSetClassGI innerBox "inner-pad"
  _ <- widgetSetClassGI outerBox "outer-pad"
  Gtk.widgetShow outerBox
  Gtk.widgetShow innerBox
  Gtk.toWidget outerBox

buildContentsBox :: (MonadIO m) => Gtk.Widget -> m Gtk.Widget
buildContentsBox widget = liftIO $ do
  contents <- Gtk.boxNew Gtk.OrientationHorizontal 0
  Gtk.containerAdd contents widget
  _ <- widgetSetClassGI contents "contents"
  Gtk.widgetShowAll contents
  Gtk.toWidget contents >>= buildPadBox

-- | Combine an icon widget and a label widget in a horizontal box with
-- standardised CSS classes. The box gets class @icon-label@, the first child
-- gets @icon@, and the second child gets @label@.
buildIconLabelBox :: (MonadIO m) => Gtk.Widget -> Gtk.Widget -> m Gtk.Widget
buildIconLabelBox iconWidget labelWidget = liftIO $ do
  box <- Gtk.boxNew Gtk.OrientationHorizontal 0
  _ <- widgetSetClassGI iconWidget "icon"
  _ <- widgetSetClassGI labelWidget "label"
  Gtk.containerAdd box iconWidget
  Gtk.containerAdd box labelWidget
  _ <- widgetSetClassGI box "icon-label"
  Gtk.widgetShowAll box
  Gtk.toWidget box

-- | Build a 'Gtk.Overlay' from a base widget plus one or more overlay widgets,
-- and mark overlays as "pass through" so they don't capture clicks/scrolls.
--
-- This is useful for workspace widgets that overlay a label on top of icons.
buildOverlayWithPassThrough :: (MonadIO m) => Gtk.Widget -> [Gtk.Widget] -> m Gtk.Widget
buildOverlayWithPassThrough base overlays = liftIO $ do
  overlay <- Gtk.overlayNew
  Gtk.containerAdd overlay base
  forM_ overlays $ \w -> do
    Gtk.overlayAddOverlay overlay w
    Gtk.overlaySetOverlayPassThrough overlay w True
  Gtk.toWidget overlay

-- | Wrap a widget in an event box aligned to the bottom-left.
--
-- This is used by workspace widgets to overlay a label on top of icons in a
-- consistent way across different backends.
buildBottomLeftAlignedBox :: (MonadIO m) => T.Text -> Gtk.Widget -> m Gtk.Widget
buildBottomLeftAlignedBox boxClass child = liftIO $ do
  ebox <- Gtk.eventBoxNew
  _ <- widgetSetClassGI ebox boxClass
  Gtk.widgetSetHalign ebox Gtk.AlignStart
  Gtk.widgetSetValign ebox Gtk.AlignEnd
  Gtk.containerAdd ebox child
  Gtk.toWidget ebox

-- | Compute a window icon strip layout given min/max icon config and a sorted
-- list of items.
--
-- Returns (effectiveMinIcons, targetLen, paddedItems) where:
-- - effectiveMinIcons is @min minIcons maxIcons@ when maxIcons is set
-- - targetLen is at least effectiveMinIcons and large enough for shown items
-- - paddedItems is exactly targetLen elements long (Just items, then Nothings)
computeIconStripLayout :: Int -> Maybe Int -> [a] -> (Int, Int, [Maybe a])
computeIconStripLayout minIcons maxIcons items =
  let itemCount = length items
      maxNeeded = maybe itemCount (min itemCount) maxIcons
      effectiveMinIcons = maybe minIcons (min minIcons) maxIcons
      targetLen = max effectiveMinIcons maxNeeded
      shownItems = take maxNeeded items
      paddedItems =
        map Just shownItems ++ replicate (targetLen - length shownItems) Nothing
   in (effectiveMinIcons, targetLen, paddedItems)

-- | CSS class name for a window icon given its state.
--
-- This matches the classes used by the workspaces widgets: `active`, `urgent`,
-- `minimized`, `normal` (and `inactive` when there is no window).
windowStatusClassFromFlags :: Bool -> Bool -> Bool -> T.Text
windowStatusClassFromFlags minimized active urgent
  | minimized = "minimized"
  | active = "active"
  | urgent = "urgent"
  | otherwise = "normal"

-- | Keep a pool of widget "slots" in sync with a desired list of per-slot data.
--
-- The pool is only grown (never shrunk). Widgets within the desired length are
-- shown and updated; widgets beyond it are updated with 'Nothing' and hidden.
syncWidgetPool ::
  (MonadIO m, Gtk.IsWidget child) =>
  Gtk.Box ->
  [w] ->
  [Maybe a] ->
  (Int -> m w) ->
  (w -> child) ->
  (w -> Maybe a -> m ()) ->
  m [w]
syncWidgetPool container pool desired mkOne getChild updateOne = do
  let targetLen = length desired

  pool' <-
    if length pool >= targetLen
      then return pool
      else do
        let start = length pool
        newOnes <- forM [start .. targetLen - 1] $ \i -> do
          w <- mkOne i
          liftIO $ Gtk.containerAdd container (getChild w)
          return w
        liftIO $ Gtk.widgetShowAll container
        return (pool ++ newOnes)

  forM_ (zip3 [0 :: Int ..] pool' (desired ++ repeat Nothing)) $ \(i, w, payload) ->
    if i < targetLen
      then do
        liftIO $ Gtk.widgetShow (getChild w)
        updateOne w payload
      else do
        updateOne w Nothing
        liftIO $ Gtk.widgetHide (getChild w)

  return pool'
