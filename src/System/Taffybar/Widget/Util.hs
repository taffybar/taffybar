-----------------------------------------------------------------------------
-- |
-- Module      : System.Taffybar.Widget.Util
-- Copyright   : (c) José A. Romero L.
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : José A. Romero L. <escherdragon@gmail.com>
-- Stability   : unstable
-- Portability : unportable
--
-- Utility functions to facilitate building GTK interfaces.
--
-----------------------------------------------------------------------------

module System.Taffybar.Widget.Util where

import           Control.Concurrent ( forkIO )
import           Control.Monad ( when, forever, void )
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Maybe
import           Data.Functor ( ($>) )
import           Data.Int
import qualified Data.Text as T
import           Data.Tuple.Sequence
import qualified GI.GdkPixbuf.Objects.Pixbuf as GI
import qualified GI.GdkPixbuf.Objects.Pixbuf as PB
import qualified GI.Gtk
import           Graphics.UI.Gtk as Gtk
import           Graphics.UI.Gtk.General.StyleContext
import           System.Directory
import           System.FilePath.Posix
import           System.Taffybar.Information.XDG.DesktopEntry
import           System.Taffybar.Util
import           Text.Printf
import qualified Graphics.Rendering.Cairo as C
import qualified GI.Cairo
import Control.Monad.Trans.Reader (runReaderT)
import Graphics.Rendering.Cairo.Internal (Render(runRender))
import Foreign.Ptr (castPtr)
import Graphics.Rendering.Cairo.Types (Cairo(Cairo))


import           Paths_taffybar ( getDataDir )

-- | Execute the given action as a response to any of the given types
-- of mouse button clicks.
onClick :: [Click] -- ^ Types of button clicks to listen to.
        -> IO a    -- ^ Action to execute.
        -> EventM EButton Bool
onClick triggers action = tryEvent $ do
  click <- eventClick
  when (click `elem` triggers) $ void $ liftIO action

-- | Attach the given widget as a popup with the given title to the
-- given window. The newly attached popup is not shown initially. Use
-- the 'displayPopup' function to display it.
attachPopup :: (WidgetClass w, WindowClass wnd) =>
               w      -- ^ The widget to set as popup.
            -> String -- ^ The title of the popup.
            -> wnd    -- ^ The window to attach the popup to.
            -> IO ()
attachPopup widget title window = do
  set window [ windowTitle := title
             , windowTypeHint := WindowTypeHintTooltip
             , windowSkipTaskbarHint := True
             , windowSkipPagerHint := True
             , windowTransientFor :=> getWindow
             ]
  windowSetKeepAbove window True
  windowStick window
  where getWindow = do
          Just topLevelWindow <- fmap castToWindow <$> widgetGetAncestor widget gTypeWindow
          return topLevelWindow

-- | Display the given popup widget (previously prepared using the
-- 'attachPopup' function) immediately beneath (or above) the given
-- window.
displayPopup :: (WidgetClass w, WindowClass wnd) =>
                w   -- ^ The popup widget.
             -> wnd -- ^ The window the widget was attached to.
             -> IO ()
displayPopup widget window = do
  windowSetPosition window WinPosMouse
  (x, y ) <- windowGetPosition window
  (_, y') <- widgetGetSizeRequest widget
  widgetShowAll window
  if y > y'
    then windowMove window x (y - y')
    else windowMove window x y'

widgetGetAllocatedSize
  :: (WidgetClass self, MonadIO m)
  => self -> m (Int, Int)
widgetGetAllocatedSize widget =
  liftIO $
  sequenceT (widgetGetAllocatedWidth widget, widgetGetAllocatedHeight widget)

-- | Creates markup with the given foreground and background colors and the
-- given contents.
colorize :: String -- ^ Foreground color.
         -> String -- ^ Background color.
         -> String -- ^ Contents.
         -> String
colorize fg bg = printf "<span%s%s>%s</span>" (attr "fg" fg) (attr "bg" bg)
  where attr name value
          | null value = ""
          | otherwise  = printf " %scolor=\"%s\"" name value

backgroundLoop :: IO a -> IO ()
backgroundLoop = void . forkIO . forever

drawOn :: WidgetClass object => object -> IO () -> IO object
drawOn drawArea action = on drawArea realize action $> drawArea

widgetSetClass
  :: (Gtk.WidgetClass widget, MonadIO m)
  => widget -> String -> m widget
widgetSetClass widget klass = liftIO $ do
  context <- Gtk.widgetGetStyleContext widget
  styleContextAddClass context klass
  return widget

widgetSetClassGI :: (GI.Gtk.IsWidget b, MonadIO m) => b -> T.Text -> m b
widgetSetClassGI widget klass =
  GI.Gtk.widgetGetStyleContext widget >>=
    flip GI.Gtk.styleContextAddClass klass >> return widget

themeLoadFlags :: [GI.Gtk.IconLookupFlags]
themeLoadFlags =
  [ GI.Gtk.IconLookupFlagsGenericFallback
  , GI.Gtk.IconLookupFlagsUseBuiltin
  ]

getImageForDesktopEntry :: Int32 -> DesktopEntry -> IO (Maybe GI.Pixbuf)
getImageForDesktopEntry size entry = runMaybeT $ do
  iconName <- MaybeT $ return $ deIcon entry
  let iconNameText = T.pack iconName
  MaybeT $ do
    iconTheme <- GI.Gtk.iconThemeGetDefault
    hasIcon <- GI.Gtk.iconThemeHasIcon iconTheme iconNameText
    logPrintFDebug "System.Taffybar.Widget.Util" "Entry: %s" entry
    logPrintFDebug "System.Taffybar.Widget.Util" "Icon present: %s" hasIcon
    if hasIcon
    then
      GI.Gtk.iconThemeLoadIcon iconTheme iconNameText size themeLoadFlags
    else do
      exists <- doesFileExist iconName
      if isAbsolute iconName && exists
      then Just <$> GI.pixbufNewFromFile iconName
      else return Nothing

loadPixbufByName :: Int32 -> T.Text -> IO (Maybe GI.Pixbuf)
loadPixbufByName size name = do
  iconTheme <- GI.Gtk.iconThemeGetDefault
  hasIcon <- GI.Gtk.iconThemeHasIcon iconTheme name
  if hasIcon
  then GI.Gtk.iconThemeLoadIcon iconTheme name size themeLoadFlags
  else return Nothing

alignCenter :: (GI.Gtk.IsWidget o, MonadIO m) => o -> m ()
alignCenter widget =
  GI.Gtk.setWidgetValign widget GI.Gtk.AlignCenter >>
  GI.Gtk.setWidgetHalign widget GI.Gtk.AlignCenter

vFillCenter :: (GI.Gtk.IsWidget o, MonadIO m) => o -> m ()
vFillCenter widget =
  GI.Gtk.widgetSetVexpand widget True >>
  GI.Gtk.setWidgetValign widget GI.Gtk.AlignFill >>
  GI.Gtk.setWidgetHalign widget GI.Gtk.AlignCenter

pixbufNewFromFileAtScaleByHeight :: Int32 -> String -> IO PB.Pixbuf
pixbufNewFromFileAtScaleByHeight height name =
  PB.pixbufNewFromFileAtScale name (-1) height True

loadIcon :: Int32 -> String -> IO PB.Pixbuf
loadIcon height name =
  ((</> "icons" </> name) <$> getDataDir) >>=
  pixbufNewFromFileAtScaleByHeight height

setMinWidth :: (Gtk.WidgetClass w, MonadIO m) => Int -> w -> m w
setMinWidth width widget = liftIO $ do
  Gtk.widgetSetSizeRequest widget width (-1)
  return widget

renderWithContext :: GI.Cairo.Context -> C.Render () -> IO ()
renderWithContext ct r = GI.Cairo.withManagedPtr ct $ \p ->
  runReaderT (runRender r) (Cairo (castPtr p))

