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
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Functor ( ($>) )
import           Data.Int
import qualified Data.Text as T
import qualified GI.GdkPixbuf.Objects.Pixbuf as GI
import qualified GI.GdkPixbuf.Objects.Pixbuf as PB
import           GI.Gtk as Gtk
import qualified GI.Gdk as D
import           System.FilePath.Posix
import           System.Taffybar.Information.XDG.DesktopEntry
import           System.Taffybar.Util
import           Text.Printf

import           Paths_taffybar ( getDataDir )

-- | Execute the given action as a response to any of the given types
-- of mouse button clicks.
onClick :: [D.EventType] -- ^ Types of button clicks to listen to.
        -> IO a    -- ^ Action to execute.
        -> D.EventButton
        -> IO Bool
onClick triggers action btn = do
  click <- D.getEventButtonType btn
  if click `elem` triggers
  then action >> return True
  else return False

-- | Attach the given widget as a popup with the given title to the
-- given window. The newly attached popup is not shown initially. Use
-- the 'displayPopup' function to display it.
attachPopup :: (Gtk.IsWidget w, Gtk.IsWindow wnd) =>
               w      -- ^ The widget to set as popup.
            -> T.Text -- ^ The title of the popup.
            -> wnd    -- ^ The window to attach the popup to.
            -> IO ()
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
          windowGType <- gobjectType (undefined :: Window)
          Just ancestor <- Gtk.widgetGetAncestor widget windowGType
          castTo Window ancestor

-- | Display the given popup widget (previously prepared using the
-- 'attachPopup' function) immediately beneath (or above) the given
-- window.
displayPopup :: (Gtk.IsWidget w, Gtk.IsWidget wnd, Gtk.IsWindow wnd) =>
                w   -- ^ The popup widget.
             -> wnd -- ^ The window the widget was attached to.
             -> IO ()
displayPopup widget window = do
  windowSetPosition window WindowPositionMouse
  (x, y ) <- windowGetPosition window
  (_, y') <- widgetGetSizeRequest widget
  widgetShowAll window
  if y > y'
    then windowMove window x (y - y')
    else windowMove window x y'

widgetGetAllocatedSize
  :: (Gtk.IsWidget self, MonadIO m)
  => self -> m (Int, Int)
widgetGetAllocatedSize widget = do
  w <- Gtk.widgetGetAllocatedWidth widget
  h <- Gtk.widgetGetAllocatedHeight widget
  return (fromIntegral w, fromIntegral h)

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

drawOn :: Gtk.IsWidget object => object -> IO () -> IO object
drawOn drawArea action = Gtk.onWidgetRealize drawArea action $> drawArea

widgetSetClassGI :: (Gtk.IsWidget b, MonadIO m) => b -> T.Text -> m b
widgetSetClassGI widget klass =
  Gtk.widgetGetStyleContext widget >>=
    flip Gtk.styleContextAddClass klass >> return widget

themeLoadFlags :: [Gtk.IconLookupFlags]
themeLoadFlags =
  [ Gtk.IconLookupFlagsGenericFallback
  , Gtk.IconLookupFlagsUseBuiltin
  ]

getImageForDesktopEntry :: Int32 -> DesktopEntry -> IO (Maybe GI.Pixbuf)
getImageForDesktopEntry size entry = fmap join $ traverse run $ deIcon entry
  where run iconName =
          maybeTCombine (loadPixbufByName size $ T.pack iconName)
                        (getPixbufFromFilePath iconName)

loadPixbufByName :: Int32 -> T.Text -> IO (Maybe GI.Pixbuf)
loadPixbufByName size name = do
  iconTheme <- Gtk.iconThemeGetDefault
  hasIcon <- Gtk.iconThemeHasIcon iconTheme name
  if hasIcon
  then Gtk.iconThemeLoadIcon iconTheme name size themeLoadFlags
  else return Nothing

alignCenter :: (Gtk.IsWidget o, MonadIO m) => o -> m ()
alignCenter widget =
  Gtk.setWidgetValign widget Gtk.AlignCenter >>
  Gtk.setWidgetHalign widget Gtk.AlignCenter

vFillCenter :: (Gtk.IsWidget o, MonadIO m) => o -> m ()
vFillCenter widget =
  Gtk.widgetSetVexpand widget True >>
  Gtk.setWidgetValign widget Gtk.AlignFill >>
  Gtk.setWidgetHalign widget Gtk.AlignCenter

pixbufNewFromFileAtScaleByHeight :: Int32 -> String -> IO PB.Pixbuf
pixbufNewFromFileAtScaleByHeight height name =
  PB.pixbufNewFromFileAtScale name (-1) height True

loadIcon :: Int32 -> String -> IO PB.Pixbuf
loadIcon height name =
  ((</> "icons" </> name) <$> getDataDir) >>=
  pixbufNewFromFileAtScaleByHeight height

setMinWidth :: (Gtk.IsWidget w, MonadIO m) => Int -> w -> m w
setMinWidth width widget = liftIO $ do
  Gtk.widgetSetSizeRequest widget (fromIntegral width) (-1)
  return widget
