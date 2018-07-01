-- | This is a simple static image widget, and a polling image widget that
-- updates its contents by calling a callback at a set interval.
module System.Taffybar.Widget.Generic.Icon
  ( iconImageWidgetNew
  , pollingIconImageWidgetNew
  ) where

import Control.Concurrent ( forkIO, threadDelay )
import Control.Exception as E
import Control.Monad ( forever )
import Graphics.UI.Gtk
import qualified GI.Gtk
import System.Taffybar.Compat.GtkLibs

-- | Create a new widget that displays a static image
--
-- > iconImageWidgetNew path
--
-- returns a widget with icon at @path@.
iconImageWidgetNew :: FilePath -> IO GI.Gtk.Widget
iconImageWidgetNew path = imageNewFromFile path >>= putInBox

-- | Create a new widget that updates itself at regular intervals.  The
-- function
--
-- > pollingIconImageWidgetNew path interval cmd
--
-- returns a widget with initial icon at @path@.  The widget
-- forks a thread to update its contents every @interval@ seconds.
-- The command should return a FilePath of a valid icon.
--
-- If the IO action throws an exception, it will be swallowed and the
-- label will not update until the update interval expires.
pollingIconImageWidgetNew
  :: FilePath -- ^ Initial file path of the icon
  -> Double -- ^ Update interval (in seconds)
  -> IO FilePath -- ^ Command to run to get the input filepath
  -> IO GI.Gtk.Widget
pollingIconImageWidgetNew path interval cmd = do
  icon <- imageNewFromFile path
  _ <- on icon realize $ do
    _ <- forkIO $ forever $ do
      let tryUpdate = do
            str <- cmd
            postGUIAsync $ imageSetFromFile icon str
      E.catch tryUpdate ignoreIOException
      threadDelay $ floor (interval * 1000000)
    return ()
  putInBox icon

putInBox :: WidgetClass child => child -> IO GI.Gtk.Widget
putInBox icon = do
  box <- hBoxNew False 0
  boxPackStart box icon PackNatural 0
  widgetShowAll box
  toGIWidget $ toWidget box

ignoreIOException :: IOException -> IO ()
ignoreIOException _ = return ()
