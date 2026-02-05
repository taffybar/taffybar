-- | This is a simple static image widget, and a polling image widget that
-- updates its contents by calling a callback at a set interval.
module System.Taffybar.Widget.Generic.Icon
  ( iconImageWidgetNew
  , iconImageWidgetNewFromName
  , pollingIconImageWidgetNew
  , pollingIconImageWidgetNewFromName
  ) where

import Control.Concurrent ( forkIO, threadDelay )
import qualified Data.Text as T
import Control.Exception as E
import Control.Monad ( forever, void )
import Control.Monad.IO.Class
import GI.Gtk
import System.Taffybar.Util

-- | Create a new widget that displays a static image
--
-- > iconImageWidgetNew path
--
-- returns a widget with icon at @path@.
iconImageWidgetNew :: MonadIO m => FilePath -> m Widget
iconImageWidgetNew path = liftIO $ imageNewFromFile path >>= putInBox

-- | Create a new widget that displays a static image
--
-- > iconWidgetNewFromName name
--
-- returns a widget with the icon named @name@. Icon
-- names are sourced from the current GTK theme.
iconImageWidgetNewFromName :: MonadIO m => T.Text -> m Widget
iconImageWidgetNewFromName name = liftIO $
  imageNewFromIconName (Just name) (fromIntegral $ fromEnum IconSizeMenu)
  >>= putInBox

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
  :: MonadIO m
  => FilePath -- ^ Initial file path of the icon
  -> Double -- ^ Update interval (in seconds)
  -> IO FilePath -- ^ Command to run to get the input filepath
  -> m Widget
pollingIconImageWidgetNew path interval cmd =
  pollingIcon interval cmd
    (imageNewFromFile path)
    (\image path' -> imageSetFromFile image (Just path'))

-- | Create a new widget that updates itself at regular intervals.  The
-- function
--
-- > pollingIconImageWidgetNewFromName name interval cmd
--
-- returns a widget with initial icon whose name is @name@.  The widget
-- forks a thread to update its contents every @interval@ seconds.
-- The command should return the name of a valid icon.
--
-- If the IO action throws an exception, it will be swallowed and the
-- label will not update until the update interval expires.
pollingIconImageWidgetNewFromName
  :: MonadIO m
  => T.Text    -- ^ Icon Name
  -> Double    -- ^ Update interval (in seconds)
  -> IO T.Text -- ^ Command to run update the icon name
  -> m Widget
pollingIconImageWidgetNewFromName name interval cmd =
  pollingIcon interval cmd
    (imageNewFromIconName (Just name) (fromIntegral $ fromEnum IconSizeMenu))
    (\image name' -> imageSetFromIconName image (Just name') $ fromIntegral $ fromEnum IconSizeMenu)

-- | Creates a polling icon.
pollingIcon
  :: MonadIO m
  => Double   -- ^ Update Interval (in seconds)
  -> IO name  -- ^ IO action that updates image's icon-name/filepath
  -> IO Image -- ^ MonadIO action that creates the initial image.
  -> (Image -> name -> IO b)
              -- ^ MonadIO action that updates the image.
  -> m Widget -- ^ Polling Icon
pollingIcon interval doUpdateName doInitImage doSetImage = liftIO $ do
  image <- doInitImage
  _ <- onWidgetRealize image $ do
    _ <- forkIO $ forever $ do
      let tryUpdate = liftIO $ do
            name' <- doUpdateName
            postGUIASync $ void $ doSetImage image name'
      E.catch tryUpdate ignoreIOException
      threadDelay $ floor (interval * 1000000)
    return ()
  putInBox image

putInBox :: IsWidget child => child -> IO Widget
putInBox icon = do
  box <- boxNew OrientationHorizontal 0
  boxAppend box icon False False 0
  widgetShow box
  toWidget box

ignoreIOException :: IOException -> IO ()
ignoreIOException _ = return ()
