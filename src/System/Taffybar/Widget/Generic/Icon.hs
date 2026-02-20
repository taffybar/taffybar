-- | This is a simple static image widget, and a polling image widget that
-- updates its contents by calling a callback at a set interval.
module System.Taffybar.Widget.Generic.Icon
  ( iconImageWidgetNew,
    iconImageWidgetNewFromName,
    pollingIconImageWidgetNew,
    pollingIconImageWidgetNewFromName,
  )
where

import Control.Concurrent (killThread)
import Control.Exception as E
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Reader (ask, runReaderT)
import Data.GI.Gtk.Threading (postGUIASync)
import qualified Data.Text as T
import GI.Gtk
import System.Taffybar.Context (TaffyIO)
import System.Taffybar.Information.Wakeup (taffyForeverWithDelay)

-- | Create a new widget that displays a static image
--
-- > iconImageWidgetNew path
--
-- returns a widget with icon at @path@.
iconImageWidgetNew :: (MonadIO m) => FilePath -> m Widget
iconImageWidgetNew path = liftIO $ imageNewFromFile path >>= putInBox

-- | Create a new widget that displays a static image
--
-- > iconWidgetNewFromName name
--
-- returns a widget with the icon named @name@. Icon
-- names are sourced from the current GTK theme.
iconImageWidgetNewFromName :: (MonadIO m) => T.Text -> m Widget
iconImageWidgetNewFromName name =
  liftIO $
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
pollingIconImageWidgetNew ::
  -- | Initial file path of the icon
  FilePath ->
  -- | Update interval (in seconds)
  Double ->
  -- | Command to run to get the input filepath
  IO FilePath ->
  TaffyIO Widget
pollingIconImageWidgetNew path interval cmd =
  pollingIcon
    interval
    cmd
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
pollingIconImageWidgetNewFromName ::
  -- | Icon Name
  T.Text ->
  -- | Update interval (in seconds)
  Double ->
  -- | Command to run update the icon name
  IO T.Text ->
  TaffyIO Widget
pollingIconImageWidgetNewFromName name interval cmd =
  pollingIcon
    interval
    cmd
    (imageNewFromIconName (Just name) (fromIntegral $ fromEnum IconSizeMenu))
    (\image name' -> imageSetFromIconName image (Just name') $ fromIntegral $ fromEnum IconSizeMenu)

-- | Creates a polling icon.
pollingIcon ::
  -- | Update Interval (in seconds)
  Double ->
  -- | IO action that updates image's icon-name/filepath
  IO name ->
  -- | MonadIO action that creates the initial image.
  IO Image ->
  -- | MonadIO action that updates the image.
  (Image -> name -> IO b) ->
  -- | Polling Icon
  TaffyIO Widget
pollingIcon interval doUpdateName doInitImage doSetImage = do
  context <- ask
  image <- liftIO doInitImage
  liftIO $ do
    _ <- onWidgetRealize image $ do
      sampleThread <-
        runReaderT
          ( taffyForeverWithDelay interval $
              liftIO $
                E.catch
                  ( do
                      name' <- doUpdateName
                      postGUIASync $ void $ doSetImage image name'
                  )
                  ignoreIOException
          )
          context
      void $ onWidgetUnrealize image $ killThread sampleThread
    return ()
  liftIO $ putInBox image

putInBox :: (IsWidget child) => child -> IO Widget
putInBox icon = do
  box <- boxNew OrientationHorizontal 0
  boxPackStart box icon False False 0
  widgetShowAll box
  toWidget box

ignoreIOException :: IOException -> IO ()
ignoreIOException _ = return ()
