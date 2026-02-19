--------------------------------------------------------------------------------

--------------------------------------------------------------------------------

-- |
-- Module      : System.Taffybar.Widget.ImageCommandButton
-- Copyright   : (c) Ulf Jasper
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ulf Jasper <ulf.jasper@web.de>
-- Stability   : unstable
-- Portability : unportable
--
-- Simple button which shows an image and runs a user defined command when
-- being clicked.
module System.Taffybar.Widget.ImageCommandButton
  ( -- * Usage
    -- $usage
    imageCommandButtonNew,
    imageCommandButtonNewFromName,
  )
where

import Control.Monad (void)
import Control.Monad.IO.Class
import qualified Data.Text as T
import GI.Gtk
import System.Process

-- $usage
--
-- In order to use this widget add the following lines to your
-- @taffybar.hs@ file:
--
-- > import System.Taffybar.Widget
-- > main = do
-- >   let fileIconButton = imageCommandButtonNew "/path/to/icon.png" "xterm"
-- >       themedIconButton = imageCommandButtonNewFromName "system-shutdown" "systemctl poweroff"
--
-- Now you can use these widgets like any other Taffybar widget.

-- | Creates a new image command button using an icon from a file path.
imageCommandButtonNew ::
  (MonadIO m) =>
  -- | File path of the image.
  FilePath ->
  -- | Command to execute. Should be in $PATH or an absolute path.
  T.Text ->
  m Widget
imageCommandButtonNew path cmd = do
  image <- imageNewFromFile path
  imageCommandButtonWithImage image cmd

-- | Creates a new image command button using an icon from the current GTK
-- theme.
imageCommandButtonNewFromName ::
  (MonadIO m) =>
  -- | Name of the icon in the current GTK icon theme.
  T.Text ->
  -- | Command to execute. Should be in $PATH or an absolute path.
  T.Text ->
  m Widget
imageCommandButtonNewFromName iconName cmd = do
  image <-
    imageNewFromIconName
      (Just iconName)
      (fromIntegral $ fromEnum IconSizeMenu)
  imageCommandButtonWithImage image cmd

imageCommandButtonWithImage ::
  (MonadIO m) =>
  Image ->
  T.Text ->
  m Widget
imageCommandButtonWithImage image cmd = do
  button <- buttonNew
  containerAdd button image
  void $ onButtonClicked button $ void $ spawnCommand $ T.unpack cmd
  widgetShowAll button
  toWidget button
