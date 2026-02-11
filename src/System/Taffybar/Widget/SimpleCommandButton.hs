--------------------------------------------------------------------------------

--------------------------------------------------------------------------------

-- |
-- Module      : System.Taffybar.Widget.SimpleCommandButton
-- Copyright   : (c) Ulf Jasper
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ulf Jasper <ulf.jasper@web.de>
-- Stability   : unstable
-- Portability : unportable
--
-- Simple button which runs a user defined command when being clicked
module System.Taffybar.Widget.SimpleCommandButton
  ( -- * Usage
    -- $usage
    simpleCommandButtonNew,
  )
where

import Control.Monad (void)
import Control.Monad.IO.Class
import qualified Data.Text as T
import GI.Gtk
import System.Process

-- $usage
--
-- In order to use this widget add the following line to your
-- @taffybar.hs@ file:
--
-- > import System.Taffybar.Widget
-- > main = do
-- >   let cmdButton = simpleCommandButtonNew "Hello World!" "xterm -e \"echo Hello World!; read x\""
--
-- Now you can use @cmdButton@ like any other Taffybar widget.

-- | Creates a new simple command button.
simpleCommandButtonNew ::
  (MonadIO m) =>
  -- | Contents of the button's label.
  T.Text ->
  -- | Command to execute. Should be in $PATH or an absolute path
  T.Text ->
  m Widget
simpleCommandButtonNew txt cmd = do
  button <- buttonNewWithLabel txt
  void $ onButtonClicked button $ void $ spawnCommand $ T.unpack cmd
  toWidget button
