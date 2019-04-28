{-# LANGUAGE OverloadedStrings #-}
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
--------------------------------------------------------------------------------

module System.Taffybar.Widget.SimpleCommandButton (
  -- * Usage
  -- $usage
  simpleCommandButtonNew)
where

import           Control.Monad.IO.Class
import           GI.Gtk
import           System.Process
import qualified Data.Text as T

-- $usage
--
-- In order to use this widget add the following line to your
-- @taffybar.hs@ file:
--
-- > import System.Taffybar.Widget.SimpleCommandButton
-- > main = do
-- >   let cmdButton = simpleCommandButtonNew $ xterm -c "echo Hello World!"
--
-- Now you can use @cmdButton@ like any other Taffybar widget.

-- | Creates a new simple command button.
simpleCommandButtonNew :: MonadIO m =>
                          T.Text -- ^ Contents of the button's label.
                       -> T.Text -- ^ Command to execute. Should be in $PATH or an absolute path
                       -> m Widget
simpleCommandButtonNew  txt cmd = do
  but <- buttonNewWithLabel txt
  _ <- onButtonClicked but $ spawnCommand (T.unpack cmd) >> return ()
  toWidget but

