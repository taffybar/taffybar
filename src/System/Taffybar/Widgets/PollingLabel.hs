{-# LANGUAGE ScopedTypeVariables #-}
-- | This is a simple text widget that updates its contents by calling
-- a callback at a set interval.
module System.Taffybar.Widgets.PollingLabel ( pollingLabelNew ) where

import Control.Concurrent ( forkIO, threadDelay )
import Control.Exception.Enclosed as E
import Control.Monad ( forever )
import Graphics.UI.Gtk

-- | Create a new widget that updates itself at regular intervals.  The
-- function
--
-- > pollingLabelNew initialString cmd interval
--
-- returns a widget with initial text @initialString@.  The widget
-- forks a thread to update its contents every @interval@ seconds.
-- The command should return a string with any HTML entities escaped.
-- This is not checked by the function, since Pango markup shouldn't
-- be escaped.  Proper input sanitization is up to the caller.
--
-- If the IO action throws an exception, it will be swallowed and the
-- label will not update until the update interval expires.
pollingLabelNew :: String       -- ^ Initial value for the label
                   -> Double    -- ^ Update interval (in seconds)
                   -> IO String -- ^ Command to run to get the input string
                   -> IO Widget
pollingLabelNew initialString interval cmd = do
  l <- labelNew (Nothing :: Maybe String)
  labelSetMarkup l initialString

  _ <- on l realize $ do
    _ <- forkIO $ forever $ do
      estr <- E.tryAny cmd
      case estr of
        Left _ -> return ()
        Right str -> postGUIAsync $ labelSetMarkup l str
      threadDelay $ floor (interval * 1000000)
    return ()

  return (toWidget l)

