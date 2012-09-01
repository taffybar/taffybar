-- | A variant of the Graph widget that automatically updates itself
-- with a callback at a fixed interval.
module System.Taffybar.Widgets.PollingGraph (
  -- * Types
  GraphHandle,
  GraphConfig(..),
  GraphDirection(..),
  -- * Constructors and accessors
  pollingGraphNew,
  defaultGraphConfig
  ) where

import Control.Concurrent
import Control.Monad ( forever )
import Graphics.UI.Gtk

import System.Taffybar.Widgets.Graph

pollingGraphNew :: GraphConfig
                   -> Double
                   -> IO [Double]
                   -> IO Widget
pollingGraphNew cfg pollSeconds action = do
  (da, h) <- graphNew cfg

  _ <- on da realize $ do
       _ <- forkIO $ forever $ do
         sample <- action
         graphAddSample h sample
         threadDelay $ floor (pollSeconds * 1000000)
       return ()

  return da