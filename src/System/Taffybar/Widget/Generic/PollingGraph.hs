-- | A variant of the Graph widget that automatically updates itself
-- with a callback at a fixed interval.
module System.Taffybar.Widget.Generic.PollingGraph (
  -- * Types
  GraphHandle,
  GraphConfig(..),
  GraphDirection(..),
  GraphStyle(..),
  -- * Constructors and accessors
  pollingGraphNew,
  defaultGraphConfig
  ) where

import           Control.Concurrent
import qualified Control.Exception.Enclosed as E
import           Control.Monad ( forever )
import           Control.Monad.Trans
import           Graphics.UI.Gtk

import           System.Taffybar.Widget.Generic.Graph

pollingGraphNew
  :: MonadIO m
  => GraphConfig -> Double -> IO [Double] -> m Widget
pollingGraphNew cfg pollSeconds action = liftIO $ do
  (da, h) <- graphNew cfg

  _ <- on da realize $ do
       _ <- forkIO $ forever $ do
         esample <- E.tryAny action
         case esample of
           Left _ -> return ()
           Right sample -> graphAddSample h sample
         threadDelay $ floor (pollSeconds * 1000000)
       return ()

  return da
