{-# OPTIONS_GHC -Wno-missing-fields #-}

module System.Taffybar.ContextStateSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar qualified as MV
import Control.Exception (throwIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (runReaderT)
import Data.IORef
import Data.Map.Strict qualified as Map
import System.Taffybar.Context
import System.Timeout (timeout)
import Test.Hspec
import UnliftIO.Async (cancel, mapConcurrently, wait, withAsync)

newtype StateA = StateA Int deriving (Eq, Show)

newtype StateB = StateB Int deriving (Eq, Show)

stateContext :: IO Context
stateContext = do
  state <- MV.newMVar Map.empty
  pure Context {contextState = state}

spec :: Spec
spec = before stateContext $ describe "Context state initialization" $ do
  it "can initialize and write dependencies of other types" $ \ctx -> do
    let initialize = getStateDefault $ do
          StateB n <- getStateDefault (pure $ StateB 4)
          _ <- setState (StateB 5)
          pure $ StateA n
    timeout 1000000 (runReaderT initialize ctx) `shouldReturn` Just (StateA 4)
    runReaderT getState ctx `shouldReturn` Just (StateB 5)

  it "runs concurrent initializers for one type only once" $ \ctx -> do
    calls <- newIORef (0 :: Int)
    let initialize = getStateDefault $ liftIO $ do
          atomicModifyIORef' calls (\n -> (n + 1, ()))
          threadDelay 30000
          pure $ StateA 7
    results <- mapConcurrently (const $ runReaderT initialize ctx) [1 .. 16 :: Int]
    results `shouldBe` replicate 16 (StateA 7)
    readIORef calls `shouldReturn` 1

  it "allows unrelated state access while an initializer is blocked" $ \ctx -> do
    entered <- MV.newEmptyMVar
    release <- MV.newEmptyMVar
    let initialize = getStateDefault $ liftIO $ MV.putMVar entered () >> MV.readMVar release >> pure (StateA 1)
    withAsync (runReaderT initialize ctx) $ \worker -> do
      MV.takeMVar entered
      timeout 1000000 (runReaderT (getStateDefault $ pure $ StateB 2) ctx)
        `shouldReturn` Just (StateB 2)
      MV.putMVar release ()
      wait worker `shouldReturn` StateA 1

  it "retries after initialization throws" $ \ctx -> do
    runReaderT (getStateDefault $ liftIO $ throwIO $ userError "failed" :: TaffyIO StateA) ctx
      `shouldThrow` anyIOException
    runReaderT (getStateDefault $ pure $ StateA 3) ctx `shouldReturn` StateA 3

  it "releases an initialization slot when its owner is cancelled" $ \ctx -> do
    entered <- MV.newEmptyMVar
    release <- MV.newEmptyMVar
    let initialize = getStateDefault $ liftIO $ MV.putMVar entered () >> MV.readMVar release >> pure (StateA 1)
    withAsync (runReaderT initialize ctx) $ \worker -> MV.takeMVar entered >> cancel worker
    timeout 1000000 (runReaderT (getStateDefault $ pure $ StateA 2) ctx)
      `shouldReturn` Just (StateA 2)

  it "does not overwrite an explicit update made during initialization" $ \ctx -> do
    entered <- MV.newEmptyMVar
    release <- MV.newEmptyMVar
    let initialize = getStateDefault $ liftIO $ MV.putMVar entered () >> MV.readMVar release >> pure (StateA 1)
    withAsync (runReaderT initialize ctx) $ \worker -> do
      MV.takeMVar entered
      runReaderT (setState $ StateA 9) ctx `shouldReturn` StateA 9
      MV.putMVar release ()
      wait worker `shouldReturn` StateA 9
    runReaderT getState ctx `shouldReturn` Just (StateA 9)

  it "rejects recursive initialization instead of deadlocking" $ \ctx -> do
    let initialize = getStateDefault $ getStateDefault (pure $ StateA 1)
    timeout 1000000 (runReaderT initialize ctx) `shouldThrow` anyIOException
