module System.Taffybar.Information.SafeX11Spec (spec) where

import Control.Concurrent (threadDelay)
import Control.Exception (ErrorCall (..), throwIO)
import System.Taffybar.Information.SafeX11 (postX11RequestSyncDef)
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = sequential $ describe "Safe X11 request isolation" $ do
  it "answers a failed request and continues serving healthy requests" $ do
    timeout 2000000 (postX11RequestSyncDef (0 :: Int) $ throwIO $ ErrorCall "failed getter")
      `shouldReturn` Just 0
    timeout 2000000 (postX11RequestSyncDef (0 :: Int) $ pure 42)
      `shouldReturn` Just 42

  it "forces a result inside the request exception boundary" $ do
    timeout 2000000 (postX11RequestSyncDef (0 :: Int) $ pure $ error "invalid result")
      `shouldReturn` Just 0

  it "continues serving requests after a request times out" $ do
    timeout 2000000 (postX11RequestSyncDef (0 :: Int) $ threadDelay 1000000 >> pure 1)
      `shouldReturn` Just 0
    timeout 2000000 (postX11RequestSyncDef (0 :: Int) $ pure 2)
      `shouldReturn` Just 2
