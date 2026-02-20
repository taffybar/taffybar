{-# LANGUAGE FlexibleInstances #-}

module System.Taffybar.Information.X11DesktopInfoSpec (spec) where

import System.Taffybar.Information.X11DesktopInfo
import System.Taffybar.Test.XvfbSpec (XPropName (..), XPropValue (..), withXdummy, xpropSet)
import Test.Hspec hiding (context)

spec :: Spec
spec = around withXdummy $ describe "withX11Context" $ do
  it "trivial" $ \dn ->
    example $
      withX11Context dn (pure ()) `shouldReturn` ()

  it "getPrimaryOutputNumber" $ \dn ->
    example $
      withX11Context dn getPrimaryOutputNumber `shouldReturn` Just 0

  it "read property of root window" $ \dn -> do
    xpropSet dn (XPropName "_XMONAD_VISIBLE_WORKSPACES") (XPropValue "hello")
    ws <- withX11Context dn (readAsListOfString Nothing "_XMONAD_VISIBLE_WORKSPACES")
    ws `shouldBe` ["hello"]

  it "send something" $ \dn -> do
    withX11Context dn $ do
      atom <- getAtom "iamanatom"
      sendCommandEvent atom 42
