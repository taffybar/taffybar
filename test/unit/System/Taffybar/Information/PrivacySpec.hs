{-# LANGUAGE OverloadedStrings #-}

module System.Taffybar.Information.PrivacySpec (spec) where

import Data.Either (isLeft)
import Data.Maybe (isJust)
import System.Taffybar.Information.Privacy
import Test.Hspec

spec :: Spec
spec = describe "Privacy monitoring availability" $ do
  it "distinguishes a valid empty snapshot from malformed data" $ do
    parsePrivacyInfo defaultPrivacyConfig "[]" `shouldBe` Right (PrivacyInfo [])
    parsePrivacyInfo defaultPrivacyConfig "not JSON" `shouldSatisfy` isLeft

  it "reports an unavailable monitor when pw-dump fails" $ do
    info <- getPrivacyInfo defaultPrivacyConfig {privacyPwDumpPath = "false"}
    privacyInfoError info `shouldSatisfy` isJust

  it "reports an unavailable monitor when the executable is missing" $ do
    info <- getPrivacyInfo defaultPrivacyConfig {privacyPwDumpPath = "/does-not-exist/taffybar-pw-dump"}
    privacyInfoError info `shouldSatisfy` isJust

  it "continues reporting active microphone streams" $ do
    let input = "[{\"id\":1,\"type\":\"PipeWire:Interface:Node\",\"info\":{\"state\":\"running\",\"props\":{\"media.class\":\"Stream/Input/Audio\",\"application.name\":\"Recorder\"}}}]"
    fmap (map appName . activeNodes) (parsePrivacyInfo defaultPrivacyConfig input)
      `shouldBe` Right ["Recorder"]
