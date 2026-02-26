{-# LANGUAGE OverloadedStrings #-}

module UtilSpec (spec) where

import qualified DBus.Internal.Message as M
import DBus.Internal.Types (ErrorName, Serial (..), errorName_)
import StatusNotifier.Util
import Test.Hspec

spec :: Spec
spec = do
  describe "splitServiceName" $ do
    it "splits a bus name with no path" $
      splitServiceName "org.example.Service"
        `shouldBe` ("org.example.Service", Nothing)

    it "splits a bus name with an object path suffix" $
      splitServiceName "org.example.Service/StatusNotifierItem"
        `shouldBe` ("org.example.Service", Just "/StatusNotifierItem")

    it "splits a bus name with a nested object path suffix" $
      splitServiceName "org.example.Service/StatusNotifierItem/Submenu"
        `shouldBe` ("org.example.Service", Just "/StatusNotifierItem/Submenu")

    it "keeps path-only values intact when no bus name is present" $
      splitServiceName "/StatusNotifierItem"
        `shouldBe` ("/StatusNotifierItem", Nothing)

    it "returns an empty bus component for empty input" $
      splitServiceName ""
        `shouldBe` ("", Nothing)

  describe "convertARGBToABGR" $ do
    it "swaps red and blue channels while preserving alpha/green" $
      convertARGBToABGR 0x11223344 `shouldBe` 0x11443322

    it "leaves values unchanged when red and blue channels are equal" $
      convertARGBToABGR 0xAADD33DD `shouldBe` 0xAADD33DD

  describe "maybeToEither" $ do
    it "returns Right for Just values" $
      maybeToEither "err" (Just (3 :: Int)) `shouldBe` Right 3

    it "returns Left default for Nothing" $
      maybeToEither "err" (Nothing :: Maybe Int) `shouldBe` Left ("err" :: String)

  describe "exemptUnknownMethod" $ do
    it "converts unknown-method errors to Right default" $
      exemptUnknownMethod "fallback" (Left $ mkMethodError errorUnknownMethod)
        `shouldBe` Right ("fallback" :: String)

    it "keeps non-unknown errors intact" $
      exemptUnknownMethod "fallback" (Left $ mkMethodError errorFailed)
        `shouldBe` Left (mkMethodError errorFailed)

    it "passes through successful values unchanged" $
      exemptUnknownMethod "fallback" (Right ("ok" :: String))
        `shouldBe` Right "ok"

  describe "exemptAll" $ do
    it "maps all errors to Right default" $
      exemptAll "fallback" (Left $ mkMethodError $ errorName_ "org.example.Error")
        `shouldBe` Right ("fallback" :: String)

    it "passes through successful values unchanged" $
      exemptAll "fallback" (Right ("ok" :: String))
        `shouldBe` Right "ok"

mkMethodError :: ErrorName -> M.MethodError
mkMethodError errName =
  M.MethodError
    { M.methodErrorName = errName
    , M.methodErrorSerial = Serial 0
    , M.methodErrorSender = Nothing
    , M.methodErrorDestination = Nothing
    , M.methodErrorBody = []
    }

errorUnknownMethod :: ErrorName
errorUnknownMethod = errorName_ "org.freedesktop.DBus.Error.UnknownMethod"

errorFailed :: ErrorName
errorFailed = errorName_ "org.freedesktop.DBus.Error.Failed"
