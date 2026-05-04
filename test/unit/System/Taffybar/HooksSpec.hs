module System.Taffybar.HooksSpec (spec) where

import Control.Exception (bracket)
import Data.Default (def)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.Taffybar.Context (CSSPaths (..), TaffybarConfig (cssPathTransform))
import System.Taffybar.Hooks (CSSPathEnvConfig (..), withCSSPathEnvConfig)
import Test.Hspec

spec :: Spec
spec =
  describe "withCSSPathEnvConfig" $ do
    it "does nothing when configured environment variables are unset" $
      withUnsetEnv testVars $ do
        cssPathTransform (withCSSPathEnvConfig testEnvConfig def) testCSSPaths
          `shouldReturn` testCSSPaths

    it "replaces resolved user CSS paths with the configured path-list variable" $
      withUnsetEnv testVars $
        withEnv cssPathsVar (Just "override.css:debug.css") $ do
          cssPathTransform (withCSSPathEnvConfig testEnvConfig def) testCSSPaths
            `shouldReturn` CSSPaths ["vendor.css"] ["override.css", "debug.css"]

    it "can drop vendored and user CSS paths independently" $
      withUnsetEnv testVars $
        withEnv disableVendorVar (Just "yes") $
          withEnv disableUserVar (Just "1") $ do
            cssPathTransform (withCSSPathEnvConfig testEnvConfig def) testCSSPaths
              `shouldReturn` CSSPaths [] []

testCSSPaths :: CSSPaths
testCSSPaths =
  CSSPaths
    { vendorCSSPaths = ["vendor.css"],
      userCSSPaths = ["user.css"]
    }

testEnvConfig :: CSSPathEnvConfig
testEnvConfig =
  CSSPathEnvConfig
    { cssPathsEnvVar = Just cssPathsVar,
      disableVendorCssEnvVar = Just disableVendorVar,
      disableUserCssEnvVar = Just disableUserVar
    }

testVars :: [String]
testVars = [cssPathsVar, disableVendorVar, disableUserVar]

cssPathsVar :: String
cssPathsVar = "TAFFYBAR_TEST_CSS_PATHS"

disableVendorVar :: String
disableVendorVar = "TAFFYBAR_TEST_DISABLE_VENDOR_CSS"

disableUserVar :: String
disableUserVar = "TAFFYBAR_TEST_DISABLE_USER_CSS"

withUnsetEnv :: [String] -> IO a -> IO a
withUnsetEnv vars action =
  foldr (\var inner -> withEnv var Nothing inner) action vars

withEnv :: String -> Maybe String -> IO a -> IO a
withEnv name value action =
  bracket (lookupEnv name) restoreEnv $ \_ -> do
    maybe (unsetEnv name) (setEnv name) value
    action
  where
    restoreEnv Nothing = unsetEnv name
    restoreEnv (Just originalValue) = setEnv name originalValue
