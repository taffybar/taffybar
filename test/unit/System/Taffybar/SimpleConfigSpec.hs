{-# LANGUAGE OverloadedRecordDot #-}

module System.Taffybar.SimpleConfigSpec (spec) where

import Data.Maybe (maybeToList)

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Monadic

import System.Taffybar.ContextSpec (runTaffyDefault)
import System.Taffybar.Test.XvfbSpec (RRSetup(..), RROutput(..), RROutputSettings(..), withXdummy, setDefaultDisplay_, withRandrSetup)

import System.Taffybar.Information.X11DesktopInfo (DisplayName(..))
import System.Taffybar.SimpleConfig

spec :: Spec
spec = aroundAll_ (withXdummy . flip setDefaultDisplay_) $ do
  -- Pending: Can't run properties without cleaning up buildContext
  xprop "useAllMonitors" prop_useAllMonitors
  xprop "usePrimaryMonitor" prop_usePrimaryMonitor

prop_useAllMonitors :: RRSetup -> Property
prop_useAllMonitors rr = monadicIO $ do
  allMonitors <- run $ withRandrSetup DefaultDisplay rr $
    runTaffyDefault useAllMonitors

  let rrOutputNumbers = [ i | (i, o) <- zip [0..] rr.outputs
                            , not o.settings.disabled ]

  pure $ allMonitors === rrOutputNumbers

prop_usePrimaryMonitor :: RRSetup -> Property
prop_usePrimaryMonitor rr = monadicIO $ do
  primaryMonitor <- run $ withRandrSetup DefaultDisplay rr $
    runTaffyDefault usePrimaryMonitor

  let rrPrimaryMonitor = fromIntegral <$> maybeToList rr.primary

  pure $ primaryMonitor === rrPrimaryMonitor
