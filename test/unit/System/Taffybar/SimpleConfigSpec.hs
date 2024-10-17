{-# LANGUAGE OverloadedRecordDot #-}

module System.Taffybar.SimpleConfigSpec (spec) where

import Data.Maybe (isJust, maybeToList)

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
  -- fixme: this fails because it needs gtk_init
  xprop "useAllMonitors" prop_useAllMonitors
  prop "usePrimaryMonitor" (withMaxSuccess 50 prop_usePrimaryMonitor)

prop_useAllMonitors :: RRSetup -> Property
prop_useAllMonitors rr = monadicIO $ do
  allMonitors <- run $ withRandrSetup DefaultDisplay rr $
    runTaffyDefault useAllMonitors

  let rrOutputNumbers = [ i | (i, o) <- zip [0..] rr.outputs
                            , not o.settings.disabled ]

  pure $ allMonitors === rrOutputNumbers

prop_usePrimaryMonitor :: RRSetup -> Property
prop_usePrimaryMonitor rr = isJust rr.primary ==> monadicIO $ do
  primaryMonitor <- run $ withRandrSetup DefaultDisplay rr $
    runTaffyDefault usePrimaryMonitor

  let rrPrimaryMonitor = fromIntegral <$> maybeToList rr.primary

  pure $ primaryMonitor === rrPrimaryMonitor
