{-# LANGUAGE OverloadedRecordDot #-}

module System.Taffybar.SimpleConfigSpec (spec) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (runReaderT)
import Data.Default (def)
import Data.IORef
import Data.Maybe (fromMaybe, maybeToList)
import Data.Text qualified as T
import Graphics.UI.GIGtkStrut (strutYPadding)
import System.Taffybar.Context qualified as BC
import System.Taffybar.ContextSpec (runTaffyDefault)
import System.Taffybar.Information.X11DesktopInfo (DisplayName (..))
import System.Taffybar.SimpleConfig
import System.Taffybar.Test.DBusSpec (withTestDBus)
import System.Taffybar.Test.XvfbSpec (RROutput (..), RROutputSettings (..), RRSetup (..), setDefaultDisplay_, withRandrSetup, withXdummy)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Monadic

spec :: Spec
spec = do
  aroundAll_ withTestDBus $ describe "toTaffybarConfigPerMonitor" $ do
    it "builds distinct monitor geometry and CSS classes" $ do
      let base = defaultSimpleTaffyConfig {monitorsAction = pure [0, 1]}
          config =
            toTaffybarConfigPerMonitor base $ \monitorNumber ->
              pure $
                SimpleMonitorConfig
                  (T.pack $ show monitorNumber)
                  base
                    { barPadding = monitorNumber,
                      barCssClasses = [T.pack $ "monitor-" <> show monitorNumber]
                    }
      barConfigs <- runTaffyWayland $ BC.getBarConfigsParam config
      map BC.barCssClasses barConfigs `shouldBe` [["monitor-0"], ["monitor-1"]]
      map (strutYPadding . BC.strutConfig) barConfigs `shouldBe` [0, 1]

    it "reuses bars while monitor configuration keys are unchanged" $ do
      let base = defaultSimpleTaffyConfig {monitorsAction = pure [0, 1]}
          config =
            toTaffybarConfigPerMonitor base $ \monitorNumber ->
              pure $ SimpleMonitorConfig (T.pack $ show monitorNumber) base
      (first, second) <- runTaffyWayland $ do
        first <- BC.getBarConfigsParam config
        second <- BC.getBarConfigsParam config
        pure (first, second)
      map BC.barId second == map BC.barId first `shouldBe` True

    it "rebuilds only the monitor whose configuration key changed" $ do
      keysRef <- newIORef [(0, "zero"), (1, "one")]
      let base = defaultSimpleTaffyConfig {monitorsAction = pure [0, 1]}
          monitorConfig monitorNumber = do
            keys <- liftIO $ readIORef keysRef
            pure $ SimpleMonitorConfig (fromMaybe "" $ lookup monitorNumber keys) base
          config = toTaffybarConfigPerMonitor base monitorConfig
      (first, second) <- runTaffyWayland $ do
        first <- BC.getBarConfigsParam config
        liftIO $ writeIORef keysRef [(0, "changed"), (1, "one")]
        second <- BC.getBarConfigsParam config
        pure (first, second)
      zipWith (/=) (map BC.barId second) (map BC.barId first) `shouldBe` [True, False]

    it "purges removed monitors from the bar cache" $ do
      monitorsRef <- newIORef [0, 1]
      let base = defaultSimpleTaffyConfig {monitorsAction = liftIO $ readIORef monitorsRef}
          config =
            toTaffybarConfigPerMonitor base $ \monitorNumber ->
              pure $ SimpleMonitorConfig (T.pack $ show monitorNumber) base
      (first, third) <- runTaffyWayland $ do
        first <- BC.getBarConfigsParam config
        liftIO $ writeIORef monitorsRef [0]
        _ <- BC.getBarConfigsParam config
        liftIO $ writeIORef monitorsRef [0, 1]
        third <- BC.getBarConfigsParam config
        pure (first, third)
      zipWith (/=) (map BC.barId third) (map BC.barId first) `shouldBe` [False, True]

  aroundAll_ (withXdummy . flip setDefaultDisplay_) $ do
    -- Pending: Can't run properties without cleaning up buildContext
    xprop "useAllMonitors" prop_useAllMonitors
    xprop "usePrimaryMonitor" prop_usePrimaryMonitor

runTaffyWayland :: BC.TaffyIO a -> IO a
runTaffyWayland action =
  BC.buildContextWithBackend BC.BackendWayland def >>= runReaderT action

prop_useAllMonitors :: RRSetup -> Property
prop_useAllMonitors rr = monadicIO $ do
  allMonitors <-
    run $
      withRandrSetup DefaultDisplay rr $
        runTaffyDefault useAllMonitors

  let rrOutputNumbers =
        [ i
        | (i, o) <- zip [0 ..] rr.outputs,
          not o.settings.disabled
        ]

  pure $ allMonitors === rrOutputNumbers

prop_usePrimaryMonitor :: RRSetup -> Property
prop_usePrimaryMonitor rr = monadicIO $ do
  primaryMonitor <-
    run $
      withRandrSetup DefaultDisplay rr $
        runTaffyDefault usePrimaryMonitor

  let rrPrimaryMonitor = fromIntegral <$> maybeToList rr.primary

  pure $ primaryMonitor === rrPrimaryMonitor
