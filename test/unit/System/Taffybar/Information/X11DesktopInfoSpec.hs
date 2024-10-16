{-# LANGUAGE FlexibleInstances #-}

module System.Taffybar.Information.X11DesktopInfoSpec
  ( spec
  , runX11
  , TestEvent
  , TestEvent' (..)
  , TestAtom (..)
  ) where

import Control.Monad (forM, (<=<))
import Control.Monad.Trans.Reader (ReaderT (..))
import Data.Word (Word32)
import GHC.Generics (Generic)
import System.Taffybar.Information.X11DesktopInfo
import System.Taffybar.Test.UtilSpec hiding (spec)
import System.Taffybar.Test.XvfbSpec (XPropName (..), XPropValue (..), withXdummy, xpropSet)
import Test.Hspec hiding (context)
import Test.QuickCheck
import Test.QuickCheck.Monadic
import UnliftIO.Exception (evaluate)
import UnliftIO.STM (atomically, newTChan, readTChan, writeTChan)

spec :: Spec
spec = logSetup $ aroundAll withXdummy $ do
  withX11ContextSpec
  eventLoopSpec

withX11ContextSpec :: SpecWith DisplayName
withX11ContextSpec = describe "withX11Context" $ do
  it "trivial" $ \dn ->
    example $ runX11 dn (pure ()) `shouldReturn` ()

  it "getPrimaryOutputNumber" $ \dn ->
    example $ runX11 dn getPrimaryOutputNumber `shouldReturn` Just 0

  it "read property of root window" $ \dn -> do
    xpropSet dn (XPropName "_XMONAD_VISIBLE_WORKSPACES") (XPropValue "hello")
    ws <- runX11 dn (readAsListOfString Nothing "_XMONAD_VISIBLE_WORKSPACES")
    ws `shouldBe` ["hello"]

  it "send something" $ \dn ->
    runX11 dn $ do
      atom <- getAtom "iamanatom"
      sendCommandEvent atom 42

eventLoopSpec :: SpecWith DisplayName
eventLoopSpec = describe "eventLoop" $ around_ (laxTimeout' 4_000_000) $ do
  it "receives command events" $ property . withMaxSuccess 50 . noShrinking . prop_eventLoop_sendCommandEvents
  it "hammer trivial " $ property . withMaxSuccess 50 . prop_eventLoop_trivial

prop_eventLoop_trivial :: DisplayName -> Property
prop_eventLoop_trivial dn = monadicIO $ run $
  withX11EventLoop dn (const $ pure ()) (pure ())

------------------------------------------------------------------------

type TestEvent = TestEvent' TestAtom

data TestEvent' a = TestEvent
  { testEventArg1 :: !a
  -- | @XClientMessageEvent@ data holds up to five 32-bit values
  , testEventArg2 :: !Word32
  }
  deriving (Show, Read, Eq, Generic)

newtype TestAtom = TestAtom {getTestAtom :: String}
  deriving (Show, Read, Eq, Ord, Generic)

instance Arbitrary TestEvent where
  arbitrary = TestEvent <$> arbitrary <*> arbitrary
  shrink = genericShrink

instance Arbitrary TestAtom where
  arbitrary = TestAtom <$> listOf1 (chooseEnum ('a', 'z'))
  shrink = map TestAtom . filter (not . null) . shrink . getTestAtom

newtype TestEventLoop = TestEventLoop {getBatches :: [[TestEvent]]}
  deriving (Show, Read, Eq, Generic)

instance Arbitrary TestEventLoop where
  arbitrary = fmap TestEventLoop $ gg <$> evs <*> infiniteListOf batchSize
    where
      evs = fmap getNonEmpty arbitrary
      batchSize = sized $ \n -> choose (1, ceiling (sqrt (fromIntegral n) :: Double))

      gg :: [a] -> [Int] -> [[a]]
      gg [] _ = []
      gg _ [] = []
      gg xs (n : ns) = take n xs : gg (drop n xs) ns

  shrink = shrinkMap (TestEventLoop . map getNonEmpty . getNonEmpty) (NonEmpty . map NonEmpty . getBatches)

prop_eventLoop_sendCommandEvents :: DisplayName -> TestEventLoop -> Property
prop_eventLoop_sendCommandEvents dn (TestEventLoop batches) = monadicIO $ fmap conjoin $
  run $ runX11 dn $ do
    chan <- atomically newTChan
    let handler = atomically . writeTChan chan <=< evaluate . getClientMessageEvent
        handler' e = specLog "Handling an event now" >> handler e
    withX11EventLoop dn handler' $
      forM batches $ \msgs -> do
        sent <- forM msgs $ \(TestEvent (TestAtom s) param) -> do
          atom <- getAtom s
          specLog "Sending command event"
          sendCommandEvent atom (fromIntegral param)
          pure $ TestEvent atom param

        fmap conjoin $ forM sent $ \cmd -> do
          specLog "Reading channel"
          recv <- atomically $ readTChan chan
          specLog "Got something from chan"
          pure $ recv === Right cmd

getClientMessageEvent :: Event -> Either Word32 (TestEvent' Atom)
getClientMessageEvent ClientMessageEvent {..} = case ev_data of
  arg1 : _ -> Right (TestEvent ev_message_type (fromIntegral arg1))
  [] -> Left ev_event_type
getClientMessageEvent e = Left (ev_event_type e)

------------------------------------------------------------------------

runX11 :: DisplayName -> ReaderT X11Context IO a -> IO a
runX11 dn f = withX11Context dn (runReaderT f)
