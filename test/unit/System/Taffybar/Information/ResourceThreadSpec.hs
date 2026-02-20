{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-unused-binds -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Unused LANGUAGE pragma" #-}

module System.Taffybar.Information.ResourceThreadSpec
  ( spec,
  )
where

import Control.Exception (AsyncException (..), ErrorCall (..))
import Control.Monad
import Control.Monad.STM (throwSTM)
import Data.Bifunctor (first)
import Data.Coerce (coerce)
import Data.Default
import Data.Foldable (toList)
import Data.IORef
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Unique (Unique, hashUnique, newUnique)
import GHC.Generics (Generic)
import GHC.Stack (emptyCallStack)
import System.IO.Unsafe (unsafePerformIO)
import System.Taffybar.Information.ResourceThread
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.StateMachine
import Test.StateMachine.Lockstep.Simple
import Test.StateMachine.TreeDiff (ToExpr (..), defaultExprViaShow)
import Type.Reflection
import UnliftIO.Async
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception (Exception (..), SomeException (..), StringException (..), bracket, throwIO, throwString, try, tryAny)
import UnliftIO.STM (atomically, newEmptyTMVarIO, putTMVar, readTMVar)

class
  ( Show (Effects x),
    Show (Handle x),
    Arbitrary (Effects x),
    Typeable x
  ) =>
  ModelResource x
  where
  data Handle x :: Type
  data Effects x :: Type

  -- pure
  applyEffects :: Effects x -> x -> x

  -- impure
  mkHandle :: x -> IO (Handle x)
  doEffects :: Effects x -> Handle x -> IO ()

-- | Impure helper for test resource
allocModelResource :: (ModelResource x) => x -> (Handle x -> IO a) -> IO a
allocModelResource x0 f = bracket (openX x0) closeX (f . fst)
  where
    openX x = (,) <$> mkHandle x <*> newIORef False
    closeX (_, ref) = do
      c <- atomicModifyIORef' ref (True,)
      when c $ throwString "double-close of Handle"

mkTestReqAction :: (ModelResource x) => Effects x -> Either AnException a -> Handle x -> IO a
mkTestReqAction es req x = do
  doEffects es x
  case req of
    Right r -> pure r
    Left (AnException exc) -> throwIO exc

------------------------------------------------------------------------

-- Model state with a list of ints.
newtype XS = XState {xstate :: [Int]}
  deriving stock (Show, Read, Eq, Generic)
  deriving newtype (Semigroup, Monoid)

instance ModelResource XS where
  -- Model effects of an action with a list of ints.
  data Effects XS = XEffects [Int]
    deriving stock (Show, Generic)

  -- Model resource handle with IORef to state
  newtype Handle XS = XHandle (IORef XS)
    deriving stock (Generic)
    deriving (Show) via (ShowIORef XS)

  -- apply effects using list concatenation to state
  applyEffects (XEffects es) (XState s) = XState (s <> es)

  mkHandle = fmap XHandle . newIORef
  doEffects es (XHandle ref) = atomicModifyIORef' ref $
    \xs -> (applyEffects es xs, ())

instance Arbitrary XS where
  arbitrary = XState <$> arbitrary
  shrink = genericShrink

------------------------------------------------------------------------
-- Test req

data TestReq es = TestReq
  { -- | Pre-determined effects.
    testReqEffects :: es,
    -- | Pre-determined result.
    testReqResult :: SomeTestResult
  }
  deriving (Show, Generic)

-- TODO: test nested SafeReq

------------------------------------------------------------------------
-- Test result

-- | Extract pre-determined result from 'TestReq'.
runTestReq :: (ModelResource x) => TestReq (Effects x) -> SomeTestResult
runTestReq = testReqResult

-- | Apply pre-determined effects from 'TestReq'.
applyTestReqEffects :: (ModelResource x) => TestReq (Effects x) -> x -> x
applyTestReqEffects = applyEffects . testReqEffects

-- | Existential wrapper for result of 'TestReq'.
data SomeTestResult = forall a. (Show a, Eq a, Typeable a, Arbitrary a) => SomeTestResult
  {getSomeResult :: Either AnException a}

deriving instance Show SomeTestResult

instance Eq SomeTestResult where
  SomeTestResult (Left a) == SomeTestResult (Left b) = a == b
  SomeTestResult (Right a) == SomeTestResult (Right b) = case eqTypeRep (typeOf a) (typeOf b) of
    Just HRefl -> a == b
    Nothing -> False
  _ == _ = False

------------------------------------------------------------------------
-- Test result exception

-- | An existential wrapper for 'Arbitrary' exceptions. Like
-- 'SomeException' except this type isn't itself an exception.
data AnException = forall e. (Exception e) => AnException {unGenException :: e}

-- | 'AnException' is unwrapped and thrown in 'runReal'.  It will be
-- caught as 'SomeException', which needs to be coerced back into
-- 'AnException' so it can be compared with the results of 'runMock'.
rewrap :: SomeException -> AnException
rewrap (SomeException exc) = AnException exc

-- instance Show AnException where
--   showsPrec p (AnException exc) = showParen (p > app_prec) $
--     showString "AnException " .
--     showsPrec (app_prec+1) exc
--     where app_prec = 10
deriving instance Show AnException

deriving instance Typeable AnException

instance Eq AnException where
  AnException a == AnException b = case eqTypeRep (typeOf a) (typeOf b) of
    Just HRefl -> show a == show b
    Nothing -> False

instance Default AnException where
  def = AnException aStringException

aStringException :: StringException
aStringException = StringException "AnException occurred" emptyCallStack

instance Arbitrary AnException where
  arbitrary =
    oneof
      [ pure (AnException aStringException),
        -- , AnException . stringException . getNonEmpty <$> arbitrary
        pure (AnException (ErrorCall "this is an error"))
        -- , pure (AnException StackOverflow)
      ]
  shrink exc =
    [ AnException aStringException
    | Just se <- [fromAnException exc],
      se /= aStringException
    ]

------------------------------------------------------------------------
-- Generator for test reqs

instance Arbitrary (Effects XS) where
  arbitrary = XEffects <$> scale (`div` 2) arbitrary
  shrink = genericShrink

instance (Arbitrary es) => Arbitrary (TestReq es) where
  arbitrary = TestReq <$> arbitrary <*> arbitrary
  shrink = genericShrink

instance Arbitrary SomeTestResult where
  arbitrary =
    oneof
      [ SomeTestResult <$> (genResult :: Gen (Either AnException ())),
        SomeTestResult <$> (genResult :: Gen (Either AnException Int)),
        SomeTestResult <$> (genResult :: Gen (Either AnException Char))
      ]
    where
      genResult :: (Arbitrary a) => Gen (Either AnException a)
      genResult =
        frequency
          [ (3, Right <$> arbitrary),
            (1, Left <$> arbitrary)
          ]
  shrink (SomeTestResult r) = shrinkType ++ shrinkReq
    where
      shrinkReq = SomeTestResult <$> shrink r
      -- Try voiding result type if it isn't () already.
      shrinkType = case typeOf r of
        App _ aRep -> case eqTypeRep aRep (typeRep @()) of
          Just HRefl -> []
          Nothing -> [SomeTestResult (void r)]

{-------------------------------------------------------------------------------
  Instantiate the simple API
-------------------------------------------------------------------------------}

data T a

data instance Cmd (T a) h
  = New
  | RunSafeReq (TestReq (Effects a)) h
  | Close h
  deriving stock (Functor, Foldable, Traversable)

deriving instance (Show h, Show (Effects a)) => Show (Cmd (T a) h)

data instance Resp (T a) h
  = Var h
  | RunResult SomeTestResult
  | Unit ()
  | ErrClosed
  deriving stock (Show, Eq, Functor, Foldable, Traversable)

newtype instance MockHandle (T a) = MV Int
  deriving stock (Show, Eq, Ord, Generic)

newtype instance RealHandle (T a) = RealVar
  {getRealVar :: Opaque (UnBracket (ResourceThread (Handle a)))}
  deriving stock (Generic, Show, Eq)

instance ToExpr XS

instance ToExpr (Handle XS) where
  toExpr = defaultExprViaShow -- IORef has no Generic instance.

instance ToExpr SomeTestResult where
  toExpr = defaultExprViaShow -- Existentials can't have Generic instances.

type instance MockState (T a) = Map (MockHandle (T a)) a

instance ToExpr (MockHandle (T a))

instance ToExpr (RealHandle (T a))

type instance Tag (T _) = TagCmd

{-------------------------------------------------------------------------------
  Interpreters
-------------------------------------------------------------------------------}

runMock ::
  (ModelResource a) =>
  a ->
  Cmd (T a) (MockHandle (T a)) ->
  MockState (T a) ->
  (Resp (T a) (MockHandle (T a)), MockState (T a))
runMock e cmd m = case cmd of
  New -> let v = MV (Map.size m) in (Var v, Map.insert v e m)
  Close h -> (Unit (), Map.delete h m)
  RunSafeReq req h -> case Map.lookup h m of
    Just _ ->
      ( RunResult (runTestReq req),
        Map.adjust (applyTestReqEffects req) h m
      )
    Nothing -> (ErrClosed, m)

runReal ::
  (ModelResource a) =>
  a ->
  Cmd (T a) (RealHandle (T a)) ->
  IO (Resp (T a) (RealHandle (T a)))
runReal e cmd = case cmd of
  New -> Var <$> openRealHandle e
  Close h -> Unit <$> closeRealHandle (coerce h)
  RunSafeReq req h ->
    -- either errClosed RunResult
    errClosed2 <$> try (runRealSafeReq h req)

errClosed :: ResourceUnavailableException -> Resp (T a) (RealHandle (T a))
errClosed _ = ErrClosed

errClosed2 :: Either ResourceUnavailableException SomeTestResult -> Resp (T a) (RealHandle (T a))
errClosed2 = \case
  Left _ -> ErrClosed
  Right r@(SomeTestResult (Left exc)) ->
    case fromAnException exc of
      Just (_ :: ResourceUnavailableException) -> ErrClosed
      Nothing -> case fromAnException exc of
        Just (_ :: AsyncCancelled) -> ErrClosed
        Nothing -> RunResult r
  Right r -> RunResult r

fromAnException :: forall e. (Exception e) => AnException -> Maybe e
fromAnException (AnException exc) = case eqTypeRep (typeOf exc) (typeRep @e) of
  Just HRefl -> Just exc
  Nothing -> Nothing

cleanup :: forall a. a -> Model (T a) Concrete -> IO ()
cleanup _ (Model _ hs) = mapM_ (dealloc . getResource) hs
  where
    getResource = unOpaque . getRealVar . concrete . fst

runRealSafeReq ::
  (ModelResource a) =>
  RealHandle (T a) ->
  TestReq (Effects a) ->
  IO SomeTestResult
runRealSafeReq (RealVar (Opaque ub)) (TestReq es (SomeTestResult req)) =
  use ub >>= handleResult . resourceRunSync safeReq
  where
    handleResult = fmap (SomeTestResult . first rewrap) . tryAny
    safeReq = mkSafe (mkTestReqAction es req)

openRealHandle :: (ModelResource a) => a -> IO (RealHandle (T a))
openRealHandle e = coerce <$> newResourceThread (allocModelResource e)

closeRealHandle :: RealHandle (T a) -> IO ()
closeRealHandle (RealVar (Opaque ub)) = dealloc ub

newResourceThread :: (forall c. (Handle x -> IO c) -> IO c) -> IO (UnBracket (ResourceThread (Handle x)))
newResourceThread use = unBracket (withResourceThread use)

deleteResourceThread :: (IO (), ResourceThread x) -> IO ()
deleteResourceThread (d, _) = d

------------------------------------------------------------------------

unBracket :: (forall c. (r -> IO c) -> IO c) -> IO (UnBracket r)
unBracket withResource = do
  var <- newEmptyTMVarIO
  let action = atomically . putTMVar var
      hold = forever (threadDelay maxBound)
      use a = pollSTM a >>= maybe (readTMVar var) (const $ throwSTM ResourceUnavailable)

  a <- async (withResource (\r -> action r >> hold))

  UnBracket (cancel a) (atomically (use a)) <$> newUnique

data UnBracket r = UnBracket
  { dealloc :: IO (),
    use :: IO r,
    ubid :: Unique
  }
  deriving (Generic)

instance Show (UnBracket r) where
  show u = "UnBracket{ubid=" ++ show (hashUnique (ubid u)) ++ "}"

instance Eq (UnBracket r) where
  a == b = ubid a == ubid b

{-------------------------------------------------------------------------------
  Generator
-------------------------------------------------------------------------------}

generator ::
  forall a.
  (ModelResource a) =>
  Model (T a) Symbolic ->
  Maybe (Gen (Cmd (T a) :@ Symbolic))
generator (Model _ hs) = Just gens
  where
    gens = if null hs then withoutHandle else withHandle

    withoutHandle :: Gen (Cmd (T a) :@ Symbolic)
    withoutHandle = return $ At New

    withHandle :: Gen (Cmd (T a) :@ Symbolic)
    withHandle =
      frequency
        [ (15, fmap At $ RunSafeReq <$> arbitrary <*> genHandle),
          (1, fmap At $ Close <$> genHandle)
        ]

    genHandle :: Gen (Reference (RealHandle (T a)) Symbolic)
    genHandle = elements (map fst hs)

shrinker :: (ModelResource a) => Model (T a) Symbolic -> Cmd (T a) :@ Symbolic -> [Cmd (T a) :@ Symbolic]
shrinker _ (At cmd) = case cmd of
  New -> []
  Close _ -> []
  RunSafeReq r h -> At . flip RunSafeReq h <$> shrink r

{-------------------------------------------------------------------------------
  Tagging
-------------------------------------------------------------------------------}

data TagCmd = TagNew | TagRun TagRunSafeReq | TagClose
  deriving stock (Show)

tagCmds :: [Event (T a) Symbolic] -> [TagCmd]
tagCmds = map (aux . unAt . cmd)
  where
    aux :: Cmd (T a) h -> TagCmd
    aux New = TagNew
    aux (RunSafeReq r _) = TagRun (tagRun r)
    aux (Close _) = TagClose

data TagRunSafeReq = TagRunSafeReq
  deriving stock (Show)

tagRun :: TestReq a -> TagRunSafeReq
tagRun _req = TagRunSafeReq

{-------------------------------------------------------------------------------
  Wrapping it all up
-------------------------------------------------------------------------------}

ioRefTest :: StateMachineTest (T XS)
ioRefTest =
  StateMachineTest
    { initMock = mempty,
      generator = System.Taffybar.Information.ResourceThreadSpec.generator,
      shrinker = System.Taffybar.Information.ResourceThreadSpec.shrinker,
      newHandles = toList,
      runMock = System.Taffybar.Information.ResourceThreadSpec.runMock mempty,
      runReal = System.Taffybar.Information.ResourceThreadSpec.runReal mempty,
      cleanup = System.Taffybar.Information.ResourceThreadSpec.cleanup mempty,
      tag = tagCmds
    }

{-------------------------------------------------------------------------------
  Spec
-------------------------------------------------------------------------------}

prop_resourceThread_sequential :: Property
prop_resourceThread_sequential = prop_sequential ioRefTest Nothing

prop_resourceThread_parallel :: Property
prop_resourceThread_parallel = prop_parallel ioRefTest Nothing

spec :: Spec
spec = describe "ResourceThread state machine" $ do
  prop "sequential" prop_resourceThread_sequential
  prop "parallel" prop_resourceThread_parallel

-- prop "ResourceThread property" prop_resourceThread

------------------------------------------------------------------------
-- misc

-- | Type wrapper to make a naughty showable IORef.
newtype ShowIORef a = ShowIORef (IORef a)

instance (Show a) => Show (ShowIORef a) where
  show (ShowIORef ref) = "IORef<<" ++ show (unsafePerformIO $ readIORef ref) ++ ">>"

-- | Wrapper with ability to show the type of an IO action.
newtype ShowIOFunc x r = ShowIOFunc (x -> IO r)

instance forall x r. (Typeable x, Typeable r) => Show (ShowIOFunc x r) where
  show _f = "_ :: " ++ show (TypeRep @x) ++ " -> IO " ++ show (TypeRep @r)
