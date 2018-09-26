{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}

-- | Example uses and instances of the @HasState@ capability.
module State where

import Capability.Reader
import Capability.State
import Control.Monad.Reader (ReaderT (..))
import Control.Monad.State.Strict (State, StateT (..), runState)
import Data.IORef
import GHC.Generics (Generic)
import Test.Hspec

----------------------------------------------------------------------
-- Example Programs

incFoo :: HasState "foo" Int m => m ()
incFoo = modify @"foo" (+1)

twoStates :: (HasState "foo" Int m, HasState "bar" Int m) => m ()
twoStates = do
  incFoo
  modify @"bar" (subtract 1)


useZoom :: HasState "foobar" (Int, Int) m => m Int
useZoom = do
  put @"foobar" (2, 2)
  -- Zoom in on the first element in the current state, rename tag 1 to "foo".
  zoom @"foobar" @"foo" @(Rename 1 :.: Pos 1 "foobar") $
    incFoo
  gets @"foobar" (\(foo, bar) -> foo + bar)


----------------------------------------------------------------------
-- Instances

data TwoStates = TwoStates
  { tsFoo :: IORef Int
  , tsBar :: IORef Int
  } deriving Generic

-- | Deriving @HasState@ from @HasReader@ of an @IORef@.
--
-- In this case two separate state capabilities are derived from the record
-- fields of the @HasReader@ context.
newtype TwoStatesM a = TwoStatesM (ReaderT TwoStates IO a)
  deriving (Functor, Applicative, Monad)
  deriving (HasState "foo" Int) via
    ReaderIORef (Rename "tsFoo" (Field "tsFoo" ()
    (MonadReader (ReaderT TwoStates IO))))
  deriving (HasState "bar" Int) via
    ReaderIORef (Rename "tsBar" (Field "tsBar" ()
    (MonadReader (ReaderT TwoStates IO))))

runTwoStatesM :: TwoStatesM a -> IO (a, (Int, Int))
runTwoStatesM (TwoStatesM m) = do
  fooRef <- newIORef 0
  barRef <- newIORef 0
  result <- runReaderT m TwoStates
    { tsFoo = fooRef
    , tsBar = barRef
    }
  fooVal <- readIORef fooRef
  barVal <- readIORef barRef
  pure (result, (fooVal, barVal))


-- | Deriving two @HasState@ instances from the components of a tuple in
-- @MonadState@.
newtype PairStateM a = PairStateM (State (Int, Int) a)
  deriving (Functor, Applicative, Monad)
  deriving (HasState "foo" Int) via
    Rename 1 (Pos 1 () (MonadState (State (Int, Int))))
  deriving (HasState "bar" Int) via
    Rename 2 (Pos 2 () (MonadState (State (Int, Int))))

runPairStateM :: PairStateM a -> (a, (Int, Int))
runPairStateM (PairStateM m) = runState m (0, 0)


-- | Combining the @HasState@ instances from two nested @StateT@ transformers.
--
-- Note, that this is not the recommended way to provide multiple `HasState`
-- capabilities. Use the approach shown above in 'TwoStatesM' instead. However,
-- this pattern can be useful to transation existing code to this library.
newtype NestedStatesM a = NestedStatesM (StateT Int (State Int) a)
  deriving (Functor, Applicative, Monad)
  deriving (HasState "foo" Int) via MonadState (StateT Int (State Int))
  deriving (HasState "bar" Int) via Lift (StateT Int (MonadState (State Int)))

runNestedStatesM :: NestedStatesM a -> ((a, Int), Int)
runNestedStatesM (NestedStatesM m) = runState (runStateT m 0) 0


runFooBarState
  :: (forall m. HasState "foobar" (Int, Int) m => m a)
  -> (Int, Int) -> (a, (Int, Int))
runFooBarState (MonadState m) = runState m


----------------------------------------------------------------------
-- Test Cases

spec :: Spec
spec = do
  describe "TwoStatesM" $
    it "evaluates twoStates" $
      runTwoStatesM twoStates `shouldReturn` ((), (1, -1))
  describe "PairStateM" $
    it "evaluates twoStates" $
      runPairStateM twoStates `shouldBe` ((), (1, -1))
  describe "NestedStatesM" $
    it "evaluates twoStates" $
      runNestedStatesM twoStates `shouldBe` (((), 1), -1)
  describe "runFooBarState" $
    it "evaluates useZoom" $
      runFooBarState useZoom (0, 0) `shouldBe` (5, (3, 2))
