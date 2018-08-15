{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeInType #-}

-- | Example uses and instances of the @HasState@ capability.
module State where

import Control.Monad.State.Strict (State, StateT (..), runState)
import GHC.Generics (Generic)
import Test.Hspec

import HasState


----------------------------------------------------------------------
-- Example Programs

twoStates :: (HasState "foo" Int m, HasState "bar" Int m) => m ()
twoStates = do
  modify @"foo" (+1)
  modify @"bar" (subtract 1)


----------------------------------------------------------------------
-- Instances

data TwoStates = TwoStates
  { tsFoo :: Int
  , tsBar :: Int
  } deriving (Eq, Generic, Show)

-- | Deriving two @HasState@ instances from the record fields of one
-- @MonadState@.
newtype TwoStatesM a = TwoStatesM (State TwoStates a)
  deriving (Functor, Applicative, Monad)
  deriving (HasState "foo" Int) via
    Field "tsFoo" (MonadState (State TwoStates))
  deriving (HasState "bar" Int) via
    Field "tsBar" (MonadState (State TwoStates))

runTwoStatesM :: TwoStatesM a -> (a, TwoStates)
runTwoStatesM (TwoStatesM m) = runState m TwoStates
  { tsFoo = 0
  , tsBar = 0
  }


-- | Deriving two @HasState@ instances from the components of a tuple in
-- @MonadState@.
newtype PairStateM a = PairStateM (State (Int, Int) a)
  deriving (Functor, Applicative, Monad)
  deriving (HasState "foo" Int) via
    Pos 1 (MonadState (State (Int, Int)))
  deriving (HasState "bar" Int) via
    Pos 2 (MonadState (State (Int, Int)))

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


----------------------------------------------------------------------
-- Test Cases

spec :: Spec
spec = do
  describe "TwoStatesM" $
    it "evaluates twoStates" $
      runTwoStatesM twoStates `shouldBe` ((), TwoStates 1 (-1))
  describe "PairStateM" $
    it "evaluates twoStates" $
      runPairStateM twoStates `shouldBe` ((), (1, (-1)))
  describe "NestedStatesM" $
    it "evaluates twoStates" $
      runNestedStatesM twoStates `shouldBe` (((), 1), (-1))
