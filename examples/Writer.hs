{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeInType #-}

-- | Example uses and instances of the @HasWriter@ capability.
module Writer where

import Capability.HasState
import Capability.HasWriter
import Control.Monad.State.Strict (State, StateT (..), runState)
import Data.Monoid (Sum (..))
import Test.Hspec

----------------------------------------------------------------------
-- Example Programs

-- | Increase a counter using a writer monad.
useWriter :: HasWriter "count" (Sum Int) m => m ()
useWriter = do
  tell @"count" 1
  tell @"count" 2
  tell @"count" 3


-- | Mix writer and state monad operations on the same tag.
--
-- Note, mixing capabilities on the same tag like this is inadvisable in
-- real applications.  The @HasState@ capability could be used to clear the
-- accumulated outcome of the @HasWriter@ capability.
mixWriterState
  :: (HasState "count" Int m, HasWriter "count" (Sum Int) m)
  => m Int
mixWriterState = do
  tell @"count" 1
  one <- get @"count"
  tell @"count" $ Sum one
  pure one


----------------------------------------------------------------------
-- Instances

-- | @HasWriter w@ capability derived from @HasState w@.
--
-- The monoid instance ('Data.Monoid.Sum') is also selected in the deriving
-- via clause.
newtype WriterM a = WriterM (State Int a)
  deriving (Functor, Applicative, Monad)
  deriving (HasWriter "count" (Sum Int))
    via WriterLog (Coerce (Sum Int) (MonadState (State Int)))

runWriterM :: WriterM a -> (a, Int)
runWriterM (WriterM m) = runState m 0


-- | @HasWriter w@ capability derived from @HasState w@, while also exposing
-- the underlying @HasState@.
--
-- Note, that this is inadvisable in real applications.
-- See caveat on 'mixWriterState'.
newtype BadWriterM a = BadWriterM (State Int a)
  deriving (Functor, Applicative, Monad)
  deriving (HasWriter "count" (Sum Int))
    via WriterLog (Coerce (Sum Int) (MonadState (State Int)))
  deriving (HasState "count" Int)
    via MonadState (State Int)

runBadWriterM :: BadWriterM a -> (a, Int)
runBadWriterM (BadWriterM m) = runState m 0


----------------------------------------------------------------------
-- Test Cases

spec :: Spec
spec = do
  describe "WriterM" $
    it "evaluates useWriter" $
      runWriterM useWriter `shouldBe` ((), 6)
  describe "BadWriterM" $ do
    it "evaluates useWriter" $
      runBadWriterM useWriter `shouldBe` ((), 6)
    it "evaluates mixWriterState" $
      runBadWriterM mixWriterState `shouldBe` (1, 2)
