{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeInType #-}

-- | Example uses and instances of the @HasWriter@ capability.
module Writer where

import Capability.State
import Capability.Sink
import Capability.Source
import Capability.Writer
import Control.Monad.State.Strict (State, StateT (..), runState)
import Data.Monoid (Sum (..))
import Test.Hspec

----------------------------------------------------------------------
-- Example Programs

-- | Increase a counter using a writer monad.
useWriter :: HasWriter "count-writer" (Sum Int) m => m ()
useWriter = censor @"count-writer" (*2) {- double the eventual result -} $ do
  -- Add 3 and retrieve result
  ((), count) <- listen @"count-writer" (tell @"count-writer" 3)
  -- Duplicate
  tell @"count-writer" count


-- | Mix writer and state monad operations on the same tag.
--
-- Note, mixing capabilities on the same tag like this is inadvisable in
-- real applications.  The @HasState@ capability could be used to clear the
-- accumulated outcome of the @HasWriter@ capability.
mixWriterState
  :: (HasState "count-state" Int m, HasWriter "count-writer" (Sum Int) m)
  => m Int
mixWriterState = do
  tell @"count-writer" 1
  one <- get @"count-state"
  tell @"count-writer" $ Sum one
  pure one


----------------------------------------------------------------------
-- Instances

-- | @HasWriter w@ capability derived from @HasState w@.
--
-- The monoid instance ('Data.Monoid.Sum') is also selected in the deriving
-- via clause.
newtype WriterM a = WriterM (State Int a)
  deriving (Functor, Applicative, Monad)
  deriving (HasSink "count-writer" (Sum Int), HasWriter "count-writer" (Sum Int))
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
  deriving (HasSink "count-writer" (Sum Int), HasWriter "count-writer" (Sum Int))
    via WriterLog (Coerce (Sum Int) (MonadState (State Int)))
  deriving (HasSource "count-state" Int, HasSink "count-state" Int, HasState "count-state" Int)
    via MonadState (State Int)

runBadWriterM :: BadWriterM a -> (a, Int)
runBadWriterM (BadWriterM m) = runState m 0


----------------------------------------------------------------------
-- Test Cases

spec :: Spec
spec = do
  describe "WriterM" $
    it "evaluates useWriter" $
      runWriterM useWriter `shouldBe` ((), 12)
  describe "BadWriterM" $ do
    it "evaluates useWriter" $
      runBadWriterM useWriter `shouldBe` ((), 12)
    it "evaluates mixWriterState" $
      runBadWriterM mixWriterState `shouldBe` (1, 2)
