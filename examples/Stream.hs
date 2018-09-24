{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeInType #-}

-- | Example uses and instances of the @HasStream@ capability.
module Stream where

import Capability.HasState
import Capability.HasStream
import Control.Monad.State.Strict (State, StateT (..), evalStateT, runState)
import qualified Data.Set as Set
import Streaming (Stream, Of)
import qualified Streaming.Prelude as S
import Test.Common
import Test.Hspec

----------------------------------------------------------------------
-- Example Programs

iota :: HasStream "nums" Int m => Int -> m ()
iota n
  | n < 0 = error "negative number passed to iota."
  | otherwise = go 0
  where
    go i
      | i == n = pure ()
      | otherwise = yield @"nums" i >> go (succ i)

labelledNodes
  :: (HasState "counter" Int m, HasStream "out" (Int, a) m, Foldable t)
  => t a -> m ()
labelledNodes = mapM_ $ \a -> do
  n <- state @"counter" $ \n -> (n, succ n)
  yield @"out" (n, a)


----------------------------------------------------------------------
-- Instances

-- | @HasStream a@ derived from @HasState [a]@. Will produce reversed list.
newtype StreamAccM a = StreamAccM (State [Int] a)
  deriving (Functor, Applicative, Monad)
  deriving (HasStream "nums" Int) via
    StreamStack (MonadState (State [Int]))

runStreamAccM :: StreamAccM a -> (a, [Int])
runStreamAccM (StreamAccM m) = runState m []


-- | @'Streaming.Stream' ('Streaming.Of' a)@ has a @HasStream a@ instance.
printStreamOfInt :: Stream (Of Int) IO () -> IO ()
printStreamOfInt = S.stdoutLn . S.map show


-- | Composed @StateT@ and @Stream@ to provide @HasState@ and @HasStream@.
newtype StateOverStream a =
  StateOverStream (StateT Int (Stream (Of (Int, Char)) IO) a)
  deriving (Functor, Applicative, Monad)
  deriving (HasState "counter" Int) via
    MonadState (StateT Int (Stream (Of (Int, Char)) IO))
  deriving (HasStream "out" (Int, Char)) via
    Lift (StateT Int (Stream (Of (Int, Char)) IO))

printStateOverStream :: StateOverStream () -> IO ()
printStateOverStream (StateOverStream m) = do
  S.stdoutLn . S.map show $ evalStateT m 0

printLabelledNodes :: IO ()
printLabelledNodes =
  printStateOverStream $ labelledNodes $
    Set.fromList "Hello world!"


----------------------------------------------------------------------
-- Test Cases

spec :: Spec
spec = do
  describe "StreamAccM" $
    it "evaluates iota" $
      runStreamAccM (iota 10) `shouldBe` ((), [9, 8 .. 0])
  describe "Stream (Of Int)" $
    it "evaluates iota" $
      printStreamOfInt (iota 3) `shouldPrint` "0\n1\n2\n"
  describe "StateOverStream" $ do
    it "evaluates labelledNodes" $
      printLabelledNodes `shouldPrint`
        "(0,' ')\n(1,'!')\n(2,'H')\n(3,'d')\n(4,'e')\
        \\n(5,'l')\n(6,'o')\n(7,'r')\n(8,'w')\n"
