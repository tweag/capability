{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeInType #-}

-- | Example uses and instances of the @HasReader@ capability.
module Reader where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT (..))
import GHC.Generics (Generic)
import System.IO.Silently (capture_)
import Test.Hspec

import HasReader


----------------------------------------------------------------------
-- Example Programs

-- | Returns the triple of the number in context "foo".
tripleFoo :: HasReader "foo" Int m => m Int
tripleFoo = do
  single <- ask @"foo"
  double <- asks @"foo" (*2)
  pure $ single + double

-- | Prints the triple and sixfold of the number in context "foo".
fooExample :: (HasReader "foo" Int m, MonadIO m) => m ()
fooExample = do
  liftIO . print =<< tripleFoo
  liftIO . print =<< local @"foo" (*2) tripleFoo


-- | Prints the double of "bar" and the triple of "foo".
fooBarExample
  :: (HasReader "foo" Int m, HasReader "bar" Int m, MonadIO m) => m ()
fooBarExample = do
  local @"bar" (*2) $ do
    liftIO . print =<< ask @"bar"
    liftIO . print =<< tripleFoo


----------------------------------------------------------------------
-- Instances

-- | @HasReader@ instance derived via @MonadReader@.
newtype FooReaderT m (a :: *) = FooReaderT (ReaderT Int m a)
  deriving (Functor, Applicative, Monad, MonadIO)
  deriving (HasReader "foo" Int) via MonadReader (ReaderT Int m)

runFooReaderT :: FooReaderT m a -> m a
runFooReaderT (FooReaderT m) = runReaderT m 1


data FooBar = FooBar
  { foo :: Int
  , bar :: Int
  } deriving Generic

-- | Multiple @HasReader@ instances derived via record fields in @MonadReader@.
newtype FooBarReader a = FooBarReader (ReaderT FooBar IO a)
  deriving (Functor, Applicative, Monad, MonadIO)
  deriving (HasReader "foo" Int) via
    Field "foo" (MonadReader (ReaderT FooBar IO))
  deriving (HasReader "bar" Int) via
    Field "bar" (MonadReader (ReaderT FooBar IO))

runFooBarReader :: FooBarReader a -> IO a
runFooBarReader (FooBarReader m) = runReaderT m FooBar { foo = 1, bar = 2 }


-- | Multiple @HasReader@ instances on the same underlying @MonadReader@.
--
-- Demonstrates colliding instances.
-- Note, do not do this in practice. The derived @HasReader@ instances interact
-- in unexpected ways.
newtype BadFooBarReader a = BadFooBarReader (ReaderT Int IO a)
  deriving (Functor, Applicative, Monad, MonadIO)
  deriving (HasReader "foo" Int) via MonadReader (ReaderT Int IO)
  deriving (HasReader "bar" Int) via MonadReader (ReaderT Int IO)

runBadFooBarReader :: BadFooBarReader a -> IO a
runBadFooBarReader (BadFooBarReader m) = runReaderT m 1


----------------------------------------------------------------------
-- Test Cases

spec :: Spec
spec = do
  describe "FooReaderT" $
    it "evaluates fooExample" $ do
      output <- capture_ (runFooReaderT fooExample)
      output `shouldBe` "3\n6\n"
  describe "FooBarReader" $ do
    it "evaluates fooExample" $ do
      output <- capture_ (runFooBarReader fooExample)
      output `shouldBe` "3\n6\n"
    it "evaluates fooBarExample" $ do
      output <- capture_ (runFooBarReader fooBarExample)
      output `shouldBe` "4\n3\n"
  describe "BadFooBarReader" $ do
    it "evaluates fooExample" $ do
      output <- capture_ (runBadFooBarReader fooExample)
      output `shouldBe` "3\n6\n"
    it "evaluates fooBarExample" $ do
      output <- capture_ (runBadFooBarReader fooBarExample)
      output `shouldNotBe` "4\n3\n"
