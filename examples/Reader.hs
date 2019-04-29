{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}

-- | Example uses and instances of the @HasReader@ capability.
module Reader where

import Capability.Reader
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT (..))
import GHC.Generics (Generic)
import Test.Common
import Test.Hspec

data Tag = Foo | Bar

type instance TypeOf Tag 'Foo = Int
type instance TypeOf Tag 'Bar = Int

----------------------------------------------------------------------
-- Example Programs

-- | Returns the triple of the number in context "foo".
tripleFoo :: HasReader' 'Foo m => m Int
tripleFoo = do
  single <- ask @'Foo
  double <- asks @'Foo (*2)
  pure $ single + double

-- | Prints the triple and sixfold of the number in context "foo".
fooExample :: (HasReader' 'Foo m, MonadIO m) => m ()
fooExample = do
  liftIO . print =<< tripleFoo
  liftIO . print =<< local @'Foo (*2) tripleFoo


-- | Prints the double of "bar" and the triple of "foo".
fooBarExample
  :: (HasReader' 'Foo m, HasReader' 'Bar m, MonadIO m) => m ()
fooBarExample = do
  local @'Bar (*2) $ do
    liftIO . print =<< ask @'Bar
    liftIO . print =<< tripleFoo


----------------------------------------------------------------------
-- Instances

-- | @HasReader@ instance derived via @MonadReader@.
newtype FooReaderT m (a :: *) = FooReaderT (ReaderT Int m a)
  deriving (Functor, Applicative, Monad, MonadIO)
  deriving (HasReader 'Foo Int) via MonadReader (ReaderT Int m)

runFooReaderT :: FooReaderT m a -> m a
runFooReaderT (FooReaderT m) = runReaderT m 1


data FooBar = FooBar
  { foo :: Int
  , bar :: Int
  } deriving Generic

-- | Multiple @HasReader@ instances derived via record fields in @MonadReader@.
newtype FooBarReader a = FooBarReader (ReaderT FooBar IO a)
  deriving (Functor, Applicative, Monad, MonadIO)
  deriving (HasReader 'Foo Int) via
    Rename "foo" (Field "foo" () (MonadReader (ReaderT FooBar IO)))
  deriving (HasReader 'Bar Int) via
    Rename "bar" (Field "bar" () (MonadReader (ReaderT FooBar IO)))

runFooBarReader :: FooBarReader a -> IO a
runFooBarReader (FooBarReader m) = runReaderT m FooBar { foo = 1, bar = 2 }


-- | Multiple @HasReader@ instances on the same underlying @MonadReader@.
--
-- Demonstrates colliding instances.
-- Note, do not do this in practice. The derived @HasReader@ instances interact
-- in unexpected ways.
newtype BadFooBarReader a = BadFooBarReader (ReaderT Int IO a)
  deriving (Functor, Applicative, Monad, MonadIO)
  deriving (HasReader 'Foo Int) via MonadReader (ReaderT Int IO)
  deriving (HasReader 'Bar Int) via MonadReader (ReaderT Int IO)

runBadFooBarReader :: BadFooBarReader a -> IO a
runBadFooBarReader (BadFooBarReader m) = runReaderT m 1


----------------------------------------------------------------------
-- Test Cases

spec :: Spec
spec = do
  describe "FooReaderT" $
    it "evaluates fooExample" $
      runFooReaderT fooExample `shouldPrint` "3\n6\n"
  describe "FooBarReader" $ do
    it "evaluates fooExample" $
      runFooBarReader fooExample `shouldPrint` "3\n6\n"
    it "evaluates fooBarExample" $
      runFooBarReader fooBarExample `shouldPrint` "4\n3\n"
  describe "BadFooBarReader" $ do
    it "evaluates fooExample" $
      runBadFooBarReader fooExample `shouldPrint` "3\n6\n"
    it "evaluates fooBarExample" $
      runBadFooBarReader fooBarExample `shouldNotPrint` "4\n3\n"
