{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Playground where

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Reader (ReaderT, runReaderT)
import qualified Control.Monad.Reader.Class
import Data.Coerce
import Data.Deriving.Via
import qualified Data.Generics.Internal.VL.Lens as GenericLens
import qualified Data.Generics.Product.Fields as GenericLens
import Data.IORef
import Data.Monoid
import Data.Semigroup
import GHC.Exts (Proxy#, proxy#)
import GHC.Generics (Generic)
import GHC.TypeLits


----------------------------------------------------------------------
-- Simple deriving via example

newtype UsingApplicative f a = UsingApplicative (f a)

-- Given an @Applicative@ @f@ and a @Monoid@ @a@, @f a@ is a @Monoid@. 

instance (Applicative f, Semigroup a)
  => Semigroup (UsingApplicative f a)
  where
    (<>) = coerce (liftA2 (<>) :: f a -> f a -> f a)

instance (Applicative f, Monoid a)
  => Monoid (UsingApplicative f a)
  where
    mempty = coerce (pure mempty :: f a)
    mappend = (<>)


newtype UsingAlternative f a = UsingAlternative (f a)

-- Given an @Alternatve@ @f@ and any @a@, @f a@ is a @Monoid@. 

instance Alternative f
  => Semigroup (UsingAlternative f a)
  where
    (<>) = coerce ((<|>) :: f a -> f a -> f a)

instance Alternative f
  => Monoid (UsingAlternative f a)
  where
    mempty = coerce (empty :: f a)
    mappend = (<>)


-- Try @deriving via@ introduced in GHC 8.6.

newtype Foo (f :: * -> *) a = Foo (f a)
  deriving (Applicative, Functor)
  -- Inline deriving via
  deriving Semigroup via (UsingApplicative (Foo f) a)
-- Standalone deriving via
deriving via (UsingApplicative (Foo f) a)
  instance (Applicative f, Monoid a) => Monoid (Foo f a)


-- Try the backwards compatible template Haskell @deriveVia@
-- from @deriving-compat@.

newtype Bar f a = Bar (f a)
-- deriving-compat template Haskell deriving via
$(deriveVia [t|
  forall f a. Alternative f => Semigroup (Bar f a) `Via` UsingAlternative f a
  |])
$(deriveVia [t|
  forall f a. Alternative f => Monoid (Bar f a) `Via` UsingAlternative f a
  |])


----------------------------------------------------------------------
-- Capabilities playground


-- | @Has tag a s@ describes a type @s@ that can be projected to @a@
-- associated with @tag@.
class Has (tag :: k) a s | tag s -> a where
  proj_ :: Proxy# tag -> s -> a

proj :: forall tag a s. Has tag a s => s -> a
proj = proj_ (proxy# @_ @tag)


newtype UsingGenericLens s = UsingGenericLens s

-- | A record can be projected to one of its fields.
instance
  (Generic s, GenericLens.HasField' tag s a)
  => Has tag a (UsingGenericLens s)
  where
    proj_ _ = coerce (GenericLens.view (GenericLens.field' @tag) :: s -> a)


-- | A context @m@ in which we can access a value @a@ associated with @tag@.
class HasReader (tag :: k) a m | tag m -> a where
  ask_ :: Proxy# tag -> m a

ask :: forall tag a m. HasReader tag a m => m a
ask = ask_ (proxy# @_ @tag)


-- | A context @m@ in which we can modify a value @a@ associated with @tag@.
class HasState (tag :: k) a m | tag m -> a where
  get_ :: Proxy# tag -> m a
  put_ :: Proxy# tag -> a -> m ()

get :: forall tag a m. HasState tag a m => m a
get = get_ (proxy# @_ @tag)

put :: forall tag a m. HasState tag a m => a -> m ()
put = put_ (proxy# @_ @tag)


newtype UsingHas s = UsingHas s

newtype UsingMTL t m a = UsingMTL (t m a)

instance (Has tag r s, Monad m)
  => HasReader tag r (UsingMTL (ReaderT (UsingHas s)) m) where
  ask_ tag =
    coerce (Control.Monad.Reader.Class.asks proj' :: ReaderT (UsingHas s) m r)
    where proj' = coerce (proj_ tag :: s -> r)


newtype UsingReader r m a = UsingReader (m a)

newtype UsingRef ref = UsingRef ref

instance
  (HasReader tag (IORef a) m, MonadIO m)
  => HasState tag a (UsingReader (UsingRef (IORef a)) m)
  where
    get_ tag = coerce (do
      ref <- ask_ tag :: m (IORef a)
      liftIO $ readIORef ref)
    put_ tag a = coerce (do
      ref <- ask_ tag :: m (IORef a)
      liftIO $ writeIORef ref a)


newtype UsingReaderTIORef s m a = UsingReaderTIORef (ReaderT s m a)
  deriving (Functor, Applicative, Monad, MonadIO)

deriving via (UsingMTL (ReaderT (UsingHas s)) (m :: * -> *))
  instance
    (Monad m, Has tag (IORef a) s)
    => HasReader tag (IORef a) (UsingReaderTIORef s m)

deriving via (UsingReader (UsingRef (IORef a)) (UsingReaderTIORef s m :: * -> *))
  instance
    (Has tag (IORef a) s, MonadIO m)
    => HasState tag a (UsingReaderTIORef s m)

data TagA


data RecordFoo = RecordFoo
  { fooRefA :: IORef Int
  , fooRefB :: IORef Int
  , fooC :: Int
  }
  deriving Generic
instance Has TagA (IORef Int) RecordFoo where
  proj_ _ = fooRefA
deriving via (UsingGenericLens RecordFoo)
  instance Has "fooRefB" (IORef Int) RecordFoo
deriving via (UsingGenericLens RecordFoo)
  instance Has "fooC" Int RecordFoo


newtype ReaderFoo m a = ReaderFoo (ReaderT RecordFoo m a)
  deriving (Functor, Applicative, Monad, MonadIO)
-- XXX: These are quite wordy. Maybe squash this to @UsingReaderT@
--deriving via (UsingMTL (ReaderT (UsingHas RecordFoo)) (m :: * -> *))
--  instance Monad m => HasReader TagA (IORef Int) (ReaderFoo m)
--deriving via (UsingMTL (ReaderT (UsingHas RecordFoo)) (m :: * -> *))
--  instance Monad m => HasReader "fooRefB" (IORef Int) (ReaderFoo m)
deriving via (UsingMTL (ReaderT (UsingHas RecordFoo)) (m :: * -> *))
  instance Monad m => HasReader "fooC" Int (ReaderFoo m)
-- XXX: This requires @MonadIO (ReaderFoo m)@. It should only require @MonadIO m@.
--deriving via (UsingReader (UsingRef (IORef Int)) (ReaderFoo (m :: * -> *)))
--  instance (Monad m, MonadIO m) => HasState TagA Int (ReaderFoo m)
--deriving via (UsingReader (UsingRef (IORef Int)) (ReaderFoo (m :: * -> *)))
--  instance (Monad m, MonadIO m) => HasState "fooRefB" Int (ReaderFoo m)
deriving via (UsingReaderTIORef RecordFoo (m :: * -> *))
  instance MonadIO m => HasState TagA Int (ReaderFoo m)
deriving via (UsingReaderTIORef RecordFoo (m :: * -> *))
  instance MonadIO m => HasState "fooRefB" Int (ReaderFoo m)


example
  :: (Monad m, HasReader "fooC" Int m, HasState TagA Int m, HasState "fooRefB" Int m)
  => m Int
example = do
  fooC <- ask @"fooC"
  r <- (fooC +) <$> liftA2 (+) (get @TagA) (get @"fooRefB")
  put @TagA 42
  put @"fooRefB" 13
  pure r


runFoo :: Show a => ReaderFoo IO a -> IO ()
runFoo (ReaderFoo m) = do
  refA <- newIORef 1
  refB <- newIORef 2
  let c = 3
  r <- runReaderT m RecordFoo
    { fooRefA = refA
    , fooRefB = refB
    , fooC = c
    }
  a <- readIORef refA
  b <- readIORef refB
  putStr
    $  "refA: " ++ show a ++ "\n"
    ++ "refB: " ++ show b ++ "\n"
    ++ "valC: " ++ show c ++ "\n"
    ++ "result: " ++ show r ++ "\n"


main :: IO ()
main = runFoo example
