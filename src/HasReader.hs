{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module HasReader
  ( HasReader (..)
  , ask
  , asks
  , local
  , reader
  , TheMonadReader (..)
  ) where

import Control.Lens (over, view)
import Control.Monad.Reader (ReaderT)
import qualified Control.Monad.Reader.Class as Reader
import Data.Coerce (coerce)
import GHC.Exts (Proxy#, proxy#)

import Has


class Monad m
  => HasReader (tag :: k) (r :: *) (m :: * -> *) | tag m -> r
  where
    ask_ :: Proxy# tag -> m r
    local_ :: Proxy# tag -> (r -> r) -> m a -> m a
    reader_ :: Proxy# tag -> (r -> a) -> m a

ask :: forall tag r m. HasReader tag r m => m r
ask = ask_ (proxy# @_ @tag)

asks :: forall tag r m a. HasReader tag r m => (r -> a) -> m a
asks f = f <$> ask @tag

local :: forall tag r m a. HasReader tag r m => (r -> r) -> m a -> m a
local = local_ (proxy# @_ @tag)

reader :: forall tag r m a. HasReader tag r m => (r -> a) -> m a
reader = reader_ (proxy# @_ @tag)


newtype TheMonadReader m a = TheMonadReader (m a)
  deriving (Functor, Applicative, Monad)
instance
  (Has tag r r', Reader.MonadReader r' m)
  => HasReader tag r (TheMonadReader m)
  where
    ask_ tag = coerce (view (has_ tag) :: m r)
    local_ :: forall a.
      Proxy# tag -> (r -> r) -> TheMonadReader m a -> TheMonadReader m a
    local_ tag f = coerce (Reader.local $ over (has_ tag) f :: m a -> m a)
    reader_ :: forall a.
      Proxy# tag -> (r -> a) -> TheMonadReader m a
    reader_ tag f = coerce (Reader.reader (f . view (has_ tag)) :: m a)

deriving via (TheMonadReader (ReaderT r' (m :: * -> *)))
  instance (Has tag r r', Monad m) => HasReader tag r (ReaderT r' m)
