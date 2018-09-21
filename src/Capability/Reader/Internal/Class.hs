{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeInType #-}

module Capability.Reader.Internal.Class
  ( HasReader(..)
  , ask
  , asks
  , local
  , reader
  ) where

import GHC.Exts (Proxy#, proxy#)

-- | Reader capability
--
-- An instance should fulfill the following laws.
-- At this point these laws are not definitive,
-- see <https://github.com/haskell/mtl/issues/5>.
--
-- prop> k <*> ask @t = ask @t <**> k
-- prop> ask @t *> m = m = m <* ask @t
-- prop> local @t f (ask @t) = fmap f (ask @t)
-- prop> local @t f . local @t g = local @t (g . f)
-- prop> local @t f (pure x) = pure x
-- prop> local @t f (m >>= \x -> k x) = local @t f m >>= \x -> local @t f (k x)
-- prop> reader @t f = f <$> ask @t
class Monad m
  => HasReader (tag :: k) (r :: *) (m :: * -> *) | tag m -> r
  where
    -- | Use 'ask' instead.
    ask_ :: Proxy# tag -> m r
    -- | Use 'local' instead.
    local_ :: Proxy# tag -> (r -> r) -> m a -> m a
    -- | User 'reader' instead.
    reader_ :: Proxy# tag -> (r -> a) -> m a

-- | @ask \@tag@
-- Retrieve the environment.
ask :: forall tag r m. HasReader tag r m => m r
ask = ask_ (proxy# @_ @tag)
{-# INLINE ask #-}

-- | @asks \@tag@
-- Retrieve a projection of environment according to @f@.
--
-- XXX: Seems identical to 'reader'. Is 'asks' redundant?
asks :: forall tag r m a. HasReader tag r m => (r -> a) -> m a
asks f = f <$> ask @tag
{-# INLINE asks #-}

-- | @local \@tag f m@
-- Execute @m@ with the environment updated according to @f@.
local :: forall tag r m a. HasReader tag r m => (r -> r) -> m a -> m a
local = local_ (proxy# @_ @tag)
{-# INLINE local #-}

-- | @reader \@tag@
-- Retrieve a projection of environment according to @f@.
reader :: forall tag r m a. HasReader tag r m => (r -> a) -> m a
reader = reader_ (proxy# @_ @tag)
{-# INLINE reader #-}
