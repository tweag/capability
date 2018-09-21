{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeInType #-}

module Capability.State.Internal.Class
  ( HasState(..)
  , get
  , put
  , state
  , modify
  , modify'
  , gets
  ) where

import GHC.Exts (Proxy#, proxy#)

-- | State capability
--
-- An instance should fulfill the following laws.
-- At this point these laws are not definitive,
-- see <https://github.com/haskell/mtl/issues/5>.
--
-- prop> get @t >>= \s1 -> get @t >>= \s2 -> pure (s1, s2) = get @t >>= \s -> pure (s, s)
-- prop> get @t >>= \_ -> put @t s = put @t s
-- prop> put @t s1 >> put @t s2 = put @t s2
-- prop> put @t s >> get @t = put @t s >> pure s
-- prop> state @t f = get @t >>= \s -> let (a, s') = f s in put @t s' >> pure a
class Monad m
  => HasState (tag :: k) (s :: *) (m :: * -> *) | tag m -> s
  where
    -- | Use 'get' instead.
    get_ :: Proxy# tag -> m s
    -- | Use 'put' instead.
    put_ :: Proxy# tag -> s -> m ()
    -- | Use 'state' instead.
    state_ :: Proxy# tag -> (s -> (a, s)) -> m a

-- | @get \@tag@
-- Retrieve the current state under @tag@.
get :: forall tag s m. HasState tag s m => m s
get = get_ (proxy# @_ @tag)
{-# INLINE get #-}

-- | @put \@tag s@
-- Replace the current state under @tag@ with @s@.
put :: forall tag s m. HasState tag s m => s -> m ()
put = put_ (proxy# @_ @tag)
{-# INLINE put #-}

-- | @state \@tag f@
-- Update the state under @tag@ and return another value according to @f@.
state :: forall tag s m a. HasState tag s m => (s -> (a, s)) -> m a
state = state_ (proxy# @_ @tag)
{-# INLINE state #-}

-- | @modify \@tag f@
-- Update the state under @tag@ according to @f@.
modify :: forall tag s m. HasState tag s m => (s -> s) -> m ()
modify f = state @tag $ \s -> ((), f s)
{-# INLINE modify #-}

-- | Same as 'modify' but strict in the new state.
modify' :: forall tag s m. HasState tag s m => (s -> s) -> m ()
modify' f = do
  s' <- get @tag
  put @tag $! f s'
{-# INLINE modify' #-}

-- | @gets \@tag f@
-- Retrieve a projection of the current state according to @f@.
gets :: forall tag s m a. HasState tag s m => (s -> a) -> m a
gets f = do
  s <- get @tag
  pure (f s)
{-# INLINE gets #-}
