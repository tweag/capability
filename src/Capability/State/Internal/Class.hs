{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeInType #-}

{-# OPTIONS_HADDOCK hide #-}

module Capability.State.Internal.Class
  ( HasState(..)
  , get
  , put
  , state
  , modify
  , modify'
  , gets
  , zoom
  ) where

import Data.Coerce (Coercible, coerce)
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
    -- | For technical reasons, this method needs an extra proxy argument.
    -- You only need it if you are defining new instances of 'HasState.
    -- Otherwise, you will want to use 'get'.
    -- See 'get' for more documentation.
    get_ :: Proxy# tag -> m s
    -- | For technical reasons, this method needs an extra proxy argument.
    -- You only need it if you are defining new instances of 'HasState.
    -- Otherwise, you will want to use 'put'.
    -- See 'put' for more documentation.
    put_ :: Proxy# tag -> s -> m ()
    -- | For technical reasons, this method needs an extra proxy argument.
    -- You only need it if you are defining new instances of 'HasState.
    -- Otherwise, you will want to use 'state'.
    -- See 'state' for more documentation.
    state_ :: Proxy# tag -> (s -> (a, s)) -> m a

-- | @get \@tag@
-- retrieve the current state of the state capability @tag@.
get :: forall tag s m. HasState tag s m => m s
get = get_ (proxy# @_ @tag)
{-# INLINE get #-}

-- | @put \@tag s@
-- replace the current state of the state capability @tag@ with @s@.
put :: forall tag s m. HasState tag s m => s -> m ()
put = put_ (proxy# @_ @tag)
{-# INLINE put #-}

-- | @state \@tag f@
-- lifts a pure state computation @f@ to a monadic action in an arbitrary
-- monad @m@ with capability @HasState@.
--
-- Given the current state @s@ of the state capability @tag@
-- and @(a, s') = f s@, update the state to @s'@ and return @a@.
state :: forall tag s m a. HasState tag s m => (s -> (a, s)) -> m a
state = state_ (proxy# @_ @tag)
{-# INLINE state #-}

-- | @modify \@tag f@
-- given the current state @s@ of the state capability @tag@
-- and @s' = f s@, updates the state of the capability @tag@ to @s'@.
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
-- retrieves the image, by @f@ of the current state
-- of the state capability @tag@.
--
-- prop> gets @tag f = f <$> get @tag
gets :: forall tag s m a. HasState tag s m => (s -> a) -> m a
gets f = do
  s <- get @tag
  pure (f s)
{-# INLINE gets #-}

-- | Execute the given state action on a sub-component of the current state
-- as defined by the given transformer @t@.
--
-- Example:
--
-- > zoom @"foobar" @"foo" @(Field "foo" "foobar") foo
-- >   :: HasState "foobar" FooBar m => m ()
-- >
-- > foo :: HasState "foo" Int m => m ()
-- > data FooBar = FooBar { foo :: Int, bar :: String }
--
-- This function is experimental and subject to change.
-- See <https://github.com/tweag/capability/issues/46>.
zoom :: forall outertag innertag t outer inner m a.
  ( forall x. Coercible (t m x) (m x)
  , forall m'. HasState outertag outer m'
    => HasState innertag inner (t m')
  , HasState outertag outer m )
  => (forall m'. HasState innertag inner m' => m' a) -> m a
zoom m = coerce @(t m a) m
{-# INLINE zoom #-}
