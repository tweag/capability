{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}

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
  , Reified (..)
  ) where

import Capability.Constraints
import Capability.Derive (derive)
import Capability.Reflection
import Capability.Source.Internal.Class
import Capability.Sink.Internal.Class
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
class (Monad m, HasSource tag s m, HasSink tag s m)
  => HasState (tag :: k) (s :: *) (m :: * -> *) | tag m -> s
  where
    -- | For technical reasons, this method needs an extra proxy argument.
    -- You only need it if you are defining new instances of 'HasState.
    -- Otherwise, you will want to use 'state'.
    -- See 'state' for more documentation.
    state_ :: Proxy# tag -> (s -> (a, s)) -> m a

-- | @get \@tag@
-- retrieve the current state of the state capability @tag@.
get :: forall tag s m. HasState tag s m => m s
get = await @tag
{-# INLINE get #-}

-- | @put \@tag s@
-- replace the current state of the state capability @tag@ with @s@.
put :: forall tag s m. HasState tag s m => s -> m ()
put = yield @tag
{-# INLINE put #-}

-- | @state \@tag f@
-- lifts a pure state computation @f@ to a monadic action in an arbitrary
-- monad @m@ with capability @HasState@.
--
-- Given the current state @s@ of the state capability @tag@
-- and @(a, s') = f s@, update the state to @s'@ and return @a@.
state :: forall tag s m a. HasState tag s m => (s -> (a, s)) -> m a
state = state_ (proxy# @tag)
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

-- | Execute the given state action on a sub-component of the current state as
-- defined by the given transformer @t@. The set of retained capabilities must
-- be passed as @cs. If no capabilities are required,
-- 'Capabilities.Constraints.None' can be used.
--
-- Examples:
--
-- > foo :: HasState "foo" Int m => m ()
-- > zoom @"foo" @(Field "foo" "foobar") @None foo
-- >   :: (HasField' "foobar" record Int, HasState "foobar" record m) => m ()
-- >
-- > zoom @"foo" @(Field "foo" "foobar") @('[MonadIO]) bar
-- >   :: ( HasField' "foobar" record Int, HasState "foobar" record m
-- >      , MonadIO m) => m ()
-- >
-- > foo :: HasState "foo" Int m => m ()
-- > bar :: (MonadIO m, HasState "foo" Int m) => m ()
--
-- Note: the 'Data.Generics.Product.Fields.HasField'' constraint comes from the
-- @generic-lens@ package.
--
-- This function is experimental and subject to change.
-- See <https://github.com/tweag/capability/issues/46>.
zoom :: forall innertag t (cs :: [Capability]) inner m a.
  ( forall x. Coercible (t m x) (m x)
  , HasState innertag inner (t m)
  , All cs m )
  => (forall m'. All (HasState innertag inner ': cs) m' => m' a) -> m a
zoom =
  derive @t @'[HasState innertag inner] @cs
{-# INLINE zoom #-}

--------------------------------------------------------------------------------

data instance Reified tag (HasState tag s) m = ReifiedState
  { _stateSource :: Reified tag (HasSource tag s) m,
    _stateSink :: Reified tag (HasSink tag s) m,
    _state :: forall a. (s -> (a, s)) -> m a
  }

instance
  ( Monad m,
    Reifies s' (Reified tag (HasState tag s) m)
  ) =>
  HasSource tag s (Reflected s' (HasState tag s) m)
  where
  await_ _ = coerce $ _await $ _stateSource $ reified @s'
  {-# INLINE await_ #-}

instance
  ( Monad m,
    Reifies s' (Reified tag (HasState tag s) m)
  ) =>
  HasSink tag s (Reflected s' (HasState tag s) m)
  where
  yield_ _ = coerce $ _yield $ _stateSink $ reified @s'
  {-# INLINE yield_ #-}

instance
  ( Monad m,
    Reifies s' (Reified tag (HasState tag s) m)
  ) =>
  HasState tag s (Reflected s' (HasState tag s) m)
  where
  state_ :: forall a. Proxy# tag -> (s -> (a, s)) -> Reflected s' (HasState tag s) m a
  state_ _ = coerce @((s -> (a, s)) -> m a) $ _state (reified @s')
  {-# INLINE state_ #-}
