-- | Defines a capability type class for writer effects. A writer program can
-- output values with 'tell'. The values output by two consecutive
-- sub-computation are combined using a monoid's @mappend@.
--
-- The interface of 'HasWriter' follows that of
-- 'Control.Monad.Writer.Class.MonadWriter'. However, this module does not
-- include a strategy to provide a @HasWriter@ capability from a @MonadWriter@
-- instance. It is generally a bad idea to use monads such as
-- 'Control.Monad.Writer.Strict.WriterT', as they tend to leak space, as
-- described in this
-- <https://blog.infinitenegativeutility.com/2016/7/writer-monads-and-space-leaks
-- blog post> by Getty Ritter.
--
-- Instead, you should use the 'WriterLog' strategy that implements the writer
-- monad on a state monad. There is no downside, as using 'HasWriter' instead of
-- 'HasState' directly ensures your code adheres to the writer monad interface
-- and does not misuse the underlying state monad.

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-simplifiable-class-constraints -Wno-deprecations #-}

module Capability.Writer
  ( -- * Relational capability
    HasWriter(..)
  , writer
  , tell
  , listen
  , pass
  -- * Functional capability
  , HasWriter'
  , TypeOf
    -- * Strategies
  , WriterLog
  , StreamLog
  , SinkLog (..)
    -- ** Modifiers
  , module Capability.Accessors
  ) where

import Capability.Accessors
import Capability.Sink
import Capability.State
-- import deprecated module to reexport deprecated item for back-compat.
import Capability.Stream
import Data.Coerce (Coercible, coerce)
import GHC.Exts (Proxy#, proxy#)

-- | Writer capability
--
-- An instance should fulfill the following laws.
-- At this point these laws are not definitive,
-- see <https://github.com/haskell/mtl/issues/5>.
--
-- prop> listen @t (pure a) = pure (a, mempty)
-- prop> listen @t (tell @t w) = tell @t w >> pure (w, w)
-- prop> listen @t (m >>= k) = listen @t m >>= \(a, w1) -> listen @t (k a) >>= \(b, w2) -> pure (b, w1 `mappend` w2)
-- prop> pass @t (tell @t w >> pure (a, f)) = tell @t (f w) >> pure a
-- prop> writer @t (a, w) = tell @t w >> pure a
--
-- = A note on the 'HasSink' super class.
--
-- 'HasSink' offers one 'yield' method with the same signature as 'tell'.
-- Many people's intuition, however, wouldn't connect the two: 'yield'ing
-- tosses the value down some black-box chute, while 'tell'ing grows and
-- accumulation via the monoid. The connection is since the 'chute' is opaque,
-- the tosser cannot rule out there being such an accumulation at the chutes
-- other end.
--
-- Formally, we reach the same conclusion. 'HasSink' has no laws,
-- indicating the user can make no assumptions beyond the signature of 'yield'.
-- 'HasWriter', with 'tell' defined as 'yield', is thus always compatable
-- regardless of whatever additional methods it provides and laws by which it
-- abides.
class (Monoid w, Monad m, HasSink tag w m)
  => HasWriter (tag :: k) (w :: *) (m :: * -> *) | tag m -> w
  where
    -- | For technical reasons, this method needs an extra proxy argument.
    -- You only need it if you are defining new instances of 'HasReader'.
    -- Otherwise, you will want to use 'writer'.
    -- See 'writer' for more documentation.
    writer_ :: Proxy# tag -> (a, w) -> m a
    -- | For technical reasons, this method needs an extra proxy argument.
    -- You only need it if you are defining new instances of 'HasReader'.
    -- Otherwise, you will want to use 'listen'.
    -- See 'listen' for more documentation.
    listen_ :: Proxy# tag -> m a -> m (a, w)
    -- | For technical reasons, this method needs an extra proxy argument.
    -- You only need it if you are defining new instances of 'HasReader'.
    -- Otherwise, you will want to use 'pass'.
    -- See 'pass' for more documentation.
    pass_ :: Proxy# tag -> m (a, w -> w) -> m a

-- | @writer \@tag (a, w)@
-- lifts a pure writer action @(a, w)@ to a monadic action in an arbitrary
-- monad @m@ with capability @HasWriter@.
--
-- Appends @w@ to the output of the writer capability @tag@
-- and returns the value @a@.
writer :: forall tag w m a. HasWriter tag w m => (a, w) -> m a
writer = writer_ (proxy# @_ @tag)
{-# INLINE writer #-}

-- | @tell \@tag w@
-- appends @w@ to the output of the writer capability @tag@.
tell :: forall tag w m. HasWriter tag w m => w -> m ()
tell = yield_ (proxy# @_ @tag)
{-# INLINE tell #-}

-- | @listen \@tag m@
-- executes the action @m@ and returns the output of @m@
-- in the writer capability @tag@ along with result of @m@.
-- Appends the output of @m@ to the output of the writer capability @tag@.
listen :: forall tag w m a. HasWriter tag w m => m a -> m (a, w)
listen = listen_ (proxy# @_ @tag)
{-# INLINE listen #-}

-- | @pass \@tag m@
-- executes the action @m@. Assuming @m@ returns @(a, f)@ and appends
-- @w@ to the output of the writer capability @tag@.
-- @pass \@tag m@ instead appends @w' = f w@ to the output and returns @a@.
pass :: forall tag w m a. HasWriter tag w m => m (a, w -> w) -> m a
pass = pass_ (proxy# @_ @tag)
{-# INLINE pass #-}

-- | Compose two accessors.
deriving via ((t2 :: (* -> *) -> * -> *) ((t1 :: (* -> *) -> * -> *) m))
  instance
  ( forall x. Coercible (m x) (t2 (t1 m) x)
  , Monad m, HasWriter tag w (t2 (t1 m)) )
  => HasWriter tag w ((t2 :.: t1) m)

type WriterLog = SinkLog

instance (Monoid w, HasState tag w m)
  => HasWriter tag w (WriterLog m)
  where
    writer_ tag (a, w) = yield_ tag w >> pure a
    {-# INLINE writer_ #-}
    listen_ :: forall a. Proxy# tag -> WriterLog m a -> WriterLog m (a, w)
    listen_ _ m = coerce @(m (a, w)) $ do
      w0 <- get @tag
      put @tag mempty
      a <- coerce m
      w <- get @tag
      put @tag $! w0 <> w
      pure (a, w)
    {-# INLINE listen_ #-}
    pass_ :: forall a. Proxy# tag -> WriterLog m (a, w -> w) -> WriterLog m a
    pass_ _ m = coerce @(m a) $ do
      w0 <- get @tag
      put @tag mempty
      (a, f) <- coerce @_ @(m (a, w -> w)) m
      w <- get @tag
      put @tag $! w0 <> f w
      pure a
    {-# INLINE pass_ #-}

-- | Type synonym using the 'TypeOf' type family to specify 'HasWriter'
-- constraints without having to specify the type associated to a tag.
type HasWriter' (tag :: k) = HasWriter tag (TypeOf k tag)
