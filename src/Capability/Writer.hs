-- | Defines a writer monad capability.
--
-- The interface of 'HasWriter' follows that of
-- 'Control.Monad.Writer.Class.MonadWriter'. However, we do
-- not provide a strategy to derive a @HasWriter@ instance from a
-- @MonadWriter@ instance. Implementations of @MonadWriter@ based on
-- 'Control.Monad.Writer.Strict.WriterT' or similar have a space-leak, see this
-- <https://blog.infinitenegativeutility.com/2016/7/writer-monads-and-space-leaks blog post>
-- by Getty Ritter.
--
-- Instead, we provide the 'WriterLog' strategy that implements the writer
-- monad on a state monad. Using 'HasWriter' instead of 'HasState' directly
-- ensures your code is restricted to the writer monad interface and does
-- not misuse the underlying state monad.

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

module Capability.Writer
  ( -- * Interface
    HasWriter(..)
  , writer
  , tell
  , listen
  , pass
    -- * Strategies
  , WriterLog(..)
    -- ** Modifiers
  , module Capability.Accessors
  ) where

import Capability.Accessors
import Capability.State
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Primitive (PrimMonad)
import Data.Coerce (coerce)
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
class (Monoid w, Monad m)
  => HasWriter (tag :: k) (w :: *) (m :: * -> *) | tag m -> w
  where
    -- | For technical reasons, this method needs an extra proxy argument.
    -- You only need it if you are defining new instances of 'HasReader'.
    -- Otherwise, you will want to use 'writer'.
    -- See 'writer' for more documentation.
    writer_ :: Proxy# tag -> (a, w) -> m a
    -- | For technical reasons, this method needs an extra proxy argument.
    -- You only need it if you are defining new instances of 'HasReader'.
    -- Otherwise, you will want to use 'tell'.
    -- See 'tell' for more documentation.
    tell_ :: Proxy# tag -> w -> m ()
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
tell = tell_ (proxy# @_ @tag)
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

newtype WriterLog m a = WriterLog (m a)
  deriving (Functor, Applicative, Monad, MonadIO, PrimMonad)

instance (Monoid w, HasState tag w m)
  => HasWriter tag w (WriterLog m)
  where
    writer_ tag (a, w) = tell_ tag w >> pure a
    {-# INLINE writer_ #-}
    tell_ _ w = coerce @(m ()) $ modify' @tag (<> w)
    {-# INLINE tell_ #-}
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
