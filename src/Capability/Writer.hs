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
-- See <https://github.com/haskell/mtl/issues/5>.
--
-- XXX: What laws?
class (Monoid w, Monad m)
  => HasWriter (tag :: k) (w :: *) (m :: * -> *) | tag m -> w
  where
    -- | Use 'writer' instead.
    writer_ :: Proxy# tag -> (a, w) -> m a
    -- | Use 'tell' instead.
    tell_ :: Proxy# tag -> w -> m ()
    -- | Use 'listen' instead.
    listen_ :: Proxy# tag -> m a -> m (a, w)
    -- | Use 'pass' instead.
    pass_ :: Proxy# tag -> m (a, w -> w) -> m a

-- | @writer \@tag (a, w)@
-- Write the output @w@ and return the value @a@.
writer :: forall tag w m a. HasWriter tag w m => (a, w) -> m a
writer = writer_ (proxy# @_ @tag)
{-# INLINE writer #-}

-- | @tell \@tag w@
-- Write the output @w@.
tell :: forall tag w m. HasWriter tag w m => w -> m ()
tell = tell_ (proxy# @_ @tag)
{-# INLINE tell #-}

-- | @listen \@tag m@
-- Execute @m@ and return its output along with its result.
listen :: forall tag w m a. HasWriter tag w m => m a -> m (a, w)
listen = listen_ (proxy# @_ @tag)
{-# INLINE listen #-}

-- | @pass \@tag m@
-- Execute @m@ and modify its output according to the function it returns.
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
