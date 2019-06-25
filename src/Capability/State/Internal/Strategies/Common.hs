{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

{-# OPTIONS_HADDOCK hide #-}

module Capability.State.Internal.Strategies.Common
  ( MonadState(..)
  , ReaderIORef(..)
  , ReaderRef(..)
  ) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Primitive (PrimMonad)

-- | Derive 'HasState' from @m@'s
-- 'Control.Monad.State.Class.MonadState' instance.
newtype MonadState (m :: * -> *) (a :: *) = MonadState (m a)
  deriving (Functor, Applicative, Monad, MonadIO, PrimMonad)

-- | Derive a state monad from a reader over an 'Data.IORef.IORef'.
--
-- Example:
--
-- > newtype MyState m a = MyState (ReaderT (IORef Int) m a)
-- >   deriving (Functor, Applicative, Monad)
-- >   deriving HasState "foo" Int via
-- >     ReaderIORef (MonadReader (ReaderT (IORef Int) m))
--
-- See 'ReaderRef' for a more generic strategy.
newtype ReaderIORef m a = ReaderIORef (m a)
  deriving (Functor, Applicative, Monad)

-- | Derive a state monad from a reader over a mutable reference.
--
-- Mutable references are available in a 'Control.Monad.Primitive.PrimMonad'.
-- The corresponding 'Control.Monad.Primitive.PrimState' has to match the
-- 'Data.Mutable.MCState' of the reference. This constraint makes a stand-alone
-- deriving clause necessary.
--
-- Example:
--
-- > newtype MyState m a = MyState (ReaderT (IORef Int) m a)
-- >   deriving (Functor, Applicative, Monad)
-- > deriving via ReaderRef (MonadReader (ReaderT (IORef Int) m))
-- >   instance (PrimMonad m, PrimState m ~ PrimState IO)
-- >   => HasState "foo" Int (MyState m)
--
-- See 'ReaderIORef' for a specialized version over 'Data.IORef.IORef'.
newtype ReaderRef m (a :: *) = ReaderRef (m a)
  deriving (Functor, Applicative, Monad, MonadIO, PrimMonad)
