{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module HasState.Ref
  ( ReaderIORef (..)
  , ReaderRef (..)
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef
import Data.Mutable

import HasReader
import HasState


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
instance
  (HasReader tag (IORef s) m, MonadIO m)
  => HasState tag s (ReaderIORef m)
  where
    get_ _ = ReaderIORef $ do
      ref <- ask @tag
      liftIO $ readIORef ref
    put_ _ v = ReaderIORef $ do
      ref <- ask @tag
      liftIO $ writeIORef ref v
    state_ _ f = ReaderIORef $ do
      ref <- ask @tag
      liftIO $ atomicModifyIORef' ref (swap . f)
      where
        swap (a, b) = (b, a)


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
instance
  ( MutableRef ref, RefElement ref ~ s
  , HasReader tag ref m, PrimMonad m, PrimState m ~ MCState ref )
  => HasState tag s (ReaderRef m)
  where
    get_ _ = ReaderRef $ do
      ref <- ask @tag
      readRef ref
    put_ _ v = ReaderRef $ do
      ref <- ask @tag
      writeRef ref v
    state_ _ f = ReaderRef $ do
      ref <- ask @tag
      s <- readRef ref
      let (a, s') = f s
      writeRef ref s'
      pure a
