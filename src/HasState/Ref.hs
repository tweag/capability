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
  ( ReaderRef (..)
  ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Mutable

import HasReader
import HasState


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
