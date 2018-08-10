{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
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
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}

module HasState
  ( HasState (..)
  , get
  , put
  , state
  , modify
  , modify'
  , gets
  , MonadState (..)
  , ReaderIORef (..)
  , module Accessors
  ) where

import Control.Lens (set, view)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Control.Monad.State.Class as State
import Data.Coerce (coerce)
import qualified Data.Generics.Product.Fields as Generic
import Data.IORef
import GHC.Exts (Proxy#, proxy#)
import GHC.Generics (Generic)

import Accessors
import HasReader -- Used for ReaderIORef below


class Monad m
  => HasState (tag :: k) (s :: *) (m :: * -> *) | tag m -> s
  where
    get_ :: Proxy# tag -> m s
    put_ :: Proxy# tag -> s -> m ()
    state_ :: Proxy# tag -> (s -> (a, s)) -> m a

get :: forall tag s m. HasState tag s m => m s
get = get_ (proxy# @_ @tag)

put :: forall tag s m. HasState tag s m => s -> m ()
put = put_ (proxy# @_ @tag)

state :: forall tag s m a. HasState tag s m => (s -> (a, s)) -> m a
state = state_ (proxy# @_ @tag)

modify :: forall tag s m. HasState tag s m => (s -> s) -> m ()
modify f = state @tag $ \s -> ((), f s)

modify' :: forall tag s m. HasState tag s m => (s -> s) -> m ()
modify' f = do
  s' <- get @tag
  put @tag $! f s'

gets :: forall tag s m a. HasState tag s m => (s -> a) -> m a
gets f = do
  s <- get @tag
  pure (f s)


-- | Derive 'HasState' from @m@'s
-- 'Control.Monad.State.Class.MonadState' instance.
newtype MonadState (m :: * -> *) (a :: *) = MonadState (m a)
  deriving (Functor, Applicative, Monad, MonadIO)
instance State.MonadState s m => HasState tag s (MonadState m) where
  get_ _ = coerce @(m s) State.get
  put_ _ = coerce @(s -> m ()) State.put
  state_ :: forall a. Proxy# tag -> (s -> (a, s)) -> MonadState m a
  state_ _ = coerce @((s -> (a, s)) -> m a) State.state


-- | Zoom in on the record field @field@ of type @s@ in the state @s'@.
instance
  -- The constraint raises @-Wsimplifiable-class-constraints@.
  -- This could be avoided by instead placing @HasField'@s constraints here.
  -- Unfortunately, it uses non-exported symbols from @generic-lens@.
  ( Generic record, Generic.HasField' field record v, HasState tag record m )
  => HasState tag v (Field field m)
  where
    get_ _ = coerce @(m v) $
      gets @tag $ view (Generic.field' @field)
    put_ _ = coerce @(v -> m ()) $
      modify @tag . set (Generic.field' @field @record)
    state_ :: forall a. Proxy# tag -> (v -> (a, v)) -> Field field m a
    state_ _ = coerce @((v -> (a, v)) -> m a) $
      state @tag . Generic.field' @field @_ @_ @((,) a)


-- XXX: The following might belong to a different module


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
