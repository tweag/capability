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
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
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
  ) where

import Control.Lens (Lens', set, view)
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


newtype MonadState (m :: * -> *) (a :: *) = MonadState (m a)
  deriving (Functor, Applicative, Monad, MonadIO)
instance State.MonadState s m => HasState tag s (MonadState m) where
  get_ _ = coerce @(m s) State.get
  put_ _ = coerce @(s -> m ()) State.put
  state_ :: forall a. Proxy# tag -> (s -> (a, s)) -> MonadState m a
  state_ _ = coerce @((s -> (a, s)) -> m a) State.state


-- The constraint raises @-Wsimplifiable-class-constraints@.
-- This could be avoided by instead placing @HasField'@s constraints here.
-- Unfortunately, it uses non-exported symbols from @generic-lens@.
instance
  ( Generic s', Generic.HasField' field s' s, HasState tag s' m )
  => HasState tag s (Field field m)
  where
    get_ _ = coerce @(m s) $
      gets @tag $ view (Generic.field' @field)
    put_ _ = coerce @(s -> m ()) $
      modify @tag . set (Generic.field' @field @s')
    state_ :: forall a. Proxy# tag -> (s -> (a, s)) -> Field field m a
    state_ _ = coerce @((s -> (a, s)) -> m a) $
      state @tag . stateOnLens (Generic.field' @field)


stateOnLens :: Lens' s' s -> (s -> (a, s)) -> s' -> (a, s')
stateOnLens l f s' = let (a, s) = f (view l s') in (a, set l s s')


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
