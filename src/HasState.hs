{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module HasState
  ( HasState (..)
  , get
  , put
  , state
  , modify
  , modify'
  , gets
  , TheMonadState (..)
  ) where

import Control.Lens ((.=), over, set, use, view)
import qualified Control.Monad.State.Class as State
import qualified Control.Monad.State.Lazy as LState
import qualified Control.Monad.State.Strict as SState
import Data.Coerce (coerce)
import GHC.Exts (Proxy#, proxy#)

import Has


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


newtype TheMonadState m a = TheMonadState (m a)
  deriving (Functor, Applicative, Monad)
instance
  (Has tag s s', State.MonadState s' m)
  => HasState tag s (TheMonadState m)
  where
    get_ tag = coerce @(m s) $ use (has_ tag)
    put_ tag v = coerce @(m ()) $ has_ tag .= v
    state_ :: forall a. Proxy# tag -> (s -> (a, s)) -> TheMonadState m a
    state_ tag f = coerce @(m a) $ State.state $ \s' ->
      let (a, s) = f $ view (has_ tag) s' in
      (a, set (has_ tag) s s')

deriving via (TheMonadState (LState.StateT s' (m :: * -> *)))
  instance (Has tag s s', Monad m) => HasState tag s (LState.StateT s' m)
deriving via (TheMonadState (SState.StateT s' (m :: * -> *)))
  instance (Has tag s s', Monad m) => HasState tag s (SState.StateT s' m)
