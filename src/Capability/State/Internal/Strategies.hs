{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
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
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

{-# OPTIONS_HADDOCK hide #-}

module Capability.State.Internal.Strategies
  ( MonadState(..)
  , ReaderIORef(..)
  , ReaderRef(..)
  ) where

import Capability.Accessors
import Capability.Reader.Internal.Class
import Capability.State.Internal.Class
import Capability.State.Internal.Strategies.Common
import Capability.Source.Internal.Strategies ()
import Capability.Sink.Internal.Strategies ()
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Primitive (PrimMonad)
import qualified Control.Monad.State.Class as State
import Control.Monad.Trans.Class (MonadTrans, lift)
import Data.Coerce (Coercible, coerce)
import qualified Data.Generics.Product.Fields as Generic
import qualified Data.Generics.Product.Positions as Generic
import Data.IORef
import Data.Kind (Type)
import Data.Mutable
import GHC.Exts (Proxy#)

instance State.MonadState s m => HasState tag s (MonadState m) where
  state_ :: forall a. Proxy# tag -> (s -> (a, s)) -> MonadState m a
  state_ _ = coerce @((s -> (a, s)) -> m a) State.state
  {-# INLINE state_ #-}

-- | Convert the state using safe coercion.
instance
  ( Coercible from to, HasState tag from m
  , forall x y. Coercible x y => Coercible (m x) (m y) )
  => HasState tag to (Coerce to m)
  where
    state_ :: forall a. Proxy# tag -> (to -> (a, to)) -> Coerce to m a
    state_ tag = coerce @((from -> (a, from)) -> m a) $ state_ tag
    {-# INLINE state_ #-}

-- | Rename the tag.
instance HasState oldtag s m => HasState newtag s (Rename oldtag m) where
  state_ :: forall a. Proxy# newtag -> (s -> (a, s)) -> Rename oldtag m a
  state_ _ = coerce @((s -> (a, s)) -> m a) $ state @oldtag
  {-# INLINE state_ #-}

-- | Zoom in on the record field @field@ of type @v@ in the state @record@.
instance
  -- The constraint raises @-Wsimplifiable-class-constraints@.
  -- This could be avoided by instead placing @HasField'@s constraints here.
  -- Unfortunately, it uses non-exported symbols from @generic-lens@.
  ( tag ~ field, Generic.HasField' field record v, HasState oldtag record m )
  => HasState tag v (Field field oldtag m)
  where
    state_ :: forall a.
      Proxy# tag
      -> (v -> (a, v))
      -> Field field oldtag m a
    state_ _ = coerce @((v -> (a, v)) -> m a) $
      state @oldtag . Generic.field' @field @_ @_ @((,) a)
    {-# INLINE state_ #-}

-- | Zoom in on the field at position @pos@ of type @v@ in the state @struct@.
instance
  -- The constraint raises @-Wsimplifiable-class-constraints@.
  -- This could be avoided by instead placing @HasPosition'@s constraints here.
  -- Unfortunately, it uses non-exported symbols from @generic-lens@.
  ( tag ~ pos, Generic.HasPosition' pos struct v, HasState oldtag struct m )
  => HasState tag v (Pos pos oldtag m)
  where
    state_ :: forall a.
      Proxy# tag
      -> (v -> (a, v))
      -> Pos pos oldtag m a
    state_ _ = coerce @((v -> (a, v)) -> m a) $
      state @oldtag . Generic.position' @pos @_ @_ @((,) a)
    {-# INLINE state_ #-}

-- | Lift one layer in a monad transformer stack.
instance (HasState tag s m, MonadTrans t, Monad (t m))
  => HasState tag s (Lift (t m))
  where
    state_ :: forall a. Proxy# tag -> (s -> (a, s)) -> Lift (t m) a
    state_ _ = coerce $ lift @t @m . state @tag @s @m @a
    {-# INLINE state_ #-}

-- | Compose two accessors.
deriving via ((t2 :: (Type -> Type) -> Type -> Type) ((t1 :: (Type -> Type) -> Type -> Type) m))
  instance
  ( forall x. Coercible (m x) (t2 (t1 m) x)
  , Monad m, HasState tag s (t2 (t1 m)) )
  => HasState tag s ((t2 :.: t1) m)

instance
  (HasReader tag (IORef s) m, MonadIO m)
  => HasState tag s (ReaderIORef m)
  where
    state_ _ f = ReaderIORef $ do
      ref <- ask @tag
      liftIO $ atomicModifyIORef' ref (swap . f)
      where
        swap (a, b) = (b, a)
    {-# INLINE state_ #-}

instance
  ( MutableRef ref, RefElement ref ~ s
  , HasReader tag ref m, PrimMonad m, PrimState m ~ MCState ref )
  => HasState tag s (ReaderRef m)
  where
    state_ _ f = ReaderRef $ do
      ref <- ask @tag
      s <- readRef ref
      let (a, s') = f s
      writeRef ref s'
      pure a
    {-# INLINE state_ #-}
