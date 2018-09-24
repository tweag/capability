{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

module HasState.Internal.Instances
  ( MonadState(..)
  , ReaderIORef(..)
  , ReaderRef(..)
  ) where

import Control.Lens (set, view)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Primitive (PrimMonad)
import qualified Control.Monad.State.Class as State
import Control.Monad.Trans.Class (MonadTrans, lift)
import Data.Coerce (Coercible, coerce)
import qualified Data.Generics.Product.Fields as Generic
import qualified Data.Generics.Product.Positions as Generic
import Data.IORef
import Data.Mutable
import GHC.Exts (Proxy#)

import Accessors
import HasReader.Internal.Class
import HasState.Internal.Class

-- | Derive 'HasState' from @m@'s
-- 'Control.Monad.State.Class.MonadState' instance.
newtype MonadState (m :: * -> *) (a :: *) = MonadState (m a)
  deriving (Functor, Applicative, Monad, MonadIO, PrimMonad)

instance State.MonadState s m => HasState tag s (MonadState m) where
  get_ _ = coerce @(m s) State.get
  {-# INLINE get_ #-}
  put_ _ = coerce @(s -> m ()) State.put
  {-# INLINE put_ #-}
  state_ :: forall a. Proxy# tag -> (s -> (a, s)) -> MonadState m a
  state_ _ = coerce @((s -> (a, s)) -> m a) State.state
  {-# INLINE state_ #-}

-- | Convert the state using safe coercion.
instance
  ( Coercible from to, HasState tag from m
  , forall x y. Coercible x y => Coercible (m x) (m y) )
  => HasState tag to (Coerce to m)
  where
    get_ tag = coerce @(m from) $ get_ tag
    {-# INLINE get_ #-}
    put_ tag = coerce @(from -> m ()) $ put_ tag
    {-# INLINE put_ #-}
    state_ :: forall a. Proxy# tag -> (to -> (a, to)) -> Coerce to m a
    state_ tag = coerce @((from -> (a, from)) -> m a) $ state_ tag
    {-# INLINE state_ #-}

-- | Rename the tag.
instance HasState oldtag s m => HasState newtag s (Rename oldtag m) where
  get_ _ = coerce @(m s) $ get @oldtag
  {-# INLINE get_ #-}
  put_ _ = coerce @(s -> m ()) $ put @oldtag
  {-# INLINE put_ #-}
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
    get_ _ = coerce @(m v) $
      gets @oldtag $ view (Generic.field' @field)
    {-# INLINE get_ #-}
    put_ _ = coerce @(v -> m ()) $
      modify @oldtag . set (Generic.field' @field @record)
    {-# INLINE put_ #-}
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
    get_ _ = coerce @(m v) $
      gets @oldtag $ view (Generic.position' @pos)
    {-# INLINE get_ #-}
    put_ _ = coerce @(v -> m ()) $
      modify @oldtag . set (Generic.position' @pos @struct)
    {-# INLINE put_ #-}
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
    get_ _ = coerce $ lift @t @m $ get @tag @s
    {-# INLINE get_ #-}
    put_ _ = coerce $ lift @t @m . put @tag @s
    {-# INLINE put_ #-}
    state_ :: forall a. Proxy# tag -> (s -> (a, s)) -> Lift (t m) a
    state_ _ = coerce $ lift @t @m . state @tag @s @m @a
    {-# INLINE state_ #-}

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
    {-# INLINE get_ #-}
    put_ _ v = ReaderIORef $ do
      ref <- ask @tag
      liftIO $ writeIORef ref v
    {-# INLINE put_ #-}
    state_ _ f = ReaderIORef $ do
      ref <- ask @tag
      liftIO $ atomicModifyIORef' ref (swap . f)
      where
        swap (a, b) = (b, a)
    {-# INLINE state_ #-}

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
    {-# INLINE get_ #-}
    put_ _ v = ReaderRef $ do
      ref <- ask @tag
      writeRef ref v
    {-# INLINE put_ #-}
    state_ _ f = ReaderRef $ do
      ref <- ask @tag
      s <- readRef ref
      let (a, s') = f s
      writeRef ref s'
      pure a
    {-# INLINE state_ #-}
