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

{-# OPTIONS_HADDOCK hide #-}

module Capability.Source.Internal.Strategies
  ( MonadReader(..)
  , ReadStatePure(..)
  , ReadState(..)
  , MonadState(..)
  , ReaderIORef(..)
  , ReaderRef(..)
  ) where

import Capability.Source.Internal.Class
import Capability.State.Internal.Class
import Capability.State.Internal.Strategies.Common
import Capability.Accessors
import Control.Lens (view)
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Primitive (PrimMonad)
import qualified Control.Monad.Reader.Class as Reader
import qualified Control.Monad.State.Class as State
import Control.Monad.Trans.Class (MonadTrans, lift)
import Data.Coerce (Coercible, coerce)
import Data.IORef
import Data.Kind (Type)
import Data.Mutable
import qualified Data.Generics.Product.Fields as Generic
import qualified Data.Generics.Product.Positions as Generic

--------------------------------------------------------------------------------

-- | Derive 'HasSource' from @m@'s 'Control.Monad.Reader.Class.MonadReader'
-- instance.
newtype MonadReader (m :: Type -> Type) (a :: Type) = MonadReader (m a)
  deriving (Functor, Applicative, Monad, MonadIO, PrimMonad)

instance Reader.MonadReader r m => HasSource tag r (MonadReader m) where
  await_ _ = coerce @(m r) Reader.ask
  {-# INLINE await_ #-}

-- | Convert a /pure/ state monad into a reader monad.
--
-- /Pure/ meaning that the monad stack does not allow catching exceptions.
-- Otherwise, an exception occurring in the action passed to 'local' could cause
-- the context to remain modified outside of the call to 'local'. E.g.
--
-- > local @tag (const r') (throw MyException)
-- > `catch` \MyException -> ask @tag
--
-- returns @r'@ instead of the previous value.
--
-- Note, that no @MonadIO@ instance is provided, as this would allow catching
-- exceptions.
--
-- See 'ReadState'.
newtype ReadStatePure (m :: Type -> Type) (a :: Type) = ReadStatePure (m a)
  deriving (Functor, Applicative, Monad)

instance HasState tag r m => HasSource tag r (ReadStatePure m) where
  await_ _ = coerce @(m r) $ get @tag
  {-# INLINE await_ #-}

-- | Convert a state monad into a reader monad.
--
-- Use this if the monad stack allows catching exceptions.
--
-- See 'ReadStatePure'.
newtype ReadState (m :: Type -> Type) (a :: Type) = ReadState (m a)
  deriving (Functor, Applicative, Monad, MonadIO, PrimMonad)

instance
  (HasState tag r m, MonadMask m)
  => HasSource tag r (ReadState m)
  where
    await_ _ = coerce @(m r) $ get @tag
    {-# INLINE await_ #-}

instance
  ( tag ~ pos, Generic.HasPosition' pos struct v, HasSource oldtag struct m )
  => HasSource tag v (Pos pos oldtag m)
  where
    await_ _ = coerce @(m v) $
      awaits @oldtag $ view (Generic.position' @pos)
    {-# INLINE await_ #-}

deriving via ((t2 :: (Type -> Type) -> Type -> Type) ((t1 :: (Type -> Type) -> Type -> Type) m))
  instance
  ( forall x. Coercible (m x) (t2 (t1 m) x)
  , Monad m, HasSource tag r (t2 (t1 m)) )
  => HasSource tag r ((t2 :.: t1) m)

instance
  ( Coercible from to, HasSource tag from m
  , forall x y. Coercible x y => Coercible (m x) (m y) )
  => HasSource tag to (Coerce to m)
  where
    await_ tag = coerce @(m from) $ await_ tag
    {-# INLINE await_ #-}

-- | Rename the tag.
instance HasSource oldtag r m => HasSource newtag r (Rename oldtag m) where
  await_ _ = coerce @(m r) $ await @oldtag
  {-# INLINE await_ #-}

instance
  ( tag ~ field, Generic.HasField' field record v, HasSource oldtag record m )
  => HasSource tag v (Field field oldtag m)
  where
    await_ _ = coerce @(m v) $
      awaits @oldtag $ view (Generic.field' @field)
    {-# INLINE await_ #-}

instance (HasSource tag r m, MonadTrans t, Monad (t m))
  => HasSource tag r (Lift (t m))
  where
    await_ _ = coerce $ lift @t @m $ await @tag @r
    {-# INLINE await_ #-}

--------------------------------------------------------------------------------

instance State.MonadState s m => HasSource tag s (MonadState m) where
  await_ _ = coerce @(m s) State.get
  {-# INLINE await_ #-}

instance
  (HasSource tag (IORef s) m, MonadIO m)
  => HasSource tag s (ReaderIORef m)
  where
    await_ _ = ReaderIORef $ do
      ref <- await @tag
      liftIO $ readIORef ref
    {-# INLINE await_ #-}

instance
  ( MutableRef ref, RefElement ref ~ s
  , HasSource tag ref m, PrimMonad m, PrimState m ~ MCState ref )
  => HasSource tag s (ReaderRef m)
  where
    await_ _ = ReaderRef $ do
      ref <- await @tag
      readRef ref
    {-# INLINE await_ #-}
