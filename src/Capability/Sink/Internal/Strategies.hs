{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

{-# OPTIONS_HADDOCK hide #-}

module Capability.Sink.Internal.Strategies
  ( SinkStack(..)
  , SinkDList(..)
  , SinkLog(..)
  ) where

import Capability.Accessors
import Capability.Source.Internal.Class
import Capability.Sink.Internal.Class
import Capability.State.Internal.Class
import Capability.State.Internal.Strategies.Common
import Control.Lens (set)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Primitive (PrimMonad)
import qualified Control.Monad.State.Class as State
import Control.Monad.Trans.Class (MonadTrans, lift)
import Data.Coerce (Coercible, coerce)
import Data.DList (DList)
import qualified Data.DList as DList
import qualified Data.Generics.Product.Fields as Generic
import qualified Data.Generics.Product.Positions as Generic
import Data.IORef
import Data.Mutable
import Streaming
import qualified Streaming.Prelude as S

-- | Accumulate sunk values in a reverse order list.
newtype SinkStack m (a :: *) = SinkStack (m a)
  deriving (Functor, Applicative, Monad, MonadIO, PrimMonad)
instance HasState tag [a] m => HasSink tag a (SinkStack m) where
  yield_ _ a = coerce @(m ()) $ modify' @tag (a:)
  {-# INLINE yield_ #-}

-- | Accumulate sunk values in forward order in a difference list.
newtype SinkDList m (a :: *) = SinkDList (m a)
  deriving (Functor, Applicative, Monad, MonadIO, PrimMonad)
-- | This instance may seem a bit odd at first. All it does is wrap each
-- 'yield'ed value in a single element difference list. How does re-yielding
-- something else constitute a strategy for implementing 'HasSink' in the
-- first place? The answer is that difference lists form a monoid, which allows
-- a second stragegy to be used which accumulates all 'yield's in a single
-- value, actually eliminating the 'HasSink' constraint this time.
--
-- 'SinkLog' below in fact does this, so the easiest way to fully eliminate
-- the 'HasSink' constraint as described above is:
--
-- > deriving (HasSink tag w) via
-- >   SinkDList (SinkLog (MonadState SomeStateMonad))
instance HasSink tag (DList a) m => HasSink tag a (SinkDList m) where
  yield_ _ = coerce @(a -> m ()) $ yield @tag . DList.singleton
  {-# INLINE yield_ #-}

-- | Accumulate sunk values with their own monoid.
newtype SinkLog m (a :: *) = SinkLog (m a)
  deriving (Functor, Applicative, Monad, MonadIO, PrimMonad)
instance (Monoid w, HasState tag w m) => HasSink tag w (SinkLog m) where
    yield_ _ w = coerce @(m ()) $ modify' @tag (<> w)
    {-# INLINE yield_ #-}

instance Monad m => HasSink tag a (S.Stream (Of a) m) where
  yield_ _ = S.yield
  {-# INLINE yield_ #-}

-- | Lift one layer in a monad transformer stack.
--
-- Note, that if the 'HasSink' instance is based on 'HasState', then it is
-- more efficient to apply 'Lift' to the underlying state capability. E.g.
-- you should favour
--
-- > deriving (HasSink tag w) via
-- >   SinkLog (Lift (SomeTrans (MonadState SomeStateMonad)))
--
-- over
--
-- > deriving (HasSink tag w) via
-- >   Lift (SomeTrans (SinkLog (MonadState SomeStateMonad)))
instance (HasSink tag a m, MonadTrans t, Monad (t m))
  => HasSink tag a (Lift (t m))
  where
    yield_ _ = coerce @(a -> t m ()) $ lift . yield @tag
    {-# INLINE yield_ #-}

-- | Compose two accessors.
deriving via ((t2 :: (* -> *) -> * -> *) ((t1 :: (* -> *) -> * -> *) m))
  instance
  ( forall x. Coercible (m x) (t2 (t1 m) x)
  , Monad m, HasSink tag a (t2 (t1 m)) )
  => HasSink tag a ((t2 :.: t1) m)

-- | Convert the state using safe coercion.
instance
  ( Coercible from to, HasSink tag from m
  , forall x y. Coercible x y => Coercible (m x) (m y) )
  => HasSink tag to (Coerce to m)
  where
    yield_ tag = coerce @(from -> m ()) $ yield_ tag
    {-# INLINE yield_ #-}

-- | Rename the tag.
instance HasSink oldtag s m => HasSink newtag s (Rename oldtag m) where
  yield_ _ = coerce @(s -> m ()) $ yield @oldtag
  {-# INLINE yield_ #-}

-- | Zoom in on the record field @field@ of type @v@ in the state @record@.
instance
  -- The constraint raises @-Wsimplifiable-class-constraints@.
  -- This could be avoided by instead placing @HasField'@s constraints here.
  -- Unfortunately, it uses non-exported symbols from @generic-lens@.
  ( tag ~ field, Generic.HasField' field record v, HasState oldtag record m )
  => HasSink tag v (Field field oldtag m)
  where
    yield_ _ = coerce @(v -> m ()) $
      modify @oldtag . set (Generic.field' @field @record)
    {-# INLINE yield_ #-}

-- | Zoom in on the field at position @pos@ of type @v@ in the state @struct@.
instance
  -- The constraint raises @-Wsimplifiable-class-constraints@.
  -- This could be avoided by instead placing @HasPosition'@s constraints here.
  -- Unfortunately, it uses non-exported symbols from @generic-lens@.
  ( tag ~ pos, Generic.HasPosition' pos struct v, HasState oldtag struct m )
  => HasSink tag v (Pos pos oldtag m)
  where
    yield_ _ = coerce @(v -> m ()) $
      modify @oldtag . set (Generic.position' @pos @struct)
    {-# INLINE yield_ #-}

--------------------------------------------------------------------------------

instance State.MonadState s m => HasSink tag s (MonadState m) where
  yield_ _ = coerce @(s -> m ()) State.put
  {-# INLINE yield_ #-}

instance
  (HasSource tag (IORef s) m, MonadIO m)
  => HasSink tag s (ReaderIORef m)
  where
    yield_ _ v = ReaderIORef $ do
      ref <- await @tag
      liftIO $ writeIORef ref v
    {-# INLINE yield_ #-}

instance
  ( MutableRef ref, RefElement ref ~ s
  , HasSource tag ref m, PrimMonad m, PrimState m ~ MCState ref )
  => HasSink tag s (ReaderRef m)
  where
    yield_ _ v = ReaderRef $ do
      ref <- await @tag
      writeRef ref v
    {-# INLINE yield_ #-}
