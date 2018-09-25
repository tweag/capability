-- | Defines a capability for computations that produce a stream of values
-- as part of their execution.
--
-- This can be thought of as a writer capability of a list of values
-- @HasWriter tag [v]@ with @\\x -> tell \@tag [x]@ as primitive operation.
-- However, that implementation would be inefficient.
--
-- For example using the 'Streaming.Prelude.Stream' instance a producer
-- defined using this capability can be consumed efficiently
-- in a streaming fashion.

{-# LANGUAGE AllowAmbiguousTypes #-}
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
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Capability.Stream
  ( -- * Interface
    HasStream(..)
  , yield
    -- * Strategies
  , StreamStack(..)
  , StreamDList(..)
    -- ** Modifiers
  , module Capability.Accessors
  ) where

import Capability.Accessors
import Capability.State
import Capability.Writer
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Primitive (PrimMonad)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Data.Coerce (Coercible, coerce)
import Data.DList (DList)
import qualified Data.DList as DList
import GHC.Exts (Proxy#, proxy#)
import Streaming
import qualified Streaming.Prelude as S

-- | Streaming capability.
--
-- An instance does not need to fulfill any additional laws
-- besides the monad laws.
class Monad m
  => HasStream (tag :: k) (a :: *) (m :: * -> *) | tag m -> a
  where
    -- | For technical reasons, this method needs an extra proxy argument.
    -- You only need it if you are defining new instances of 'HasReader'.
    -- Otherwise, you will want to use 'yield'.
    -- See 'yield' for more documentation.
    yield_ :: Proxy# tag -> a -> m ()

-- | @yield \@tag a@
-- emits @a@ in the stream capability @tag@.
yield :: forall tag a m. HasStream tag a m => a -> m ()
yield = yield_ (proxy# @_ @tag)
{-# INLINE yield #-}

-- | Accumulate streamed values in a reverse order list.
newtype StreamStack m (a :: *) = StreamStack (m a)
  deriving (Functor, Applicative, Monad, MonadIO, PrimMonad)
instance HasState tag [a] m => HasStream tag a (StreamStack m) where
  yield_ _ a = coerce @(m ()) $ modify' @tag (a:)
  {-# INLINE yield_ #-}

-- | Accumulate streamed values in forward order in a difference list.
newtype StreamDList m (a :: *) = StreamDList (m a)
  deriving (Functor, Applicative, Monad, MonadIO, PrimMonad)
instance HasWriter tag (DList a) m => HasStream tag a (StreamDList m) where
  yield_ _ = coerce @(a -> m ()) $ tell @tag . DList.singleton
  {-# INLINE yield_ #-}

instance Monad m => HasStream tag a (S.Stream (Of a) m) where
  yield_ _ = S.yield
  {-# INLINE yield_ #-}

-- | Lift one layer in a monad transformer stack.
instance (HasStream tag a m, MonadTrans t, Monad (t m))
  => HasStream tag a (Lift (t m))
  where
    yield_ _ = coerce @(a -> t m ()) $ lift . yield @tag
    {-# INLINE yield_ #-}


-- | Compose two accessors.
deriving via ((t2 :: (* -> *) -> * -> *) ((t1 :: (* -> *) -> * -> *) m))
  instance
  ( forall x. Coercible (m x) (t2 (t1 m) x)
  , Monad m, HasStream tag a (t2 (t1 m)) )
  => HasStream tag a ((t2 :.: t1) m)
