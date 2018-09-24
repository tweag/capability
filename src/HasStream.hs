{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module HasStream
  ( HasStream(..)
  , yield
  , StreamStack(..)
  , StreamDList(..)
  ) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Primitive (PrimMonad)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Data.Coerce (coerce)
import Data.DList (DList)
import qualified Data.DList as DList
import GHC.Exts (Proxy#, proxy#)
import Streaming
import qualified Streaming.Prelude as S

import HasState
import HasWriter

-- | Streaming capability.
class Monad m
  => HasStream (tag :: k) (a :: *) (m :: * -> *) | tag m -> a
  where
    -- | Use 'yield' instead.
    yield_ :: Proxy# tag -> a -> m ()

-- | Emit the given value.
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
