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
{-# LANGUAGE UndecidableInstances #-}

module HasStream
  ( HasStream (..)
  , yield
  , StreamStack (..)
  ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Coerce (coerce)
import GHC.Exts (Proxy#, proxy#)
import Streaming
import qualified Streaming.Prelude as S

import HasState


-- | Streaming capability.
class Monad m
  => HasStream (tag :: k) (a :: *) (m :: * -> *) | tag m -> a
  where
    -- | Use 'yield' instead.
    yield_ :: Proxy# tag -> a -> m ()

-- | Stream the given value.
yield :: forall tag a m. HasStream tag a m => a -> m ()
yield = yield_ (proxy# @_ @tag)


-- | Accumulate streamed values in a reverse order list.
newtype StreamStack m (a :: *) = StreamStack (m a)
  deriving (Functor, Applicative, Monad, MonadIO)
instance HasState tag [a] m => HasStream tag a (StreamStack m) where
  yield_ _ a = coerce @(m ()) $ modify' @tag (a:)

-- XXX: Consider adding a strategy for @HasState tag (DList a)@ as well.
--   It allows to avoid the final reverse, if the final list should have
--   forward order.


instance Monad m => HasStream tag a (S.Stream (Of a) m) where
  yield_ _ = S.yield
