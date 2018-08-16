{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UnboxedTuples #-}

module Accessors
  ( Coerce (..)
  , Field (..)
  , Pos (..)
  , Ctor (..)
  , Lift (..)
  ) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Primitive (PrimMonad)
import GHC.TypeLits (Nat, Symbol)


-- | Coerce the type in the context @m@ to @to@.
newtype Coerce to m a = Coerce (m a)
  deriving (Functor, Applicative, Monad, MonadIO, PrimMonad)


-- | Access the record field @field@ in the context @m@.
newtype Field (field :: Symbol) m a = Field (m a)
  deriving (Functor, Applicative, Monad, MonadIO, PrimMonad)


-- | Access the value at position @pos@ in the context @m@.
newtype Pos (pos :: Nat) m a = Pos (m a)
  deriving (Functor, Applicative, Monad, MonadIO, PrimMonad)


-- | Choose the given constructor in the sum-type in context @m@.
newtype Ctor (ctor :: Symbol) m a = Ctor (m a)
  deriving (Functor, Applicative, Monad, MonadIO, PrimMonad)


-- | Skip one level in a monad transformer stack.
--
-- Note, that instances generated with this strategy can incur a performance
-- penalty.
newtype Lift m a = Lift (m a)
  deriving (Functor, Applicative, Monad, MonadIO, PrimMonad)
