{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UnboxedTuples #-}

module Capability.Accessors
  ( Coerce(..)
  , Rename(..)
  , Field(..)
  , Pos(..)
  , Ctor(..)
  , Lift(..)
  ) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Primitive (PrimMonad)
import GHC.TypeLits (Nat, Symbol)

-- | Coerce the type in the context @m@ to @to@.
newtype Coerce (to :: *) m (a :: *) = Coerce (m a)
  deriving (Functor, Applicative, Monad, MonadIO, PrimMonad)

-- | Rename the tag.
newtype Rename (oldtag :: k) m (a :: *) = Rename (m a)
  deriving (Functor, Applicative, Monad, MonadIO, PrimMonad)

-- | Access the record field @field@ in the context @m@.
newtype Field (field :: Symbol) (oldtag :: k) m (a :: *) = Field (m a)
  deriving (Functor, Applicative, Monad, MonadIO, PrimMonad)

-- | Access the value at position @pos@ in the context @m@.
newtype Pos (pos :: Nat) (oldtag :: k) m (a :: *) = Pos (m a)
  deriving (Functor, Applicative, Monad, MonadIO, PrimMonad)

-- | Choose the given constructor in the sum-type in context @m@.
--
-- XXX: The @ctor@ parameter might technically be redundant. But, it might
--   still be useful for documentation purposes.
newtype Ctor (ctor :: Symbol) (oldtag :: k) m (a :: *) = Ctor (m a)
  deriving (Functor, Applicative, Monad, MonadIO, PrimMonad)

-- | Skip one level in a monad transformer stack.
--
-- Note, that instances generated with this strategy can incur a performance
-- penalty.
newtype Lift m (a :: *) = Lift (m a)
  deriving (Functor, Applicative, Monad, MonadIO, PrimMonad)
