{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Accessors
  ( Coerce (..)
  , Rename (..)
  , Field (..)
  , Pos (..)
  , Ctor (..)
  , Lift (..)
  , (:.:) (..)
  , Via
  , Combine (..)
  , access
  ) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Primitive (PrimMonad)
import Data.Coerce (coerce)
import GHC.Exts (Constraint)
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


-- | Compose two accessors.
--
-- This is not necessary in deriving via clauses, but in places where a
-- transformer is expected as a type argument. E.g. 'HasError.wrapError'.
newtype (:.:)
  (t2 :: (* -> *) -> * -> *)
  (t1 :: (* -> *) -> * -> *)
  (m :: * -> *)
  (a :: *)
  = (:.:) (m a)
  deriving (Functor, Applicative, Monad, MonadIO, PrimMonad)
infixr 9 :.:


data Via
  (capability :: (* -> *) -> Constraint)
  (strategy :: (* -> *) -> * -> *)
infix 8 `Via`


newtype Combine (vias :: [*]) m (a :: *) = Combine (m a)
  deriving (Functor, Applicative, Monad, MonadIO, PrimMonad)


access :: forall vias m a.
  AllCapabilities vias (Combine vias m)
  => (forall m'. AllCapabilities vias m' => m' a)
  -> m a
access m = coerce @(Combine vias m a) m
{-# INLINE access #-}


class AllCapabilitiesF vias m => AllCapabilities vias m
instance AllCapabilitiesF vias m => AllCapabilities vias m
type family AllCapabilitiesF (vias :: [*]) (m :: * -> *) :: Constraint
  where
    AllCapabilitiesF '[] _ = ()
    AllCapabilitiesF '[Via c _] m = (c m)
    AllCapabilitiesF (Via c _ ': vias) m = (c m, AllCapabilitiesF vias m)
