{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}

-- | This module defines helper types and type families for working with sets of
-- capabilities.

module Capability.Constraints
  ( All
  , Capability
  , Constraint
  , Dict(..)
  ) where

import Data.Constraint (Dict(..))
import Data.Kind (Constraint, Type)

-- | A 'Capability' takes a type constructor @Type -> Type@ (e.g., a monad) and
-- returns a 'Constraint'. Examples of capabilities includ: @HasReader "foo"
-- Int@, @MonadIO@, …
type Capability = (Type -> Type) -> Constraint

-- | Type family used used to express a conjunction of constraints over a single
-- type.
--
-- Examples:
--
-- > All '[Num, Eq] Int
-- >   -- Equivalent to: (Num Int, Eq Int)
-- >
-- > All '[HasReader "foo" Int, HasSink "bar" Float] m
-- >   -- Equivalent to: (HasReader "foo" Int m, HasSink "bar" Float m)
type family All (xs :: [k -> Constraint]) a :: Constraint where
  All '[] a = ()
  All (x ':xs) a = (x a, All xs a)
