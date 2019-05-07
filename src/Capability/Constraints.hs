{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}

-- | This module define helper types and type families for working with
-- constraints.

module Capability.Constraints
  ( All
  , Constraint
  , Dict(..)
  , None
  ) where

import Data.Kind (Constraint)

-- | Evidence of a constraint that can be passed around.
data Dict (c :: Constraint) = c => Dict

-- | Type family used used to apply a list of constraints or capabilities to a single type.
--
-- Examples:
--
-- > All '[Num, Eq] Int
-- >   -- Equivalent to: (Num Int, Eq Int)
-- >
-- > All '[HasReader "foo" Int, HasStram "bar" Float] m
-- >   -- Equivalent to: (Num Int, Eq Int)
type family All (xs :: [k -> Constraint]) a :: Constraint where
  All '[] a = ()
  All (x ':xs) a = (x a, All xs a)

type None = ('[] :: [k -> Constraint])
