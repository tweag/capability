{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Capability.Context where

import Capability.Constraints
import Data.Coerce (Coercible)
import Unsafe.Coerce (unsafeCoerce)

-- | @'context' \@t \@derived \@ambient act@ is used to run @act@ when the
-- capabilities required by @act@ are not necessarily the same as those
-- available.
--
-- @'context' \@t \@derived \@ambient act@ runs @act@ by providing both the
-- capabilities in @derived@ and @ambient@. The difference is that @ambient@
-- capabilities are assumed to be available, whereas @derived@ instances are
-- provided by @t@.
--
-- 'context' assumes that @t@ is a newtype defined in the form:
--
-- @
-- newtype T m a = T (m a)
-- @
--
-- Then 'context' uses type-class instances for `T` to provide for each of the
-- capability in @derived@.
--
-- See 'Capability.Error.wrapError' for an example.
--
-- The @context@ function is experimental and is subject to change.
context ::
  forall t (derived :: [Capability]) (ambient :: [Capability]) m a.
  ( forall x. Coercible (t m x) (m x)
  , All derived (t m)
  , All ambient m)
  => (forall m'. (All derived m', All ambient m') => m' a) -> m a
context action =
  let tmDict = Dict @(All derived (t m))
      mDict =
        -- Note: this use of 'unsafeCoerce' should be safe thanks the Coercible
        -- constraint between 'm x' and 't m x'. However, dictionaries
        -- themselves aren't coercible since the type role of 'c' in 'Dict c' is
        -- nominal.
        unsafeCoerce @_ @(Dict (All derived m)) tmDict in
  case mDict of
    Dict -> action
{-# INLINE context #-}
