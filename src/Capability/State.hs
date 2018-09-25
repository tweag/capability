-- | Defines a capability for computations acting on a mutable state.
-- I.e. the well known state monad.
--
-- Description taken from Mtl
-- <http://hackage.haskell.org/package/mtl-2.2.2/docs/Control-Monad-State.html>
--
-- This module is inspired by the paper Functional Programming with Overloading and Higher-Order Polymorphism, Mark P Jones (<http://web.cecs.pdx.edu/~mpj/>) Advanced School of Functional Programming, 1995.

module Capability.State
  ( -- * Interface
    HasState(..)
  , get
  , put
  , state
  , modify
  , modify'
  , gets
  , zoom
    -- * Strategies
  , MonadState(..)
  , ReaderIORef(..)
  , ReaderRef(..)
    -- ** Modifiers
  , module Capability.Accessors
  ) where

import Capability.Accessors
import Capability.State.Internal.Class
import Capability.State.Internal.Instances
