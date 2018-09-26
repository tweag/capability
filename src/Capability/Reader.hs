-- | Defines a capability for computations that read values
-- from a shared environment. I.e. the well known reader monad.
--
-- Description taken from Mtl
-- <http://hackage.haskell.org/package/mtl-2.2.2/docs/Control-Monad-Reader-Class.html>
--
-- The Reader monad (also called the Environment monad).
-- Represents a computation, which can read values from a shared environment,
-- pass values from function to function, and execute sub-computations in a
-- modified environment. Using Reader monad for such computations is often
-- clearer and easier than using the State monad.
--
-- Inspired by the paper Functional Programming with Overloading and
-- Higher-Order Polymorphism, Mark P Jones (<http://web.cecs.pdx.edu/~mpj/>)
-- Advanced School of Functional Programming, 1995.

module Capability.Reader
  ( -- * Interface
    HasReader(..)
  , ask
  , asks
  , local
  , reader
  , magnify
    -- * Strategies
  , MonadReader(..)
  , ReadStatePure(..)
  , ReadState(..)
    -- ** Modifiers
  , module Capability.Accessors
  ) where

import Capability.Accessors
import Capability.Reader.Internal.Class
import Capability.Reader.Internal.Instances
