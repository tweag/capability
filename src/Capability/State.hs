-- | Defines a capability type class for a state effect. A state capability
-- provides a state which can be retrieved with 'get' and set with 'put'. As an
-- analogy, each state capability is equivalent to making one @IORef@ available
-- in an @IO@ computation (except, of course, that a state capability does not
-- have to be provided by @IO@).
--
-- This is a very expressive capability. It is often preferable to restrict to
-- less powerful capabilities such as "Capability.Reader", "Capability.Writer",
-- or "Capability.Stream".

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
