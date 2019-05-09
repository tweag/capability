{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeInType #-}

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
    module Capability.State.Internal.Class
    -- * Strategies
  , module Capability.State.Internal.Strategies
    -- ** Modifiers
  , module Capability.Accessors
    -- * Helpers
  , module Capability.Constraints
  , module Capability.TypeOf
  , HasState'
  ) where

import Capability.Accessors
import Capability.Constraints
import Capability.State.Internal.Class
import Capability.State.Internal.Strategies
import Capability.TypeOf

-- | Type synonym using the 'TypeOf' type family to specify 'HasState'
-- constraints without having to specify the type associated to a tag.
type HasState' (tag :: k) = HasState tag (TypeOf k tag)
