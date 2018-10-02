-- | Defines a capability type class for a reader effect. A reader provides an
-- environment, say an initialization context or a configuration. The
-- environment held in the reader effect can be changed (with 'local') within
-- the scope of a sub-computation. Contrary to the "Capability.State", such
-- a change is local, and does not persist when the 'local' call ends.

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
import Capability.Reader.Internal.Strategies
