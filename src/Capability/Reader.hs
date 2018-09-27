-- | This module defines a capability type class for reader effect (like your
-- good old monad reader). A reader can provide, for instance, an environment,
-- or a configuration. The value held in the reader effect can be changed (with
-- 'local') for the sake of a sub-computation. Contrary to the
-- "Capability.State", such a change is local, and does not persist when the
-- 'local' call.

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
