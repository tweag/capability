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
