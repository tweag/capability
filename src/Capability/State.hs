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
