module Capability.HasState
  ( HasState(..)
  , get
  , put
  , state
  , modify
  , modify'
  , gets
  , MonadState(..)
  , ReaderIORef(..)
  , ReaderRef(..)
  , module Capability.Accessors
  ) where

import Capability.Accessors
import Capability.HasState.Internal.Class
import Capability.HasState.Internal.Instances
