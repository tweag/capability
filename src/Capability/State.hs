module Capability.State
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
import Capability.State.Internal.Class
import Capability.State.Internal.Instances
