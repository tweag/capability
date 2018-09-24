module Capability.HasReader
  ( HasReader(..)
  , ask
  , asks
  , local
  , reader
  , MonadReader(..)
  , ReadStatePure(..)
  , ReadState(..)
  , module Capability.Accessors
  ) where

import Capability.Accessors
import Capability.HasReader.Internal.Class
import Capability.HasReader.Internal.Instances
