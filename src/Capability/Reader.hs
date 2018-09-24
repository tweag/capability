module Capability.Reader
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
import Capability.Reader.Internal.Class
import Capability.Reader.Internal.Instances
