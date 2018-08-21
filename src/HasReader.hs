module HasReader
  ( -- * Interface
    HasReader (..)
  , ask
  , asks
  , local
  , reader
    -- * Strategies
  , MonadReader (..)
  , ReadStatePure (..)
  , ReadState (..)
    -- ** Modifiers
  , module Accessors
  ) where

import Accessors
import HasReader.Internal.Class
import HasReader.Internal.Instances
