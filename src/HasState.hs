module HasState
  ( -- * Interface
    HasState (..)
  , get
  , put
  , state
  , modify
  , modify'
  , gets
    -- * Strategies
  , MonadState (..)
  , ReaderIORef (..)
  , ReaderRef (..)
    -- ** Modifiers
  , module Accessors
  ) where

import Accessors
import HasState.Internal.Class
import HasState.Internal.Instances
