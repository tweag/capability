{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeInType #-}

-- | Defines a capability type class for a reader effect. A reader provides an
-- environment, say an initialization context or a configuration. The
-- environment held in the reader effect can be changed (with 'local') within
-- the scope of a sub-computation. Contrary to the "Capability.State", such
-- a change is local, and does not persist when the 'local' call ends.

module Capability.Reader
  ( -- * Interface
    module Capability.Reader.Internal.Class
    -- * Strategies
  , module Capability.Reader.Internal.Strategies
    -- ** Modifiers
  , module Capability.Accessors
  , module Capability.TypeOf
  , HasReader'
  ) where

import Capability.Accessors
import Capability.Reader.Internal.Class
import Capability.Reader.Internal.Strategies
import Capability.TypeOf

-- | Type synonym using the 'TypeOf' type family to specify 'HasReader'
-- constraints without having to specify the type associated to a tag.
type HasReader' (tag :: k) = HasReader tag (TypeOf k tag)
