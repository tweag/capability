-- | Defines a capability for computations that produce a stream of values
-- as part of their execution.
--
-- Programs producing streams of data are common. Examples: emitting events on
-- input, or emitting events whenever certain conditions are observed. streams
-- are similar to Python generators.
--
-- The 'HasSink' capability enables separating the logic responsible for
-- emitting events from that responsible for collecting or handling them.
-- The name is because a sink is needed to consume the locally produced stream.
--
-- This can be thought of as a writer capability of a list of values @HasWriter
-- tag [v]@ with @\\x -> tell \@tag [x]@ as a primitive operation. However, that
-- implementation would be inefficient.
--
-- For example using the 'Streaming.Prelude.Stream' instance, a producer defined
-- using this capability can be consumed efficiently in a streaming fashion.

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PolyKinds #-}

module Capability.Sink
  ( -- * Interface
    module Capability.Sink.Internal.Class
    -- * Strategies
  , module Capability.Sink.Internal.Strategies
    -- ** Modifiers
  , module Capability.Accessors
    -- * Helpers
  , module Capability.Constraints
  , module Capability.TypeOf
  , HasSink'
  ) where

import Capability.Sink.Internal.Class
import Capability.Sink.Internal.Strategies
import Capability.Accessors
import Capability.Constraints
import Capability.TypeOf

-- | Type synonym using the 'TypeOf' type family to specify 'HasSink'
-- constraints without having to specify the type associated to a tag.
type HasSink' (tag :: k) = HasSink tag (TypeOf k tag)
