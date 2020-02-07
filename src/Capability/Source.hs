-- | Defines a capability for computations that consume a stream of values
-- as part of their execution.
--
-- Programs comsuming streams of data are common. Examples: rolling up input
-- events. Sources are similar to Python generators.
--
-- This can be thought of as a reader capability where there's no guarantee that
-- one reads the same value each time.
--
-- The 'HasSource' capability enables separating the logic responsible for
-- emitting events from that responsible for collecting or handling them.
-- The name is because a source is needed to produce the locally consumed stream.
--

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module Capability.Source
  ( -- * Interface
    module Capability.Source.Internal.Class
    -- * Strategies
  , module Capability.Source.Internal.Strategies
    -- ** Modifiers
  , module Capability.Accessors
    -- * Helpers
  , module Capability.TypeOf
  , HasSource'
  ) where

import Capability.Accessors
import Capability.Source.Internal.Class
import Capability.Source.Internal.Strategies
import Capability.TypeOf

-- | Type synonym using the 'TypeOf' type family to specify 'HasSource'
-- constraints without having to specify the type associated to a tag.
type HasSource' (tag :: k) = HasSource tag (TypeOf k tag)
