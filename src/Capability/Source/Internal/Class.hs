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

{-# OPTIONS_HADDOCK hide #-}

module Capability.Source.Internal.Class where

import GHC.Exts (Proxy#, proxy#)

-- | Sourcing capability.
--
-- An instance does not need to fulfill any additional laws
-- besides the monad laws.
class Monad m
  => HasSource (tag :: k) (a :: *) (m :: * -> *) | tag m -> a
  where
    -- | For technical reasons, this method needs an extra proxy argument.
    -- You only need it if you are defining new instances of 'HasSource'.
    -- Otherwise, you will want to use 'await'.
    -- See 'await' for more documentation.
    await_ :: Proxy# tag -> m a

-- | @await \@tag a@
-- takes @a@ from the source capability @tag@.
await :: forall tag a m. HasSource tag a m => m a
await = await_ (proxy# @_ @tag)
{-# INLINE await #-}

-- | @awaits \@tag@
-- retrieves the image by @f@ of the environment
-- of the reader capability @tag@.
--
-- prop> awaits @tag f = f <$> await @tag
awaits :: forall tag r m a. HasSource tag r m => (r -> a) -> m a
awaits f = f <$> await @tag
{-# INLINE awaits #-}
