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
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_HADDOCK hide #-}

module Capability.Sink.Internal.Class where

import Capability.Reflection
import Data.Coerce (coerce)
import GHC.Exts (Proxy#, proxy#)

-- | Sinking capability.
--
-- An instance does not need to fulfill any additional laws
-- besides the monad laws.
class Monad m
  => HasSink (tag :: k) (a :: *) (m :: * -> *) | tag m -> a
  where
    -- | For technical reasons, this method needs an extra proxy argument.
    -- You only need it if you are defining new instances of 'HasSink'.
    -- Otherwise, you will want to use 'yield'.
    -- See 'yield' for more documentation.
    yield_ :: Proxy# tag -> a -> m ()

-- | @yield \@tag a@
-- emits @a@ in the sink capability @tag@.
yield :: forall tag a m. HasSink tag a m => a -> m ()
yield = yield_ (proxy# @tag)
{-# INLINE yield #-}

--------------------------------------------------------------------------------

data instance Reified tag (HasSink tag a) m = ReifiedSink {_yield :: a -> m ()}

instance
  ( Monad m,
    Reifies s (Reified tag (HasSink tag a) m)
  ) =>
  HasSink tag a (Reflected s (HasSink tag a) m)
  where
  yield_ _ = coerce $ _yield (reified @s)
  {-# INLINE yield_ #-}
