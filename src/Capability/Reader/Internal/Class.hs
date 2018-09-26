{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeInType #-}

module Capability.Reader.Internal.Class
  ( HasReader(..)
  , ask
  , asks
  , local
  , reader
  , magnify
  ) where

import Data.Coerce (Coercible, coerce)
import GHC.Exts (Proxy#, proxy#)

-- | Reader capability
--
-- An instance should fulfill the following laws.
-- At this point these laws are not definitive,
-- see <https://github.com/haskell/mtl/issues/5>.
--
-- prop> k <*> ask @t = ask @t <**> k
-- prop> ask @t *> m = m = m <* ask @t
-- prop> local @t f (ask @t) = fmap f (ask @t)
-- prop> local @t f . local @t g = local @t (g . f)
-- prop> local @t f (pure x) = pure x
-- prop> local @t f (m >>= \x -> k x) = local @t f m >>= \x -> local @t f (k x)
-- prop> reader @t f = f <$> ask @t
class Monad m
  => HasReader (tag :: k) (r :: *) (m :: * -> *) | tag m -> r
  where
    -- | For technical reasons, this method needs an extra proxy argument.
    -- You only need it if you are defining new instances of 'HasReader'.
    -- Otherwise, you will want to use 'ask'.
    -- See 'ask' for more documentation.
    ask_ :: Proxy# tag -> m r
    -- | For technical reasons, this method needs an extra proxy argument.
    -- You only need it if you are defining new instances of 'HasReader'.
    -- Otherwise, you will want to use 'local'.
    -- See 'local' for more documentation.
    local_ :: Proxy# tag -> (r -> r) -> m a -> m a
    -- | For technical reasons, this method needs an extra proxy argument.
    -- You only need it if you are defining new instances of 'HasReader'.
    -- Otherwise, you will want to use 'reader'.
    -- See 'reader' for more documentation.
    reader_ :: Proxy# tag -> (r -> a) -> m a

-- | @ask \@tag@
-- retrieves the environment of the reader capability @tag@.
ask :: forall tag r m. HasReader tag r m => m r
ask = ask_ (proxy# @_ @tag)
{-# INLINE ask #-}

-- | @asks \@tag@
-- retrieves the image by @f@ of the environment
-- of the reader capability @tag@.
--
-- prop> asks @tag f = f <$> ask @tag
asks :: forall tag r m a. HasReader tag r m => (r -> a) -> m a
asks f = f <$> ask @tag
{-# INLINE asks #-}

-- | @local \@tag f m@
-- runs the monadic action @m@ in a modified environment @e' = f e@,
-- where @e@ is the environment of the reader capability @tag@.
-- Symbolically: @return e = ask \@tag@.
local :: forall tag r m a. HasReader tag r m => (r -> r) -> m a -> m a
local = local_ (proxy# @_ @tag)
{-# INLINE local #-}

-- | @reader \@tag act@
-- lifts a purely environment-dependent action @act@ to a monadic action
-- in an arbitrary monad @m@ with capability @HasReader@.
--
-- It happens to coincide with @asks@: @reader = asks@.
reader :: forall tag r m a. HasReader tag r m => (r -> a) -> m a
reader = reader_ (proxy# @_ @tag)
{-# INLINE reader #-}

-- | Execute the given reader action on a sub-component of the current context
-- as defined by the given transformer @t@.
--
-- See 'Capability.State.zoom'.
--
-- This function is experimental and subject to change.
-- See <https://github.com/tweag/capability/issues/46>.
magnify :: forall outertag innertag t outer inner m a.
  ( forall x. Coercible (t m x) (m x)
  , forall m'. HasReader outertag outer m'
    => HasReader innertag inner (t m')
  , HasReader outertag outer m )
  => (forall m'. HasReader innertag inner m' => m' a) -> m a
magnify m = coerce @(t m a) m
{-# INLINE magnify #-}
