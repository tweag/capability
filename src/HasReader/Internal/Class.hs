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

module HasReader.Internal.Class
  ( HasReader (..)
  , ask
  , asks
  , local
  , reader
  , magnify
  ) where

import Data.Coerce (Coercible, coerce)
import GHC.Exts (Proxy#, proxy#)


class Monad m
  => HasReader (tag :: k) (r :: *) (m :: * -> *) | tag m -> r
  where
    ask_ :: Proxy# tag -> m r
    local_ :: Proxy# tag -> (r -> r) -> m a -> m a
    reader_ :: Proxy# tag -> (r -> a) -> m a

ask :: forall tag r m. HasReader tag r m => m r
ask = ask_ (proxy# @_ @tag)
{-# INLINE ask #-}

asks :: forall tag r m a. HasReader tag r m => (r -> a) -> m a
asks f = f <$> ask @tag
{-# INLINE asks #-}

local :: forall tag r m a. HasReader tag r m => (r -> r) -> m a -> m a
local = local_ (proxy# @_ @tag)
{-# INLINE local #-}

reader :: forall tag r m a. HasReader tag r m => (r -> a) -> m a
reader = reader_ (proxy# @_ @tag)
{-# INLINE reader #-}

-- | Execute the given reader action on a sub-component of the current context
-- as defined by the given transformer @t@.
--
-- See 'HasState.zoom'.
magnify :: forall outertag innertag t outer inner m a.
  ( forall x. Coercible (t m x) (m x)
  , forall m'. HasReader outertag outer m'
    => HasReader innertag inner (t m')
  , HasReader outertag outer m )
  => (forall m'. HasReader innertag inner m' => m' a) -> m a
magnify m = coerce @(t m a) m
{-# INLINE magnify #-}
