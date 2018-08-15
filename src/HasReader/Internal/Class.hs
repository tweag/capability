{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
  ) where

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
