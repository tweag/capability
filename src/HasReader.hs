{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module HasReader
  ( HasReader (..)
  , ask
  , asks
  , local
  , reader
  , MonadReader (..)
  ) where

import Control.Lens (over, view)
import Control.Monad.IO.Class (MonadIO)
import qualified Control.Monad.Reader.Class as Reader
import Data.Coerce (coerce)
import qualified Data.Generics.Product.Fields as Generic
import GHC.Exts (Proxy#, proxy#)
import GHC.Generics (Generic)

import Accessors


class Monad m
  => HasReader (tag :: k) (r :: *) (m :: * -> *) | tag m -> r
  where
    ask_ :: Proxy# tag -> m r
    local_ :: Proxy# tag -> (r -> r) -> m a -> m a
    reader_ :: Proxy# tag -> (r -> a) -> m a

ask :: forall tag r m. HasReader tag r m => m r
ask = ask_ (proxy# @_ @tag)

asks :: forall tag r m a. HasReader tag r m => (r -> a) -> m a
asks f = f <$> ask @tag

local :: forall tag r m a. HasReader tag r m => (r -> r) -> m a -> m a
local = local_ (proxy# @_ @tag)

reader :: forall tag r m a. HasReader tag r m => (r -> a) -> m a
reader = reader_ (proxy# @_ @tag)


newtype MonadReader (m :: * -> *) (a :: *) = MonadReader (m a)
  deriving (Functor, Applicative, Monad, MonadIO)
instance Reader.MonadReader r m => HasReader tag r (MonadReader m) where
  ask_ _ = coerce @(m r) Reader.ask
  local_
    :: forall a. Proxy# tag -> (r -> r) -> MonadReader m a -> MonadReader m a
  local_ _ = coerce @((r -> r) -> m a -> m a) Reader.local
  reader_ :: forall a. Proxy# tag -> (r -> a) -> MonadReader m a
  reader_ _ = coerce @((r -> a) -> m a) Reader.reader


-- The constraint raises @-Wsimplifiable-class-constraints@.
-- This could be avoided by instead placing @HasField'@s constraints here.
-- Unfortunately, it uses non-exported symbols from @generic-lens@.
instance
  ( Generic r', Generic.HasField' field r' r, HasReader tag r' m )
  => HasReader tag r (Field field m)
  where
    ask_ _ = coerce @(m r) $
      asks @tag $ view (Generic.field' @field)
    local_
      :: forall a. Proxy# tag -> (r -> r) -> Field field m a -> Field field m a
    local_ tag = coerce @((r -> r) -> m a -> m a) $
      local_ tag . over (Generic.field' @field)
    reader_ :: forall a. Proxy# tag -> (r -> a) -> Field field m a
    reader_ tag f = coerce @(m a) $
      reader_ tag $ f . view (Generic.field' @field)
