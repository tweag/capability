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
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}

module HasReader
  ( HasReader (..)
  , ask
  , asks
  , local
  , reader
  , MonadReader (..)
  , module Accessors
  ) where

import Control.Lens (over, view)
import Control.Monad.IO.Class (MonadIO)
import qualified Control.Monad.Reader.Class as Reader
import Data.Coerce (Coercible, coerce)
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


-- | Derive 'HasReader' from @m@'s
-- 'Control.Monad.Reader.Class.MonadReader' instance.
newtype MonadReader (m :: * -> *) (a :: *) = MonadReader (m a)
  deriving (Functor, Applicative, Monad, MonadIO)
instance Reader.MonadReader r m => HasReader tag r (MonadReader m) where
  ask_ _ = coerce @(m r) Reader.ask
  local_
    :: forall a. Proxy# tag -> (r -> r) -> MonadReader m a -> MonadReader m a
  local_ _ = coerce @((r -> r) -> m a -> m a) Reader.local
  reader_ :: forall a. Proxy# tag -> (r -> a) -> MonadReader m a
  reader_ _ = coerce @((r -> a) -> m a) Reader.reader


-- | Convert the environment using safe coercion.
instance
  ( Coercible from to, HasReader tag from m
  , forall x y. Coercible x y => Coercible (m x) (m y) )
  => HasReader tag to (Coerce to m)
  where
    ask_ tag = coerce @(m from) $ ask_ tag
    local_
      :: forall a. Proxy# tag -> (to -> to) -> Coerce to m a -> Coerce to m a
    local_ tag = coerce @((from -> from) -> m a -> m a) $ local_ tag
    reader_ :: forall a. Proxy# tag -> (to -> a) -> Coerce to m a
    reader_ tag = coerce @((from -> a) -> m a) $ reader_ tag


-- | Zoom in on the record field @field@ of type @r@ in the environment @r'@.
instance
  -- The constraint raises @-Wsimplifiable-class-constraints@.
  -- This could be avoided by instead placing @HasField'@s constraints here.
  -- Unfortunately, it uses non-exported symbols from @generic-lens@.
  ( Generic record, Generic.HasField' field record v, HasReader tag record m )
  => HasReader tag v (Field field m)
  where
    ask_ _ = coerce @(m v) $
      asks @tag $ view (Generic.field' @field)
    local_
      :: forall a. Proxy# tag -> (v -> v) -> Field field m a -> Field field m a
    local_ tag = coerce @((v -> v) -> m a -> m a) $
      local_ tag . over (Generic.field' @field)
    reader_ :: forall a. Proxy# tag -> (v -> a) -> Field field m a
    reader_ tag f = coerce @(m a) $
      reader_ tag $ f . view (Generic.field' @field)
