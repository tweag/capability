-- See @Has tag a (TheField field s)@.
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}

module Has where

import Control.Lens as Lens
import Data.Coerce (coerce)
import qualified Data.Generics.Product.Fields as Generic
import GHC.Exts (Proxy#, proxy#)
import GHC.Generics (Generic)
import GHC.TypeLits (Symbol)


class (Proj tag a s, Set tag a s)
  => Has (tag :: k) (a :: *) (s :: *)
  where
    has_ :: Proxy# tag -> Lens' s a

has :: forall tag a s. Has tag a s => Lens' s a
has = has_ (proxy# @_ @tag)


class Proj (tag :: k) (a :: *) (s :: *) where
  proj_ :: Proxy# tag -> s -> a
  default proj_ :: Has tag a s => Proxy# tag -> s -> a
  proj_ tag = Lens.view (has_ tag)

proj :: forall tag a s. Proj tag a s => s -> a
proj = proj_ (proxy# @_ @tag)


class Set (tag :: k) (a :: *) (s :: *) where
  set_ :: Proxy# tag -> a -> s -> s
  default set_ :: Has tag a s => Proxy# tag -> a -> s -> s
  set_ tag = Lens.set (has_ tag)

set :: forall tag a s. Set tag a s => a -> s -> s
set = set_ (proxy# @_ @tag)


newtype TheValue a = TheValue a
instance Has tag a (TheValue a) where
  has_ _ = coerced
instance Proj tag a (TheValue a)
instance Set tag a (TheValue a)


newtype TheField (field :: Symbol) s = TheField s
-- The constraint raises @-Wsimplifiable-class-constraints@.
-- This could be avoided by instead placing @HasField'@s constraints here.
-- Unfortunately, it uses non-exported symbols from @generic-lens@.
instance (Generic s, Generic.HasField' field s a)
  => Has tag a (TheField field s)
  where
    has_ _ = coerced' . Generic.field' @field
      where
        coerced' = coerced :: Iso' (TheField field s) s
instance (Generic s, Generic.HasField' field s a)
  => Proj tag a (TheField field s)
instance (Generic s, Generic.HasField' field s a)
  => Set tag a (TheField field s)
