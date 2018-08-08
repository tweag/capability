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

module Has
  ( Has (..)
  , has
  , TheValue
  , TheField
  ) where

import Control.Lens as Lens hiding (Lens', has)
import Data.Coerce (Coercible, coerce)
import qualified Data.Generics.Product.Fields as Generic
import Data.Roles (Representational, rep)
import Data.Type.Coercion (Coercion (Coercion))
import GHC.Exts (Proxy#, proxy#)
import GHC.Generics (Generic)
import GHC.TypeLits (Symbol)


-- | A lens that ensures the role of @f@'s argument is representational.
type Lens' s a =
  forall f. (Functor f, Representational f)
  => (a -> f a) -> s -> f s

coerceLens'
  :: forall s' s a f. (Coercible s' s, Functor f, Representational f)
  => ( (a -> f a) -> s' -> f s' ) -> (a -> f a) -> s -> f s
coerceLens' = case rep @_ @_ @f @s' @s Coercion of
  Coercion -> coerce id'
  where
    id' :: ( (a -> f a) -> s -> f s ) -> (a -> f a) -> s -> f s
    id' = id


class Has (tag :: k) (a :: *) (s :: *) where
  has_ :: Proxy# tag -> Lens' s a

has :: forall tag a s. Has tag a s => Lens' s a
has = has_ (proxy# @_ @tag)


newtype TheValue a = TheValue a
instance Has tag a (TheValue a) where
  has_ _ = coerceLens' id


newtype TheField (field :: Symbol) s = TheField s
-- The constraint raises @-Wsimplifiable-class-constraints@.
-- This could be avoided by instead placing @HasField'@s constraints here.
-- Unfortunately, it uses non-exported symbols from @generic-lens@.
instance (Generic s, Generic.HasField' field s a)
  => Has tag a (TheField field s)
  where
    has_ _ = coerceLens' $ Generic.field' @field @s @a
