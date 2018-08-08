-- See @Has tag a (TheField field s)@.
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Has
  ( Has (..)
  , has
  , Lens'
  , TheValue (..)
  , TheField (..)
  ) where

import Data.Coerce (Coercible, coerce)
import qualified Data.Generics.Product.Fields as Generic
import GHC.Exts (Proxy#, proxy#)
import GHC.Generics (Generic)
import GHC.TypeLits (Symbol)


-- | @Has tag a s@ denotes that an @s@ has an @a@ associated with @tag@ that
-- can be extracted or modified.
class Has (tag :: k) (a :: *) (s :: *) | tag s -> a where
  has_ :: forall f
    . ( Functor f, forall x y. Coercible x y => Coercible (f x) (f y) )
    => Proxy# tag -> (a -> f a) -> s -> f s

-- | A lens from @s@ to @a@ associated with @tag@.
has :: forall tag a s. Has tag a s => Lens' s a
has = has_ (proxy# @_ @tag)

-- | A lens that ensures the role of @f@'s argument is representational.
-- This is to enable coercion of lenses.
type Lens' s a =
  forall f. (Functor f, forall a b. Coercible a b => Coercible (f a) (f b))
  => (a -> f a) -> s -> f s


-- | For any tag a value has itself.
newtype TheValue a = TheValue a
instance Has tag a (TheValue a) where
  has_
    :: forall f
    . ( Functor f, forall x y. Coercible x y => Coercible (f x) (f y) )
    => Proxy# tag -> (a -> f a) -> TheValue a -> f (TheValue a)
  has_ _ = coerce (id :: (a -> f a) -> a -> f a)


newtype TheField (field :: Symbol) s = TheField s
-- The constraint raises @-Wsimplifiable-class-constraints@.
-- This could be avoided by instead placing @HasField'@s constraints here.
-- Unfortunately, it uses non-exported symbols from @generic-lens@.
instance (Generic s, Generic.HasField' field s a)
  => Has tag a (TheField field s)
  where
    has_
      :: forall f
      . ( Functor f, forall x y. Coercible x y => Coercible (f x) (f y) )
      => Proxy# tag -> (a -> f a) -> TheField field s -> f (TheField field s)
    has_ _ = coerce (Generic.field' @field :: (a -> f a) -> s -> f s)
