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

import Control.Lens as Lens hiding (has)
import Data.Coerce (coerce)
import qualified Data.Generics.Product.Fields as Generic
import GHC.Exts (Proxy#, proxy#)
import GHC.Generics (Generic)
import GHC.TypeLits (Symbol)


class Has (tag :: k) (a :: *) (s :: *) where
  has_ :: Proxy# tag -> Lens' s a

has :: forall tag a s. Has tag a s => Lens' s a
has = has_ (proxy# @_ @tag)


newtype TheValue a = TheValue a
instance Has tag a (TheValue a) where
  has_ _ = coerced


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
