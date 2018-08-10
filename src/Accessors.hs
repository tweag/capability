{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}

module Accessors
  ( Coerce (..)
  , Field (..)
  ) where

import Control.Monad.IO.Class (MonadIO)
import GHC.TypeLits (Symbol)


-- | Coerce the type in the context @m@ to @to@.
newtype Coerce to m a = Coerce (m a)
  deriving (Functor, Applicative, Monad, MonadIO)


-- | Access the record field @field@ in the context @m@.
newtype Field (field :: Symbol) m a = Field (m a)
  deriving (Functor, Applicative, Monad, MonadIO)
