{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}

module Accessors
  ( Field (..)
  ) where

import Control.Monad.IO.Class (MonadIO)
import GHC.TypeLits (Symbol)


-- | Access the record field @field@ in the context @m@.
newtype Field (field :: Symbol) m a = Field (m a)
  deriving (Functor, Applicative, Monad, MonadIO)
