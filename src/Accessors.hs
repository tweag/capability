{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeInType #-}

module Accessors
  ( Field (..)
  , Rename (..)
  ) where

import Control.Monad.IO.Class (MonadIO)
import GHC.TypeLits (Symbol)


-- | Access the record field @field@ in the context @m@.
newtype Field (field :: Symbol) m (a :: *) = Field (m a)
  deriving (Functor, Applicative, Monad, MonadIO)


newtype Rename (newtag :: k) (oldtag :: k') m (a :: *) = Rename (m a)
  deriving (Functor, Applicative, Monad, MonadIO)
