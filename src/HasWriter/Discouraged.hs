{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Defines discouraged instances of writer monad capabilities.
module HasWriter.Discouraged
  ( module Accessors
  ) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Unlift (MonadTransUnlift, Unlift (..), askUnlift)
import Data.Coerce (coerce)
import GHC.Exts (Proxy#)

import Accessors
import HasWriter


-- | Lift one layer in a monad transformer stack.
--
-- Note, that if the 'HasWriter' instance is based on 'HasState', then it is
-- more efficient to apply 'Lift' to the underlying state capability. E.g.
-- you should favour
--
-- > deriving (HasWriter tag w) via
-- >   WriterLog (Lift (SomeTrans (MonadState SomeStateMonad)))
--
-- over
--
-- > deriving (HasWriter tag w) via
-- >   Lift (SomeTrans (WriterLog (MonadState SomeStateMonad)))
instance
  -- MonadTransUnlift constraint requires -Wno-simplifiable-class-constraints
  (HasWriter tag w m, MonadTransUnlift t, Monad (t m))
  => HasWriter tag w (Lift (t m))
  where
    writer_ :: forall a. Proxy# tag -> (a, w) -> Lift (t m) a
    writer_ _ = coerce @((a, w) -> t m a) $ lift . writer @tag
    {-# INLINE writer_ #-}
    tell_ _ = coerce @(w -> t m ()) $ lift . tell @tag
    {-# INLINE tell_ #-}
    listen_ :: forall a. Proxy# tag -> Lift (t m) a -> Lift (t m) (a, w)
    listen_ _ = coerce @(t m a -> t m (a, w)) $ \m -> do
      u <- askUnlift
      lift $ listen @tag $ unlift u m
    {-# INLINE listen_ #-}
    pass_ :: forall a. Proxy# tag -> Lift (t m) (a, w -> w) -> Lift (t m) a
    pass_ _ = coerce @(t m (a, w -> w) -> t m a) $ \m -> do
      u <- askUnlift
      lift $ pass @tag $ unlift u m
    {-# INLINE pass_ #-}
