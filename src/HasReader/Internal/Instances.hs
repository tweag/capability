{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module HasReader.Internal.Instances
  ( MonadReader (..)
  , ReadStatePure (..)
  , ReadState (..)
  ) where

import Control.Lens (over, view)
import Control.Monad.Catch (MonadMask, bracket)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Primitive (PrimMonad)
import qualified Control.Monad.Reader.Class as Reader
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Control (MonadTransControl (..))
import Data.Coerce (Coercible, coerce)
import qualified Data.Generics.Product.Fields as Generic
import qualified Data.Generics.Product.Positions as Generic
import GHC.Exts (Proxy#)

import Accessors
import HasReader.Internal.Class
import HasState.Internal.Class


-- | Derive 'HasReader' from @m@'s
-- 'Control.Monad.Reader.Class.MonadReader' instance.
newtype MonadReader (m :: * -> *) (a :: *) = MonadReader (m a)
  deriving (Functor, Applicative, Monad, MonadIO, PrimMonad)
instance Reader.MonadReader r m => HasReader tag r (MonadReader m) where
  ask_ _ = coerce @(m r) Reader.ask
  {-# INLINE ask_ #-}
  local_
    :: forall a. Proxy# tag -> (r -> r) -> MonadReader m a -> MonadReader m a
  local_ _ = coerce @((r -> r) -> m a -> m a) Reader.local
  {-# INLINE local_ #-}
  reader_ :: forall a. Proxy# tag -> (r -> a) -> MonadReader m a
  reader_ _ = coerce @((r -> a) -> m a) Reader.reader
  {-# INLINE reader_ #-}


-- | Convert a /pure/ state monad into a reader monad.
--
-- /Pure/ meaning that the monad stack does not allow to catch exceptions.
-- Otherwise, an exception occurring in the action passed to 'local' could
-- cause the context to remain modified outside of the call to 'local'. E.g.
--
-- > local @tag (const r') (throw MyException)
-- > `catch` \MyException -> ask @tag
--
-- returns @r'@ instead of the previous value.
--
-- Note, that no @MonadIO@ instance is provided, as this would allow to
-- catch exceptions.
--
-- See 'ReadState.
newtype ReadStatePure (m :: * -> *) (a :: *) = ReadStatePure (m a)
  deriving (Functor, Applicative, Monad)
instance HasState tag r m => HasReader tag r (ReadStatePure m) where
  ask_ _ = coerce @(m r) $ get @tag
  {-# INLINE ask_ #-}
  local_ :: forall a.
    Proxy# tag -> (r -> r) -> ReadStatePure m a -> ReadStatePure m a
  local_ _ f = coerce @(m a -> m a) $ \m -> do
    r <- state @tag $ \r -> (r, f r)
    m <* put @tag r
  {-# INLINE local_ #-}
  reader_ :: forall a. Proxy# tag -> (r -> a) -> ReadStatePure m a
  reader_ _ = coerce @((r -> a) -> m a) $ gets @tag
  {-# INLINE reader_ #-}


-- | Convert a state monad into a reader monad.
--
-- Use this if the monad stack allows to catch exceptions.
--
-- See 'ReadStatePure'.
newtype ReadState (m :: * -> *) (a :: *) = ReadState (m a)
  deriving (Functor, Applicative, Monad, MonadIO, PrimMonad)
instance
  (HasState tag r m, MonadMask m)
  => HasReader tag r (ReadState m)
  where
    ask_ _ = coerce @(m r) $ get @tag
    {-# INLINE ask_ #-}
    local_ :: forall a.
      Proxy# tag -> (r -> r) -> ReadState m a -> ReadState m a
    local_ _ f = coerce @(m a -> m a) $ \action ->
      let
        setAndSave = state @tag $ \r -> (r, f r)
        restore r = put @tag r
      in
      bracket setAndSave restore $ \_ -> action
    {-# INLINE local_ #-}
    reader_ :: forall a. Proxy# tag -> (r -> a) -> ReadState m a
    reader_ _ = coerce @((r -> a) -> m a) $ gets @tag
    {-# INLINE reader_ #-}


-- | Convert the environment using safe coercion.
instance
  ( Coercible from to, HasReader tag from m
  , forall x y. Coercible x y => Coercible (m x) (m y) )
  => HasReader tag to (Coerce to m)
  where
    ask_ tag = coerce @(m from) $ ask_ tag
    {-# INLINE ask_ #-}
    local_
      :: forall a. Proxy# tag -> (to -> to) -> Coerce to m a -> Coerce to m a
    local_ tag = coerce @((from -> from) -> m a -> m a) $ local_ tag
    {-# INLINE local_ #-}
    reader_ :: forall a. Proxy# tag -> (to -> a) -> Coerce to m a
    reader_ tag = coerce @((from -> a) -> m a) $ reader_ tag
    {-# INLINE reader_ #-}


-- | Rename the tag.
instance HasReader oldtag r m => HasReader newtag r (Rename oldtag m) where
  ask_ _ = coerce @(m r) $ ask @oldtag
  {-# INLINE ask_ #-}
  local_ :: forall a.
    Proxy# newtag -> (r -> r) -> Rename oldtag m a -> Rename oldtag m a
  local_ _ = coerce @((r -> r) -> m a -> m a) $ local @oldtag
  {-# INLINE local_ #-}
  reader_ :: forall a. Proxy# newtag -> (r -> a) -> Rename oldtag m a
  reader_ _ = coerce @((r -> a) -> m a) $ reader @oldtag
  {-# INLINE reader_ #-}


-- | Zoom in on the record field @field@ of type @v@
-- in the environment @record@.
instance
  -- The constraint raises @-Wsimplifiable-class-constraints@.
  -- This could be avoided by instead placing @HasField'@s constraints here.
  -- Unfortunately, it uses non-exported symbols from @generic-lens@.
  ( tag ~ field, Generic.HasField' field record v, HasReader oldtag record m )
  => HasReader tag v (Field field oldtag m)
  where
    ask_ _ = coerce @(m v) $
      asks @oldtag $ view (Generic.field' @field)
    {-# INLINE ask_ #-}
    local_ :: forall a.
      Proxy# tag
      -> (v -> v)
      -> Field field oldtag m a
      -> Field field oldtag m a
    local_ _ = coerce @((v -> v) -> m a -> m a) $
      local @oldtag . over (Generic.field' @field)
    {-# INLINE local_ #-}
    reader_ :: forall a.
      Proxy# tag
      -> (v -> a)
      -> Field field oldtag m a
    reader_ _ f = coerce @(m a) $
      reader @oldtag $ f . view (Generic.field' @field)
    {-# INLINE reader_ #-}


-- | Zoom in on the field at position @pos@ of type @v@
-- in the environment @struct@.
instance
  -- The constraint raises @-Wsimplifiable-class-constraints@.
  -- This could be avoided by instead placing @HasPosition'@s constraints here.
  -- Unfortunately, it uses non-exported symbols from @generic-lens@.
  ( tag ~ pos, Generic.HasPosition' pos struct v, HasReader oldtag struct m )
  => HasReader tag v (Pos pos oldtag m)
  where
    ask_ _ = coerce @(m v) $
      asks @oldtag $ view (Generic.position' @pos)
    {-# INLINE ask_ #-}
    local_ :: forall a.
      Proxy# tag
      -> (v -> v)
      -> Pos pos oldtag m a
      -> Pos pos oldtag m a
    local_ _ = coerce @((v -> v) -> m a -> m a) $
      local @oldtag . over (Generic.position' @pos)
    {-# INLINE local_ #-}
    reader_ :: forall a.
      Proxy# tag
      -> (v -> a)
      -> Pos pos oldtag m a
    reader_ _ f = coerce @(m a) $
      reader @oldtag $ f . view (Generic.position' @pos)
    {-# INLINE reader_ #-}


-- | Lift one layer in a monad transformer stack.
instance (HasReader tag r m, MonadTransControl t, Monad (t m))
  => HasReader tag r (Lift (t m))
  where
    ask_ _ = coerce $ lift @t @m $ ask @tag @r
    {-# INLINE ask_ #-}
    local_
      :: forall a. Proxy# tag -> (r -> r) -> Lift (t m) a -> Lift (t m) a
    local_ _ f = coerce @(t m a -> t m a) $
      \m -> liftWith (\run -> local @tag f $ run m) >>= restoreT . pure
    {-# INLINE local_ #-}
    reader_ :: forall a. Proxy# tag -> (r -> a) -> Lift (t m) a
    reader_ _ = coerce @((r -> a) -> t m a) $ lift . reader @tag
    {-# INLINE reader_ #-}
