{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Use this module to provide an ad-hoc interpreter for a capability using
-- type class reflection.
--
-- Use the functions 'interpret_' or 'interpret' for ad-hoc interpretation of
-- capabilities.
--
-- Refer to 'Reflected' if you would like to enable reflection for a new
-- capability.
--
-- More complex examples using this module can be found in the
-- [Reflection example
-- module](https://github.com/tweag/capability/blob/master/examples/Reflection.hs).
--
-- For details on reflection refer to the tutorial at
-- <https://www.tweag.io/posts/2017-12-21-reflection-tutorial.html> and the
-- @reflection@ library at <https://hackage.haskell.org/package/reflection>.

module Capability.Reflection
  ( -- * Reflection
    interpret_
  , interpret
  , Reified
  , Reflected (..)
  , reified
    -- * Re-exported
  , Reifies
  , reify
  , reflect
  , Proxy (..)
  ) where

import Capability.Constraints
import Capability.Derive
import Data.Proxy
import Data.Reflection

-- | @interpret_ \@tag dict action@
--
-- Execute @action@ using the ad-hoc interpretation of a capability @c@ under
-- @tag@ defined by @dict@, where @dict@ is a value of type @'Reified' tag c@,
-- i.e. a record providing the implementation of the methods of capability @c@.
--
-- For example, the following provides an ad-hoc interpretation for the
-- 'Capability.Source.HasSource' capability.
--
-- >>> :{
--   interpret_ @"my-source"
--     ReifiedSource { _await = pure "capabilities" }
--     (replicateM 3 (await @"my-source"))
--   :}
-- ["capabilities", "capabilities", "capabilities"]
interpret_ ::
  forall tag c m a.
  ( Monad m,
    forall s. Reifies s (Reified (c m)) => c (Reflected s c m)
  ) =>
  Reified (c m) ->
  (forall m'. c m' => m' a) ->
  m a
interpret_ = interpret @tag @'[] @c
{-# INLINE interpret_ #-}

-- | @interpret \@tag \@ambient dict action@
--
-- Like 'interpret_' but forwards the ambient capabilities @ambient@ into the
-- context of @action@ as well.
--
-- For example, the following provides an ad-hoc interpretation for the
-- 'Capability.Source.HasSource' capability, while using an ambient
-- 'Capability.Sink.HasSink' capability.
--
-- >>> :{
--   interpret @"my-source" @'[HasSink "my-sink" String]
--     ReifiedSource { _await = pure "capabilities" }
--     (replicateM_ 3 (await @"my-source" >>= yield @"my-sink"))
--   :}
interpret ::
  forall tag (cs :: [Capability]) c m a.
  ( Monad m,
    All cs m,
    forall s. Reifies s (Reified (c m)) => c (Reflected s c m)
  ) =>
  Reified (c m) ->
  (forall m'. All (c ': cs) m' => m' a) ->
  m a
interpret dict action =
  reify dict $ \(_ :: Proxy s) ->
    derive @(Reflected s c) @'[c] @cs action
{-# INLINE interpret #-}

-- | @Reified tag capability m@
--
-- Defines the dictionary type for the methods of @capability@ under @tag@ in
-- the monad @m@. Refer to 'interpret_' for an example use-case.
--
-- For example, the 'Capability.Sink.HasSink' capability has the method
-- @'Capability.Sink.yield' :: a -> m ()@. The corresponding dictionary type is
-- defined as follows.
--
-- >>> :{
--   data instance Reified tag (HasSink tag a) m =
--     ReifiedSink { _yield :: forall a. a -> m () }
--   :}
--
-- Superclass dictionaries are represented as nested records. For example, the
-- 'Capability.State.HasState' capability has the superclasses
-- 'Capability.Source.HasSource' and 'Capability.Sink.HasSink' and the method
-- @'Capability.State.state' :: (s -> (a, s)) -> m a@. The corresponding
-- dictionary type is defined as follows.
--
-- >>> :{
--   data instance Reified tag (HasState tag s) m =
--     ReifiedState
--       { _stateSource :: Reified tag (HasSource tag s) m,
--         _stateSink :: Reified tag (HasSink tag s) m,
--         _state :: forall a. (s -> (a, s)) -> m a
--       }
--   :}
data family Reified (c :: Constraint)

-- | @Reflected s capability m@
--
-- Carries the type class instance for @capability@ in the monad @m@ defined by
-- the dictionary reflected in @s@ in the type system.
--
-- For most use-cases it is not necessary to use this type directly. Use
-- 'interpret_' or 'interpret' instead.
--
-- If you wish to enable reflection for a new capability, then you will need to
-- define a type class instance for @Reflected@ for the new capability. Note,
-- you will also need to define an instance of 'Reified' which defines the
-- dictionary type of the new capability. Hint, you can use @'reified' \@s@ to
-- obtain the dictionary from the context in the instance implementation.
--
-- For example, the @Reflected@ instance for the 'Capability.Sink.HasSink'
-- capability can be defined as follows. Assuming the dictionary described in
-- 'Reified'.
--
-- >>> :{
--   instance
--     (Monad m, Reifies s (Reified tag (HasSink tag a) m)) =>
--     HasSink tag a (Reflected s (HasSink tag a) m)
--     where
--     yield a = Reflect $ _yield (reified @s) a
--   :}
newtype Reflected (s :: *) (c :: Capability) (m :: * -> *) (a :: *) = Reflect (m a)
  deriving (Functor, Applicative, Monad)

-- | @reified \@s@
--
-- Obtain the dictionary that is reflected in the type system under @s@.
--
-- This is a convenience wrapper around 'Data.Reflection.reflect'.
reified :: forall s c m. Reifies s (Reified (c m)) => Reified (c m)
reified = reflect (Proxy @s)
{-# INLINE reified #-}
