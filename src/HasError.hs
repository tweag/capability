{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

module HasError
  ( HasError
  , HasThrow (..)
  , throw
  , HasCatch (..)
  , catch
  , MonadThrow (..)
  , MonadCatch (..)
  , SafeExceptions (..)
  , MonadUnliftIO (..)
  , module Accessors
  ) where

import qualified Control.Exception.Safe as Safe
import Control.Lens (review)
import qualified Control.Monad.Catch as Catch
import qualified Control.Monad.Except as Except
import Control.Monad.IO.Class (MonadIO)
import qualified Control.Monad.IO.Unlift as UnliftIO
import Control.Monad.Primitive (PrimMonad)
import Data.Coerce (coerce)
import qualified Data.Generics.Sum.Constructors as Generic
import GHC.Exts (Proxy#, proxy#)
import qualified UnliftIO.Exception as UnliftIO

import Accessors


class (HasThrow tag e m, HasCatch tag e m) => HasError tag e m
instance (HasThrow tag e m, HasCatch tag e m) => HasError tag e m


class HasThrow (tag :: k) (e :: *) (m :: * -> *) where
  -- | Use 'throw' instead.
  throw_ :: Proxy# tag -> e -> m a

-- | Throw an exception.
throw :: forall tag e m a. HasThrow tag e m => e -> m a
throw = throw_ (proxy# @_ @tag)
{-# INLINE throw #-}


-- XXX: Should catch only catch exceptions thrown under the same tag?
--   Or should we consider colliding tags on the same transformer illegal,
--   the same way it is illegal in @HasReader@ or @HasState@?

class HasCatch (tag :: k) (e :: *) (m :: * -> *) where
  -- | Use 'catch' instead.
  catch_ :: Proxy# tag -> m a -> (e -> m a) -> m a

-- | Provide a handler for exceptions thrown in the given action.
catch :: forall tag e m a. HasCatch tag e m => m a -> (e -> m a) -> m a
catch = catch_ (proxy# @_ @tag)
{-# INLINE catch #-}


-- XXX: Does it make sense to add a HasMask capability similar to @MonadMask@?
--   What would the meaning of the tag be?


-- | Derive 'HasError from @m@'s 'Control.Monad.Except.MonadError' instance.
newtype MonadError m (a :: *) = MonadError (m a)
  deriving (Functor, Applicative, Monad, MonadIO, PrimMonad)
instance Except.MonadError e m => HasThrow tag e (MonadError m) where
  throw_ :: forall a. Proxy# tag -> e -> MonadError m a
  throw_ _ = coerce @(e -> m a) $ Except.throwError
  {-# INLINE throw_ #-}
instance Except.MonadError e m => HasCatch tag e (MonadError m) where
  catch_ :: forall a.
    Proxy# tag -> MonadError m a -> (e -> MonadError m a) -> MonadError m a
  catch_ _ = coerce @(m a -> (e -> m a) -> m a) $ Except.catchError
  {-# INLINE catch_ #-}


-- | Derive 'HasThrow' from @m@'s 'Control.Monad.Catch.MonadThrow' instance.
newtype MonadThrow m (a :: *) = MonadThrow (m a)
  deriving (Functor, Applicative, Monad, MonadIO, PrimMonad)
instance (Catch.Exception e, Catch.MonadThrow m)
  => HasThrow tag e (MonadThrow m)
  where
    throw_ :: forall a. Proxy# tag -> e -> MonadThrow m a
    throw_ _ = coerce @(e -> m a) $ Catch.throwM
    {-# INLINE throw_ #-}


-- | Derive 'HasCatch from @m@'s 'Control.Monad.Catch.MonadCatch instance.
newtype MonadCatch m (a :: *) = MonadCatch (m a)
  deriving (Functor, Applicative, Monad, MonadIO, PrimMonad)
instance (Catch.Exception e, Catch.MonadCatch m)
  => HasCatch tag e (MonadCatch m)
  where
    catch_ :: forall a.
      Proxy# tag -> MonadCatch m a -> (e -> MonadCatch m a) -> MonadCatch m a
    catch_ _ = coerce @(m a -> (e -> m a) -> m a) $ Catch.catch
    {-# INLINE catch_ #-}


-- | Derive 'HasError' using the functionality from the @safe-exceptions@
-- package.
newtype SafeExceptions m (a :: *) = SafeExceptions (m a)
  deriving (Functor, Applicative, Monad, MonadIO, PrimMonad)
instance (Safe.Exception e, Safe.MonadThrow m)
  => HasThrow tag e (SafeExceptions m)
  where
    throw_ :: forall a. Proxy# tag -> e -> SafeExceptions m a
    throw_ _ = coerce @(e -> m a) $ Safe.throw
    {-# INLINE throw_ #-}
instance (Safe.Exception e, Safe.MonadCatch m)
  => HasCatch tag e (SafeExceptions m)
  where
    catch_ :: forall a.
      Proxy# tag
      -> SafeExceptions m a
      -> (e -> SafeExceptions m a)
      -> SafeExceptions m a
    catch_ _ = coerce @(m a -> (e -> m a) -> m a) $ Safe.catch
    {-# INLINE catch_ #-}


-- | Derive 'HasError' using the functionality from the @unliftio@ package.
newtype MonadUnliftIO m (a :: *) = MonadUnliftIO (m a)
  deriving (Functor, Applicative, Monad, MonadIO, PrimMonad)
instance (UnliftIO.Exception e, MonadIO m)
  => HasThrow tag e (MonadUnliftIO m)
  where
    throw_ :: forall a. Proxy# tag -> e -> MonadUnliftIO m a
    throw_ _ = coerce @(e -> m a) $ UnliftIO.throwIO
    {-# INLINE throw_ #-}
instance (UnliftIO.Exception e, UnliftIO.MonadUnliftIO m)
  => HasCatch tag e (MonadUnliftIO m)
  where
    catch_ :: forall a.
      Proxy# tag
      -> MonadUnliftIO m a
      -> (e -> MonadUnliftIO m a)
      -> MonadUnliftIO m a
    catch_ _ = coerce @(m a -> (e -> m a) -> m a) $ UnliftIO.catch
    {-# INLINE catch_ #-}


-- | Wrap the exception @e@ with the constructor @ctor@ to throw an exception
-- of type @sum@.
instance
  -- The constraint raises @-Wsimplifiable-class-constraints@. This could
  -- be avoided by instead placing @AsConstructor'@s constraints here.
  -- Unfortunately, it uses non-exported symbols from @generic-lens@.
  (Generic.AsConstructor' ctor sum e, HasThrow tag sum m)
  => HasThrow tag e (Ctor ctor m)
  where
    throw_ :: forall a. Proxy# tag -> e -> Ctor ctor m a
    throw_ _ = coerce @(e -> m a) $
      throw @tag . review (Generic._Ctor' @ctor @sum)
    {-# INLINE throw_ #-}


-- | Catch an exception of type @sum@ if its constructor matches @ctor@.
instance
  -- The constraint raises @-Wsimplifiable-class-constraints@. This could
  -- be avoided by instead placing @AsConstructor'@s constraints here.
  -- Unfortunately, it uses non-exported symbols from @generic-lens@.
  (Generic.AsConstructor' ctor sum e, HasThrow tag sum m)
  => HasCatch tag e (Ctor ctor m)
  where
    catch_ :: forall a.
      Proxy# tag -> Ctor ctor m a -> (e -> Ctor ctor m a) -> Ctor ctor m a
    catch_ _ = coerce @(m a -> (e -> m a) -> m a) $
      -- XXX: We need a capability like @catchJust@ to implement this.
      undefined
    {-# INLINE catch_ #-}
