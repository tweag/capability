-- | Defines capabilities for actions that may fail or throw exceptions,
-- and optionally may catch exceptions.
--
-- Monads based on @IO@ can use the underlying @IO@ exception mechanism
-- to that end. The details of synchronous and asynchronous exception
-- handling depend on the strategy used.
-- See 'MonadThrow', 'SafeExceptions', 'MonadUnliftIO'.
--
-- The associated tag can be used to select the exception type, or to
-- select a layer in monad transformer stacks. Note, that it is illegal
-- to have multiple tags refer to overlapping exception types in the same
-- layer. Consider the following example
--
-- @
-- newtype M a = M (IO a)
--   deriving (HasThrow "foo" IOException) via
--     MonadUnliftIO IO
--   deriving (HasThrow "bar" SomeException) via
--     MonadUnliftIO IO
-- @
--
-- In this case the tags @"foo"@ and @"bar"@ refer to overlapping exception
-- types in the same layer, because @catch \@"bar"@ may also catch an exception
-- thrown under @"foo"@.

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

module Capability.Error
  ( -- * Relational capabilities
    HasThrow(..)
  , throw
  , HasCatch(..)
  , catch
  , catchJust
  , wrapError
    -- * Functional capabilities
  , HasThrow'
  , HasCatch'
  , TypeOf
    -- * Strategies
  , MonadError(..)
  , MonadThrow(..)
  , MonadCatch(..)
  , SafeExceptions(..)
  , MonadUnliftIO(..)
    -- ** Modifiers
  , module Capability.Accessors
    -- * Re-exported
  , Exception(..)
  , Typeable
  ) where

import Capability.Accessors
import Capability.Constraints
import Capability.Derive (derive)
import Capability.TypeOf
import Control.Exception (Exception(..))
import qualified Control.Exception.Safe as Safe
import Control.Lens (preview, review)
import Control.Monad ((<=<))
import qualified Control.Monad.Catch as Catch
import qualified Control.Monad.Except as Except
import Control.Monad.IO.Class (MonadIO)
import qualified Control.Monad.IO.Unlift as UnliftIO
import Control.Monad.Primitive (PrimMonad)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Control (MonadTransControl(..))
import Data.Coerce (Coercible, coerce)
import qualified Data.Generics.Sum.Constructors as Generic
import Data.Typeable (Typeable)
import GHC.Exts (Proxy#, proxy#)
import qualified UnliftIO.Exception as UnliftIO

-- | Capability to throw exceptions of type @e@ under @tag@.
--
-- @HasThrow@/@HasCatch@ capabilities at different tags should be independent.
-- See 'HasCatch'.
class Monad m
  => HasThrow (tag :: k) (e :: *) (m :: * -> *) | tag m -> e
  where
    -- | For technical reasons, this method needs an extra proxy argument.
    -- You only need it if you are defining new instances of 'HasReader'.
    -- Otherwise, you will want to use 'throw'.
    -- See 'throw' for more documentation.
    throw_ :: Proxy# tag -> e -> m a

-- | Throw an exception in the specified exception capability.
throw :: forall tag e m a. HasThrow tag e m => e -> m a
throw = throw_ (proxy# @_ @tag)
{-# INLINE throw #-}

-- | Capability to catch exceptions of type @e@ under @tag@.
--
-- @HasThrow@/@HasCatch@ capabilities at different tags should be independent.
-- In particular, the following program should throw @SomeError@ and not
-- return @()@.
-- > example ::
-- >   (HasThrow "Left" SomeError m, HasCatch "Right" SomeError m)
-- >   => m ()
-- > example =
-- >   catch @"Left"
-- >     (throw @"Right" SomeError)
-- >     \_ -> pure ()
--
-- See 'wrapError' for a way to combine multiple exception types into one.
class HasThrow tag e m
  => HasCatch (tag :: k) (e :: *) (m :: * -> *) | tag m -> e
  where
    -- | For technical reasons, this method needs an extra proxy argument.
    -- You only need it if you are defining new instances of 'HasReader'.
    -- Otherwise, you will want to use 'catch'.
    -- See 'catch' for more documentation.
    catch_ :: Proxy# tag -> m a -> (e -> m a) -> m a
    -- | For technical reasons, this method needs an extra proxy argument.
    -- You only need it if you are defining new instances of 'HasReader'.
    -- Otherwise, you will want to use 'catchJust'.
    -- See 'catchJust' for more documentation.
    catchJust_ :: Proxy# tag -> (e -> Maybe b) -> m a -> (b -> m a) -> m a

-- | Provide a handler for exceptions thrown in the given action
-- in the given exception capability.
catch :: forall tag e m a. HasCatch tag e m => m a -> (e -> m a) -> m a
catch = catch_ (proxy# @_ @tag)
{-# INLINE catch #-}

-- | Like 'catch', but only handle the exception if the provided function
-- returns 'Just'.
catchJust :: forall tag e m a b. HasCatch tag e m
  => (e -> Maybe b) -> m a -> (b -> m a) -> m a
catchJust = catchJust_ (proxy# @_ @tag)
{-# INLINE catchJust #-}

-- | Wrap exceptions @inner@ originating from the given action according to
-- the accessor @t@. Retain arbitrary capabilities listed in @cs@.
--
-- Example:
--
-- > wrapError
-- >   @"ComponentError" @(Ctor "ComponentError" "AppError") @'[]
-- >   component
-- >
-- > component :: HasError "ComponentError" ComponentError m => m ()
-- > data AppError = ComponentError ComponentError
--
-- This function is experimental and subject to change.
-- See <https://github.com/tweag/capability/issues/46>.
wrapError :: forall innertag t (cs :: [Capability]) inner m a.
  ( forall x. Coercible (t m x) (m x)
  , HasCatch innertag inner (t m)
  , All cs m)
  => (forall m'. All (HasCatch innertag inner ': cs) m' => m' a) -> m a
wrapError =
  derive @t @'[HasCatch innertag inner] @cs
{-# INLINE wrapError #-}

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
  catchJust_ :: forall a b.
    Proxy# tag
    -> (e -> Maybe b)
    -> MonadError m a
    -> (b -> MonadError m a)
    -> MonadError m a
  catchJust_ tag f m h = catch_ tag m $ \e -> maybe (throw_ tag e) h $ f e
  {-# INLINE catchJust_ #-}

-- | Derive 'HasThrow' from @m@'s 'Control.Monad.Catch.MonadThrow' instance.
newtype MonadThrow (e :: *) m (a :: *) = MonadThrow (m a)
  deriving (Functor, Applicative, Monad, MonadIO, PrimMonad)

instance (Catch.Exception e, Catch.MonadThrow m)
  => HasThrow tag e (MonadThrow e m)
  where
    throw_ :: forall a. Proxy# tag -> e -> MonadThrow e m a
    throw_ _ = coerce @(e -> m a) $ Catch.throwM
    {-# INLINE throw_ #-}

-- | Derive 'HasCatch from @m@'s 'Control.Monad.Catch.MonadCatch instance.
newtype MonadCatch (e :: *) m (a :: *) = MonadCatch (m a)
  deriving (Functor, Applicative, Monad, MonadIO, PrimMonad)
  deriving (HasThrow tag e) via MonadThrow e m

instance (Catch.Exception e, Catch.MonadCatch m)
  => HasCatch tag e (MonadCatch e m)
  where
    catch_ :: forall a.
      Proxy# tag
      -> MonadCatch e m a
      -> (e -> MonadCatch e m a)
      -> MonadCatch e m a
    catch_ _ = coerce @(m a -> (e -> m a) -> m a) $ Catch.catch
    {-# INLINE catch_ #-}
    catchJust_ :: forall a b.
      Proxy# tag
      -> (e -> Maybe b)
      -> MonadCatch e m a
      -> (b -> MonadCatch e m a)
      -> MonadCatch e m a
    catchJust_ _ = coerce @((e -> Maybe b) -> m a -> (b -> m a) -> m a) $
      Catch.catchJust
    {-# INLINE catchJust_ #-}


-- | Derive 'HasError' using the functionality from the @safe-exceptions@
-- package.
newtype SafeExceptions (e :: *) m (a :: *) = SafeExceptions (m a)
  deriving (Functor, Applicative, Monad, MonadIO, PrimMonad)

instance (Safe.Exception e, Safe.MonadThrow m)
  => HasThrow tag e (SafeExceptions e m)
  where
    throw_ :: forall a. Proxy# tag -> e -> SafeExceptions e m a
    throw_ _ = coerce @(e -> m a) $ Safe.throw
    {-# INLINE throw_ #-}

instance (Safe.Exception e, Safe.MonadCatch m)
  => HasCatch tag e (SafeExceptions e m)
  where
    catch_ :: forall a.
      Proxy# tag
      -> SafeExceptions e m a
      -> (e -> SafeExceptions e m a)
      -> SafeExceptions e m a
    catch_ _ = coerce @(m a -> (e -> m a) -> m a) $ Safe.catch
    {-# INLINE catch_ #-}
    catchJust_ :: forall a b.
      Proxy# tag
      -> (e -> Maybe b)
      -> SafeExceptions e m a
      -> (b -> SafeExceptions e m a)
      -> SafeExceptions e m a
    catchJust_ _ = coerce @((e -> Maybe b) -> m a -> (b -> m a) -> m a) $
      Safe.catchJust
    {-# INLINE catchJust_ #-}

-- | Derive 'HasError' using the functionality from the @unliftio@ package.
newtype MonadUnliftIO (e :: *) m (a :: *) = MonadUnliftIO (m a)
  deriving (Functor, Applicative, Monad, MonadIO, PrimMonad)

instance (UnliftIO.Exception e, MonadIO m)
  => HasThrow tag e (MonadUnliftIO e m)
  where
    throw_ :: forall a. Proxy# tag -> e -> MonadUnliftIO e m a
    throw_ _ = coerce @(e -> m a) $ UnliftIO.throwIO
    {-# INLINE throw_ #-}

instance (UnliftIO.Exception e, UnliftIO.MonadUnliftIO m)
  => HasCatch tag e (MonadUnliftIO e m)
  where
    catch_ :: forall a.
      Proxy# tag
      -> MonadUnliftIO e m a
      -> (e -> MonadUnliftIO e m a)
      -> MonadUnliftIO e m a
    catch_ _ = coerce @(m a -> (e -> m a) -> m a) $ UnliftIO.catch
    {-# INLINE catch_ #-}
    catchJust_ :: forall a b.
      Proxy# tag
      -> (e -> Maybe b)
      -> MonadUnliftIO e m a
      -> (b -> MonadUnliftIO e m a)
      -> MonadUnliftIO e m a
    catchJust_ _ = coerce @((e -> Maybe b) -> m a -> (b -> m a) -> m a) $
      UnliftIO.catchJust
    {-# INLINE catchJust_ #-}

-- | Rename the tag.
--
-- Apply cautiously. See @'HasCatch' newtag e ('Rename' oldtag m)@.
instance HasThrow oldtag e m => HasThrow newtag e (Rename oldtag m) where
  throw_ :: forall a. Proxy# newtag -> e -> Rename oldtag m a
  throw_ _ = coerce @(e -> m a) $ throw @oldtag
  {-# INLINE throw_ #-}

-- | Rename the tag.
--
-- Apply cautiously. E.g. the following code produces colliding instances,
-- where exceptions thrown in @\"Foo\"@ cannot be distinguished from exceptions
-- thrown in @\"Bar\"@ and vice-versa.
--
-- > newtype Bad a = Bad (IO a)
-- >   deriving (Functor, Applicative, Monad)
-- >   deriving
-- >     ( HasThrow "Foo" m
-- >     , HasCatch "Foo" m
-- >     ) via Rename () (MonadUnliftIO SomeError IO)
-- >   deriving
-- >     ( HasThrow "Bar" m
-- >     , HasCatch "Bar" m
-- >     ) via Rename () (MonadUnliftIO SomeError IO)
instance HasCatch oldtag e m => HasCatch newtag e (Rename oldtag m) where
  catch_ :: forall a.
    Proxy# newtag
    -> Rename oldtag m a
    -> (e -> Rename oldtag m a)
    -> Rename oldtag m a
  catch_ _ = coerce @(m a -> (e -> m a) -> m a) $ catch @oldtag
  {-# INLINE catch_ #-}
  catchJust_ :: forall a b.
    Proxy# newtag
    -> (e -> Maybe b)
    -> Rename oldtag m a
    -> (b -> Rename oldtag m a)
    -> Rename oldtag m a
  catchJust_ _ = coerce @((e -> Maybe b) -> m a -> (b -> m a) -> m a) $
    catchJust @oldtag
  {-# INLINE catchJust_ #-}

-- | Wrap the exception @e@ with the constructor @ctor@ to throw an exception
-- of type @sum@.
instance
  -- The constraint raises @-Wsimplifiable-class-constraints@. This could
  -- be avoided by instead placing @AsConstructor'@s constraints here.
  -- Unfortunately, it uses non-exported symbols from @generic-lens@.
  (Generic.AsConstructor' ctor sum e, HasThrow oldtag sum m)
  => HasThrow ctor e (Ctor ctor oldtag m)
  where
    throw_ :: forall a. Proxy# ctor -> e -> Ctor ctor oldtag m a
    throw_ _ = coerce @(e -> m a) $
      throw @oldtag . review (Generic._Ctor' @ctor @sum)
    {-# INLINE throw_ #-}

-- | Catch an exception of type @sum@ if its constructor matches @ctor@.
instance
  -- The constraint raises @-Wsimplifiable-class-constraints@. This could
  -- be avoided by instead placing @AsConstructor'@s constraints here.
  -- Unfortunately, it uses non-exported symbols from @generic-lens@.
  (Generic.AsConstructor' ctor sum e, HasCatch oldtag sum m)
  => HasCatch ctor e (Ctor ctor oldtag m)
  where
    catch_ :: forall a.
      Proxy# ctor
      -> Ctor ctor oldtag m a
      -> (e -> Ctor ctor oldtag m a)
      -> Ctor ctor oldtag m a
    catch_ _ = coerce @(m a -> (e -> m a) -> m a) $
      catchJust @oldtag @sum $ preview (Generic._Ctor' @ctor @sum)
    {-# INLINE catch_ #-}
    catchJust_ :: forall a b.
      Proxy# ctor
      -> (e -> Maybe b)
      -> Ctor ctor oldtag m a
      -> (b -> Ctor ctor oldtag m a)
      -> Ctor ctor oldtag m a
    catchJust_ _ = coerce @((e -> Maybe b) -> m a -> (b -> m a) -> m a) $ \f ->
      catchJust @oldtag @sum $ f <=< preview (Generic._Ctor' @ctor @sum)
    {-# INLINE catchJust_ #-}

-- | Lift one layer in a monad transformer stack.
instance
  ( HasThrow tag e m, MonadTrans t, Monad (t m) )
  => HasThrow tag e (Lift (t m))
  where
    throw_ :: forall a. Proxy# tag -> e -> Lift (t m) a
    throw_ tag = coerce @(e -> t m a) $ lift . throw_ tag
    {-# INLINE throw_ #-}

-- | Lift one layer in a monad transformer stack.
instance
  ( HasCatch tag e m, MonadTransControl t, Monad (t m) )
  => HasCatch tag e (Lift (t m))
  where
    catch_ :: forall a.
      Proxy# tag
      -> Lift (t m) a
      -> (e -> Lift (t m) a)
      -> Lift (t m) a
    catch_ tag = coerce @(t m a -> (e -> t m a) -> t m a) $ \m h ->
      liftWith (\run -> catch_ tag (run m) (run . h)) >>= restoreT . pure
    {-# INLINE catch_ #-}
    catchJust_ :: forall a b.
      Proxy# tag
      -> (e -> Maybe b)
      -> Lift (t m) a
      -> (b -> Lift (t m) a)
      -> Lift (t m) a
    catchJust_ tag =
      coerce @((e -> Maybe b) -> t m a -> (b -> t m a) -> t m a) $ \f m h ->
        liftWith (\run -> catchJust_ tag f (run m) (run . h)) >>= restoreT . pure
    {-# INLINE catchJust_ #-}


-- | Compose two accessors.
deriving via ((t2 :: (* -> *) -> * -> *) ((t1 :: (* -> *) -> * -> *) m))
  instance
  ( forall x. Coercible (m x) (t2 (t1 m) x)
  , Monad m, HasThrow tag e (t2 (t1 m)) )
  => HasThrow tag e ((t2 :.: t1) m)


-- | Compose two accessors.
deriving via ((t2 :: (* -> *) -> * -> *) ((t1 :: (* -> *) -> * -> *) m))
  instance
  ( forall x. Coercible (m x) (t2 (t1 m) x)
  , Monad m, HasCatch tag e (t2 (t1 m)) )
  => HasCatch tag e ((t2 :.: t1) m)

-- | Type synonym using the 'TypeOf' type family to specify 'HasThrow'
-- constraints without having to specify the type associated to a tag.
type HasThrow' (tag :: k) = HasThrow tag (TypeOf k tag)

-- | Type synonym using the 'TypeOf' type family to specify 'HasCatch'
-- constraints without having to specify the type associated to a tag.
type HasCatch' (tag :: k) = HasCatch tag (TypeOf k tag)
