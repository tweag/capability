-- | Defines @newtype@s that serve as combinators
-- to compose deriving via strategies.

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}

module Capability.Accessors
  ( Coerce(..)
  , Rename(..)
  , Field(..)
  , Pos(..)
  , Ctor(..)
  , Lift(..)
  , (:.:)(..)
  ) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Primitive (PrimMonad)
import Data.Kind (Type)
import GHC.TypeLits (Nat, Symbol)

-- | Coerce the type in the context @m@ to @to@.
--
-- Example:
--
-- @
-- newtype MyInt = MyInt Int
-- newtype MyReader a = MyReader (Reader Int a)
--   deriving (HasReader "a" MyInt) via
--     Coerce MyInt (MonadReader (Reader Int))
-- @
--
-- Converts the @'Capability.Reader.HasReader' \"a\" Int@ instance of
-- @'Capability.Reader.MonadReader' (Reader Int)@ to a
-- @'Capability.Reader.HasReader' \"a\" MyInt@
-- instance using @Coercible Int MyInt@.
newtype Coerce (to :: Type) m (a :: Type) = Coerce (m a)
  deriving (Functor, Applicative, Monad, MonadIO, PrimMonad)

-- | Rename the tag.
--
-- Example:
--
-- @
-- newtype MyReader a = MyReader (Reader Int a)
--   deriving (HasReader "foo" Int) via
--     Rename "bar" (MonadReader (Reader Int))
-- @
--
-- Converts the @'Capability.Reader.HasReader' \"bar\" Int@ instance of
-- @'Capability.Reader.MonadReader' (Reader Int)@ to a
-- @'Capability.Reader.HasReader' \"foo\" Int@ instance by renaming the tag.
--
-- Note, that 'Capability.Reader.MonadReader' itself does not fix a tag,
-- and @Rename@ is redundant in this example.
--
-- See 'Pos' below for a common use-case.
newtype Rename (oldtag :: k) m (a :: Type) = Rename (m a)
  deriving (Functor, Applicative, Monad, MonadIO, PrimMonad)

-- | Access the record field @field@ in the context @m@.
--
-- Example:
--
-- @
-- data Foo = Foo { foo :: Int }
-- newtype MyReader a = MyReader (Reader Foo a)
--   deriving (HasReader "foo" Int) via
--     Field "foo" () (MonadReader (Reader Foo))
-- @
--
-- Converts the @'Capability.Reader.HasReader' () Foo@ instance of
-- @'Capability.Reader.MonadReader' (Reader Foo)@ to a
-- @'Capability.Reader.HasReader' \"foo\" Int@
-- instance by focusing on the field @foo@ in the @Foo@ record.
--
-- See 'Rename' for a way to change the tag.
newtype Field (field :: Symbol) (oldtag :: k) m (a :: Type) = Field (m a)
  deriving (Functor, Applicative, Monad, MonadIO, PrimMonad)

-- | Access the value at position @pos@ in the context @m@.
--
-- Example:
--
-- @
-- newtype MyReader a = MyReader (Reader (Int, Bool) a)
--   deriving (HasReader 1 Int) via
--     Pos 1 () (MonadReader (Reader (Int, Bool)))
-- @
--
-- Converts the @'Capability.Reader.HasReader' () (Int, Bool)@ instance of
-- @'Capability.Reader.MonadReader' (Reader (Int, Bool))@ to a
-- @'Capability.Reader.HasReader' 1 Int@ instance
-- by focusing on the first element of the tuple.
--
-- The implied number tag can be renamed to a more descriptive name using
-- the 'Rename' combinator:
--
-- @
-- newtype MyReader a = MyReader (Reader (Int, Bool) a)
--   deriving (HasReader "foo" Int) via
--     Rename 1 (Pos 1 () (MonadReader (Reader (Int, Bool))))
-- @
newtype Pos (pos :: Nat) (oldtag :: k) m (a :: Type) = Pos (m a)
  deriving (Functor, Applicative, Monad, MonadIO, PrimMonad)

-- | Choose the given constructor in the sum-type in context @m@.
--
-- Example:
--
-- @
-- data MyError = ErrA String | ErrB String
-- newtype MyExcept a = MyExcept (ExceptT MyError Identity a)
--   deriving (HasThrow \"ErrB" String) via
--     Ctor \"ErrB" () (MonadError (ExceptT MyError Identity))
-- @
--
-- Converts the @'Capability.Error.HasThrow' () \"MyError\"@ instance of
-- @'Capability.Error.MonadError' (ExceptT MyError Identity)@ to a
-- @'Capability.Error.HasThrow' \"ErrB\" String@
-- instance by wrapping thrown @String@s in the @ErrB@ constructor.
newtype Ctor (ctor :: Symbol) (oldtag :: k) m (a :: Type) = Ctor (m a)
  deriving (Functor, Applicative, Monad, MonadIO, PrimMonad)

-- | Skip one level in a monad transformer stack.
--
-- Note, that instances generated with this strategy can incur a performance
-- penalty.
--
-- Example:
--
-- @
-- newtype MyStates a = MyStates (StateT Int (State Bool) a)
--   deriving (HasState "foo" Bool) via
--     Lift (StateT Int (MonadState (State Bool)))
-- @
--
-- Uses the 'Control.Monad.Trans.Class.MonadTrans' instance of
-- @StateT Int@ to lift
-- the @'Capability.State.HasState' "\foo\" Bool@ instance of the underlying
-- @'Capability.State.MonadState' (State Bool)@ over the
-- @StateT Int@ monad transformer.
newtype Lift m (a :: Type) = Lift (m a)
  deriving (Functor, Applicative, Monad, MonadIO, PrimMonad)


-- | Compose two accessors.
--
-- This is not necessary in deriving via clauses, but in places where a
-- transformer is expected as a type argument. E.g. 'HasError.wrapError'.
newtype (:.:)
  (t2 :: (Type -> Type) -> Type -> Type)
  (t1 :: (Type -> Type) -> Type -> Type)
  (m :: Type -> Type)
  (a :: Type)
  = (:.:) (m a)
  deriving (Functor, Applicative, Monad, MonadIO, PrimMonad)
infixr 9 :.:
