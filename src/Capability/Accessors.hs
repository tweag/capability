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
-- Converts the @HasReader \"a\" Int@ instance of @MonadReader (Reader Int)@
-- to a @HasReader \"a\" MyInt@ instance using @Coercible Int MyInt@.
newtype Coerce (to :: *) m (a :: *) = Coerce (m a)
  deriving (Functor, Applicative, Monad, MonadIO, PrimMonad)

-- | Rename the tag.
--
-- Example:
--
-- @
-- newtype MyReader a = MyReader (Reader (Int, Bool) a)
--   deriving (HasReader "foo" Int) via
--     Rename 1 (Pos 1 () (MonadReader (Reader (Int, Bool))))
-- @
--
-- Converts the @HasReader 1 Int@ instance of
-- @Pos 1 () (MonadReader (Reader (Int, Bool)))@ to a
-- @HasReader \"foo\" Int@ instance by renaming the tag.
--
-- Note, that @MonadReader@ itself does not fix a tag, and @Rename@ would
-- be redundant if it was applied directly to @MonadReader@.
-- This example demonstrates a very common use-case, which is to create a
-- more descriptive name than the number tag implied by @Pos@.
newtype Rename (oldtag :: k) m (a :: *) = Rename (m a)
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
-- Converts the @HasReader () Foo@ instance of @MonadReader (Reader Foo)@ to a
-- @HasReader \"foo\" Int@ instance by focusing on the field @foo@ in
-- the @Foo@ record.
--
-- See 'Rename' for a way to change the tag.
newtype Field (field :: Symbol) (oldtag :: k) m (a :: *) = Field (m a)
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
-- Converts the @HasReader () (Int, Bool)@ instance of
-- @MonadReader (Reader (Int, Bool))@ to a @HasReader 1 Int@ instance
-- by focusing on the first element of the tuple.
--
-- See 'Rename' for a way to change the tag.
newtype Pos (pos :: Nat) (oldtag :: k) m (a :: *) = Pos (m a)
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
-- Converts the @HasThrow () \"MyError\"@ instance of
-- @MonadError (ExceptT MyError Identity)@ to a
-- @HasThrow \"ErrB\" String@ instance by wrapping thrown @String@s
-- in the @ErrB@ constructor.
newtype Ctor (ctor :: Symbol) (oldtag :: k) m (a :: *) = Ctor (m a)
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
-- Uses the @MonadTrans@ instance of @StateT Int@ to lift
-- the @HasState "\foo\" Bool@ instance of the underlying
-- @MonadState (State Bool)@ over the @StateT Int@ monad transformer.
newtype Lift m (a :: *) = Lift (m a)
  deriving (Functor, Applicative, Monad, MonadIO, PrimMonad)


-- | Compose two accessors.
--
-- This is not necessary in deriving via clauses, but in places where a
-- transformer is expected as a type argument. E.g. 'HasError.wrapError'.
newtype (:.:)
  (t2 :: (* -> *) -> * -> *)
  (t1 :: (* -> *) -> * -> *)
  (m :: * -> *)
  (a :: *)
  = (:.:) (m a)
  deriving (Functor, Applicative, Monad, MonadIO, PrimMonad)
infixr 9 :.:
