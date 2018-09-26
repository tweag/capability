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
