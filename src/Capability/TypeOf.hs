{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}

module Capability.TypeOf where

-- | Type family associating a tag to the corresponding type. It is intended to
-- simplify constraint declarations, by removing the need to redundantly specify
-- the type associated to a tag.
--
-- It is poly-kinded, which allows users to define their own kind of tags.
-- Standard haskell types can also be used as tags by specifying the '*' kind
-- when defining the type family instance.
--
-- Defining 'TypeOf' instances for 'GHC.TypeLits.Symbol's (typelevel string
-- literals) is discouraged. Since symbols all belong to the same global
-- namespace, such instances could conflict with others defined in external
-- libraries. More generally, as for typeclasses, 'TypeOf' instances should
-- always be defined in the same module as the tag type to prevent issues due to
-- orphan instances.
--
-- Example:
-- @
--     {-# LANGUAGE EmptyDataDecls #-}
--     {-# LANGUAGE TypeFamilies #-}
--
--     import Capability.Reader
--
--     data Foo
--     data Bar
--     type instance TypeOf * Foo = Int
--     type instance TypeOf * Bar = String
--
--     -- Same as: foo :: HasReader Foo Int M => …
--     foo :: HasReader' Foo m => …
--     foo = …
-- @
type family TypeOf k (s :: k) :: *
