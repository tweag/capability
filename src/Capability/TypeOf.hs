{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}

module Capability.TypeOf where

-- | Type family associating a tag to the corresponding type. It is intended to
-- simplify constraint declarations, by removing the need to redundantly specify
-- the type associated to a tag.
--
-- It is poly-kinded, which allows users to define their own type of tags, and
-- not have to declare orphan type family instances.
--
-- Example:
-- @
--     {-# LANGUAGE DataKinds #-}
--     {-# LANGUAGE TypeFamilies #-}
--
--     import Capability.Reader
--
--     data Tag = Foo | Bar
--     type instance TypeOf 'Foo = Int
--     type instance TypeOf 'Tag = String
--
--     -- Same as: foo :: HasReader Foo Int M => …
--     foo :: HasReader' 'Foo m => …
--     foo = …
-- @
type family TypeOf k (s :: k) :: *
