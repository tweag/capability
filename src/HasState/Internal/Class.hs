{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeInType #-}

module HasState.Internal.Class
  ( HasState (..)
  , get
  , put
  , state
  , modify
  , modify'
  , gets
  , zoom
  ) where

import Data.Coerce (Coercible, coerce)
import GHC.Exts (Proxy#, proxy#)


class Monad m
  => HasState (tag :: k) (s :: *) (m :: * -> *) | tag m -> s
  where
    get_ :: Proxy# tag -> m s
    put_ :: Proxy# tag -> s -> m ()
    state_ :: Proxy# tag -> (s -> (a, s)) -> m a

get :: forall tag s m. HasState tag s m => m s
get = get_ (proxy# @_ @tag)
{-# INLINE get #-}

put :: forall tag s m. HasState tag s m => s -> m ()
put = put_ (proxy# @_ @tag)
{-# INLINE put #-}

state :: forall tag s m a. HasState tag s m => (s -> (a, s)) -> m a
state = state_ (proxy# @_ @tag)
{-# INLINE state #-}

modify :: forall tag s m. HasState tag s m => (s -> s) -> m ()
modify f = state @tag $ \s -> ((), f s)
{-# INLINE modify #-}

modify' :: forall tag s m. HasState tag s m => (s -> s) -> m ()
modify' f = do
  s' <- get @tag
  put @tag $! f s'
{-# INLINE modify' #-}

gets :: forall tag s m a. HasState tag s m => (s -> a) -> m a
gets f = do
  s <- get @tag
  pure (f s)
{-# INLINE gets #-}

-- | Execute the given state action on a sub-component of the current state
-- as defined by the given transformer @t@.
--
-- Example:
--
-- > zoom @"foobar" @"foo" @(Field "foo" "foobar") foo
-- >   :: HasState "foobar" FooBar m => m ()
-- >
-- > foo :: HasState "foo" Int m => m ()
-- > data FooBar = FooBar { foo :: Int, bar :: String }
zoom :: forall outertag innertag t outer inner m a.
  ( forall x. Coercible (t m x) (m x)
  , forall m'. HasState outertag outer m'
    => HasState innertag inner (t m')
  , HasState outertag outer m )
  => (forall m'. HasState innertag inner m' => m' a) -> m a
zoom m = coerce @(t m a) m
{-# INLINE zoom #-}
