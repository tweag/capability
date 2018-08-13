{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}

module HasState.Ref
  ( ReaderIORef (..)
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef

import HasReader -- Used for ReaderIORef below
import HasState


newtype ReaderIORef m a = ReaderIORef (m a)
  deriving (Functor, Applicative, Monad)
instance
  (HasReader tag (IORef s) m, MonadIO m)
  => HasState tag s (ReaderIORef m)
  where
    get_ _ = ReaderIORef $ do
      ref <- ask @tag
      liftIO $ readIORef ref
    put_ _ v = ReaderIORef $ do
      ref <- ask @tag
      liftIO $ writeIORef ref v
    state_ _ f = ReaderIORef $ do
      ref <- ask @tag
      liftIO $ atomicModifyIORef' ref (swap . f)
      where
        swap (a, b) = (b, a)
