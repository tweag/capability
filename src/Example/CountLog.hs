{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Example.CountLog where

import Control.Monad.IO.Class
import Control.Monad.Reader (ReaderT (..), runReaderT)
-- The @StateT@ constructor has to be imported even though it is not used
-- explicitly. Otherwise, the deriving via of @Counter CounterM@ would fail.
import Control.Monad.State.Strict (State, StateT (..), runState)
import qualified Data.Char
import Data.Coerce (coerce)
import Data.Functor.Identity (Identity (..))
import Data.IORef
import GHC.Generics (Generic)

import Has
import HasReader
import HasState


----------------------------------------------------------------------
-- Logger Capability


class Monad m => Logger m where
  logStr :: String -> m ()

-- | Any @HasReader "logger" (String -> IO ())@ can be a @Logger@.
newtype TheLoggerReader m a = TheLoggerReader (m a)
  deriving (Functor, Applicative, Monad)
instance
  (HasReader "logger" (String -> IO ()) m, MonadIO m)
  => Logger (TheLoggerReader m)
  where
    logStr msg =
      coerce (ask @"logger" >>= liftIO . ($ msg) :: m ())

-- Example program ---------------------------------------------------

-- | Log the given number.
logNum :: Logger m => Int -> m ()
logNum = logStr . ("num: " ++) . show

-- ReaderT instance --------------------------------------------------

data LogCtx = LogCtx { logger :: String -> IO () }
  deriving Generic

regularLogger :: LogCtx
regularLogger = LogCtx { logger = putStrLn }

loudLogger :: LogCtx
loudLogger = LogCtx { logger = putStrLn . map Data.Char.toUpper }


newtype LogM m a = LogM (ReaderT LogCtx m a)
  deriving (Functor, Applicative, Monad)
  deriving Logger via
    (TheLoggerReader (Field "logger"
    (MonadReader (ReaderT LogCtx m))))

runLogM :: LogCtx -> LogM m a -> m a
runLogM ctx (LogM m) = runReaderT m ctx


----------------------------------------------------------------------
-- Counter Capability


class Monad m => Counter m where
  count :: m Int

-- | Any @HasState "counter" Int m@ can be a @Counter@.
newtype TheCounterState m a = TheCounterState (m a)
  deriving (Functor, Applicative, Monad)
instance
  (HasState "counter" Int m, Monad m)
  => Counter (TheCounterState m)
  where
    count = coerce @(m Int) $
      state @"counter" $ \n -> let !n' = n + 1 in (n', n')

-- Example program ---------------------------------------------------

-- | Use a counter to count up twice.
doubleCount :: Counter m => m Int
doubleCount = count >> count

-- StateT instance ---------------------------------------------------

newtype CounterM a = CounterM (State Int a)
  deriving (Functor, Applicative, Monad)
  deriving Counter via TheCounterState (State (TheValue Int))

runCounterM :: CounterM a -> (a, Int)
runCounterM (CounterM m) = runState m 0

-- ReaderT IORef instance --------------------------------------------

newtype Counter'M m a = Counter'M (ReaderT (IORef Int) m a)
  deriving (Functor, Applicative, Monad)
  deriving Counter via
    TheCounterState (TheReaderIORef
    (MonadReader (ReaderT (IORef Int) m)))

runCounter'M :: MonadIO m => Counter'M m a -> m a
runCounter'M (Counter'M m) = runReaderT m =<< liftIO (newIORef 0)


----------------------------------------------------------------------
-- Mixed Capabilities

-- Example program ---------------------------------------------------

-- | Double count and log the result, repeat once.
mixed :: (Counter m, Logger m) => m ()
mixed = do
  doubleCount >>= logNum
  doubleCount >>= logNum

-- ReaderT instance --------------------------------------------------

data CountLogCtx = CountLogCtx
  { countCtx :: IORef Int
  , logCtx :: LogCtx
  } deriving Generic


newtype CountLogM m a = CountLogM (ReaderT CountLogCtx m a)
  deriving (Functor, Applicative, Monad)
  deriving Counter via
    (TheCounterState (TheReaderIORef
    (Field "countCtx" (MonadReader (ReaderT CountLogCtx m)))))
  -- XXX: This requires @Field@ and @MonadReader@ to have @MonadIO@ instances.
  --   That seems anti-modular - if a user-defined constraint is required,
  --   they may have to add orphan instances for @Field@ and @MonadReader@.
  deriving Logger via
    (TheLoggerReader (Field "logger" (Field "logCtx"
    (MonadReader (ReaderT CountLogCtx m)))))

runCountLogM :: MonadIO m => CountLogM m b -> m b
runCountLogM (CountLogM m) = do
  ref <- liftIO $ newIORef 0
  runReaderT m CountLogCtx
    { countCtx = ref
    , logCtx = regularLogger
    }
