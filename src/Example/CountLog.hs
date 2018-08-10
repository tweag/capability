{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}

module Example.CountLog where

import Control.Monad.IO.Class
import Control.Monad.Reader (ReaderT (..), runReaderT)
-- The @StateT@ constructor has to be imported even though it is not used
-- explicitly. Otherwise, the deriving via of @Counter CounterM@ would fail.
import Control.Monad.State.Strict (State, StateT (..), runState)
import qualified Data.Char
import Data.Coerce (coerce)
import Data.IORef
import Data.Monoid (Sum (..))
import GHC.Generics (Generic)

import HasReader
import HasState
import HasWriter


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


newtype LogM m (a :: *) = LogM (ReaderT LogCtx m a)
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
  deriving Counter via
    TheCounterState (MonadState (State Int))

runCounterM :: CounterM a -> (a, Int)
runCounterM (CounterM m) = runState m 0

-- ReaderT IORef instance --------------------------------------------

newtype Counter'M m (a :: *) = Counter'M (ReaderT (IORef Int) m a)
  deriving (Functor, Applicative, Monad)
  deriving Counter via
    TheCounterState (ReaderIORef
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


newtype CountLogM m (a :: *) = CountLogM (ReaderT CountLogCtx m a)
  deriving (Functor, Applicative, Monad)
  deriving Counter via
    (TheCounterState (ReaderIORef
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


----------------------------------------------------------------------
-- Multiple Tagged States

-- Example program ---------------------------------------------------

twoStates :: (HasState "foo" Int m, HasState "bar" Int m) => m ()
twoStates = do
  modify @"foo" (+1)
  modify @"bar" (subtract 1)

-- StateT instance ---------------------------------------------------

data TwoStates = TwoStates
  { tsFoo :: Int
  , tsBar :: Int
  } deriving (Generic, Show)


newtype TwoStatesM a = TwoStatesM (State TwoStates a)
  deriving (Functor, Applicative, Monad)
  deriving (HasState "foo" Int) via
    Field "tsFoo" (MonadState (State TwoStates))
  deriving (HasState "bar" Int) via
    Field "tsBar" (MonadState (State TwoStates))

runTwoStatesM :: TwoStatesM a -> (a, TwoStates)
runTwoStatesM (TwoStatesM m) = runState m TwoStates
  { tsFoo = 0
  , tsBar = 0
  }


----------------------------------------------------------------------
-- Writer Monad

-- Example programs --------------------------------------------------

-- | Increase a counter using a writer monad.
useWriter :: HasWriter "count" (Sum Int) m => m ()
useWriter = do
  tell @"count" 1
  tell @"count" 2
  tell @"count" 3

-- | Mix writer and state monad operations on the same tag.
--
-- Note, this is probably inadvisable in real applications and just here to
-- demonstrate that it is possible.
mixWriterState
  :: (HasState "count" (Sum Int) m, HasWriter "count" (Sum Int) m)
  => m Int
mixWriterState = do
  tell @"count" 1
  one <- get @"count"
  tell @"count" one
  pure $ getSum one

-- StateT instance ---------------------------------------------------

newtype WriterM a = WriterM (State (Sum Int) a)
  deriving (Functor, Applicative, Monad)
  -- XXX: @HasWriter@ requires a @Monoid@, but @Int@ has no canonical instance.
  --   We choose the monoid instance using the @Sum@ newtype.
  --   We may want to add a @Coerce@ combinator similar to @Field@ so that
  --   @WriterM@ can be implemented in terms of @State Int@, and the deriving
  --   clause can choose the monoid instance.
  deriving (HasWriter "count" (Sum Int))
    via WriterLog (MonadState (State (Sum Int)))
  -- See caveat on 'mixWriterState'.
  deriving (HasState "count" (Sum Int))
    via MonadState (State (Sum Int))

runWriterM :: WriterM a -> (a, Int)
runWriterM (WriterM m) = getSum <$> runState m 0
