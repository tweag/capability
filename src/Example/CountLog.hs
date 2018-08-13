{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
import Control.Monad.State.Strict (State, StateT (..), evalStateT, runState)
import qualified Data.Char
import Data.Coerce (coerce)
import Data.IORef
import Data.Monoid (Sum (..))
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Streaming (Stream, Of)
import qualified Streaming.Prelude as S

import HasReader
import HasState
import HasStream
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

-- Nested StateT instance --------------------------------------------

-- Note, that this is not the recommended way to provide multiple `HasState`
-- capabilities. Use the approach shown above in 'TwoStatesM' instead. However,
-- this pattern can be useful to transation existing code to this library.
newtype NestedStatesM a = NestedStatesM (StateT Int (State Int) a)
  deriving (Functor, Applicative, Monad)
  deriving (HasState "foo" Int) via MonadState (StateT Int (State Int))
  deriving (HasState "bar" Int) via Lift (StateT Int (MonadState (State Int)))

runNestedStatesM :: NestedStatesM a -> ((a, Int), Int)
runNestedStatesM (NestedStatesM m) = runState (runStateT m 0) 0


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
  :: (HasState "count" Int m, HasWriter "count" (Sum Int) m)
  => m Int
mixWriterState = do
  tell @"count" 1
  one <- get @"count"
  tell @"count" $ Sum one
  pure one

-- StateT instance ---------------------------------------------------

newtype WriterM a = WriterM (State Int a)
  deriving (Functor, Applicative, Monad)
  deriving (HasWriter "count" (Sum Int))
    via WriterLog (Coerce (Sum Int) (MonadState (State Int)))
  -- See caveat on 'mixWriterState'.
  deriving (HasState "count" Int)
    via MonadState (State Int)

runWriterM :: WriterM a -> (a, Int)
runWriterM (WriterM m) = runState m 0


----------------------------------------------------------------------
-- Streaming Capability

-- Example program ---------------------------------------------------

iota :: HasStream "nums" Int m => Int -> m ()
iota n
  | n < 0 = error "negative number passed to iota."
  | otherwise = go 0
  where
    go i
      | i == n = pure ()
      | otherwise = yield @"nums" i >> go (succ i)

labelledNodes
  :: (HasState "counter" Int m, HasStream "out" (Int, a) m, Foldable t)
  => t a -> m ()
labelledNodes = mapM_ $ \a -> do
  n <- state @"counter" $ \n -> (n, succ n)
  yield @"out" (n, a)

-- StateT instance ---------------------------------------------------

newtype StreamAccM a = StreamAccM (State [Int] a)
  deriving (Functor, Applicative, Monad)
  deriving (HasStream "nums" Int) via
    StreamStack (MonadState (State [Int]))

runStreamAccM :: StreamAccM a -> (a, [Int])
runStreamAccM (StreamAccM m) = runState m []

-- Stream instance ---------------------------------------------------

printStreamOfInt :: Stream (Of Int) IO () -> IO ()
printStreamOfInt = S.stdoutLn . S.map show

-- StateT Stream instance --------------------------------------------

newtype StateOverStream a =
  StateOverStream (StateT Int (Stream (Of (Int, Char)) IO) a)
  deriving (Functor, Applicative, Monad)
  deriving (HasState "counter" Int) via
    MonadState (StateT Int (Stream (Of (Int, Char)) IO))
  deriving (HasStream "out" (Int, Char)) via
    Lift (StateT Int (Stream (Of (Int, Char)) IO))

printStateOverStream :: StateOverStream () -> IO ()
printStateOverStream (StateOverStream m) = do
  S.stdoutLn . S.map show $ evalStateT m 0

printLabelledNodes :: IO ()
printLabelledNodes =
  printStateOverStream $ labelledNodes $
    Set.fromList "Hello world!"
