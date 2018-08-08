{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Example.CountLog where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.State.Strict (State, StateT, runState, runStateT)
import qualified Data.Char
import Data.Coerce (coerce)
import Data.IORef
import GHC.Generics (Generic)

import Has
import HasReader
import HasState


-- | Capability to keep a running counter.
class Monad m => Counter m where
  count :: m Int

newtype TheCounterState m a = TheCounterState (m a)
  deriving (Functor, Applicative, Monad)
-- | An integer state can server as a counter.
instance
  (HasState "counter" Int m, Monad m)
  => Counter (TheCounterState m)
  where
    count = coerce @(m Int) $
      state @"counter" $ \n -> let !n' = n + 1 in (n', n')


-- | Use a counter to count up twice.
doubleCount :: Counter m => m Int
doubleCount = count >> count


-- XXX: Using just @StateT Int m a@ makes deriving via fail. Can we fix that?
newtype CounterM a = CounterM (State (TheValue Int) a)
  deriving (Functor, Applicative, Monad)
deriving via (TheCounterState (TheMonadState (State (TheValue Int))))
  instance Counter CounterM

runCounterM :: CounterM a -> (a, Int)
runCounterM (CounterM m) = runState m (TheValue 0) & _2 %~ theValue


class Monad m => Logger m where
  logStr :: String -> m ()

-- | Log the given number.
logNum :: Logger m => Int -> m ()
logNum = logStr . ("num: " ++) . show

-- | Double count and log the result, repeat once.
program :: (Counter m, Logger m) => m ()
program = do
  doubleCount >>= logNum
  doubleCount >>= logNum


data CountCtx = CountCtx { counter :: IORef Int }
deriving via (TheValue CountCtx)
  instance Has "counter" CountCtx CountCtx


data LogCtx = LogCtx { logger :: String -> IO () }
deriving via (TheValue LogCtx)
  instance Has "log" LogCtx LogCtx

newtype TheLogCtxReader m a = TheLogCtxReader (m a)
  deriving (Functor, Applicative, Monad)
instance
  (HasReader "log" LogCtx m, MonadIO m)
  => Logger (TheLogCtxReader m)
  where
    logStr msg =
      coerce (asks @"log" logger >>= liftIO . ($ msg) :: m ())


data CountLogCtx = CountLogCtx
  { countCtx :: CountCtx
  , logCtx :: LogCtx
  } deriving Generic
deriving via (TheField "countCtx" CountLogCtx)
  instance Has "counter" CountCtx CountLogCtx
deriving via (TheField "logCtx" CountLogCtx)
  instance Has "log" LogCtx CountLogCtx


mkCounter :: MonadIO m => m CountCtx
mkCounter = do
  ref <- liftIO $ newIORef 0
  pure CountCtx { counter = ref }

regularLogger :: LogCtx
regularLogger = LogCtx { logger = putStrLn }

loudLogger :: LogCtx
loudLogger = LogCtx { logger = putStrLn . map Data.Char.toUpper }


newtype LogM m a = LogM (ReaderT LogCtx m a)
  deriving (Functor, Applicative, Monad)
deriving via (TheLogCtxReader (ReaderT LogCtx (m :: * -> *)))
  instance MonadIO m => Logger (LogM m)
