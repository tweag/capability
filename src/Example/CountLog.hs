{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Example.CountLog where

import Control.Monad.IO.Class
import Control.Monad.Reader (ReaderT, runReaderT)
import qualified Data.Char
import Data.Coerce (coerce)
import Data.IORef
import GHC.Generics (Generic)

import Has
import HasReader


class Monad m => Counter m where
  count :: m Int

class Monad m => Logger m where
  logStr :: String -> m ()


-- | Count twice.
doubleCount :: Counter m => m Int
doubleCount = count >> count

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
