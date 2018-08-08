{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}

module Example.CountLog where

import Control.Monad.IO.Class
import qualified Data.Char
import Data.IORef

import Has


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
  instance Has CountCtx CountCtx CountCtx

data LogCtx m = LogCtx { logger :: String -> m () }

data CountLogCtx m = CountLogCtx
  { countCtx :: CountCtx
  , logCtx :: LogCtx m
  }


mkCounter :: MonadIO m => m CountCtx
mkCounter = do
  ref <- liftIO $ newIORef 0
  pure CountCtx { counter = ref }

regularLogger :: MonadIO m => LogCtx m
regularLogger = LogCtx { logger = liftIO . putStrLn }

loudLogger :: MonadIO m => LogCtx m
loudLogger = LogCtx { logger = liftIO . putStrLn . map Data.Char.toUpper }
