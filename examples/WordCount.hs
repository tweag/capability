{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeInType #-}

module WordCount where

import Control.Lens (ifor_)
import Data.Coerce (coerce)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Control.Monad.Reader (ReaderT (..))
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Monoid (Sum (..))
import GHC.Generics (Generic)

import HasReader
import HasState
import HasWriter


-- | Accumulating key-value mapping.
--
-- The 'Monoid' instance will 'mappend'
-- values to keys that occur on both sides.
newtype Accum map = Accum map

instance (Ord k, Semigroup v)
  => Monoid (Accum (Map k v)) where
    mempty = coerce $ Map.empty @k @v
    mappend = coerce $ Map.unionWith @k @v (<>)

instance (Ord k, Semigroup v)
  => Semigroup (Accum (Map k v)) where
    (<>) = mappend


newtype Occurrences k = Occurrences (Map k Int)
  deriving (Monoid, Semigroup) via Accum (Map k (Sum Int))
  deriving Show
oneOccurrence :: Ord k => k -> Occurrences k
oneOccurrence k = Occurrences $ Map.singleton k 1


countLetter ::
  HasWriter "letterCount" (Occurrences Char) m
  => Char -> m ()
countLetter letter = tell @"letterCount" (oneOccurrence letter)

countWord ::
  HasWriter "wordCount" (Occurrences Text) m
  => Text -> m ()
countWord word = tell @"wordCount" (oneOccurrence word)


countWordAndLetters ::
  ( HasWriter "letterCount" (Occurrences Char) m
  , HasWriter "wordCount" (Occurrences Text) m )
  => Text -> m ()
countWordAndLetters word = do
  countWord word
  mapM_ countLetter (Text.unpack word)


data CounterCtx = CounterCtx
  { letterCount :: IORef (Map Char Int)
  , wordCount :: IORef (Map Text Int)
  } deriving Generic
newtype Counter a = Counter { runCounter :: CounterCtx -> IO a }
  deriving (Functor, Applicative, Monad) via (ReaderT CounterCtx IO)
  deriving (HasWriter "letterCount" (Occurrences Char)) via
    (WriterLog  -- Generate HasWriter using HasState of Monoid
    (Coerce (Occurrences Char)  -- Coerce Map to Occurrences
    (ReaderIORef  -- Generate HasState from HasReader of IORef
    (Field "letterCount" ()  -- Focus on the field letterCount
    (MonadReader  -- Generate HasReader using mtl MonadReader
    (ReaderT CounterCtx IO))))))  -- Use mtl ReaderT newtype
  deriving (HasWriter "wordCount" (Occurrences Text)) via
    (WriterLog (Coerce (Occurrences Text) (ReaderIORef
    (Field "wordCount" () (MonadReader (ReaderT CounterCtx IO))))))


wordAndLetterCount :: Text -> IO ()
wordAndLetterCount text = do
  lettersRef <- newIORef Map.empty
  wordsRef <- newIORef Map.empty
  let ctx = CounterCtx
        { letterCount = lettersRef
        , wordCount = wordsRef
        }
      counter = mapM_ countWordAndLetters (Text.words text)
  runCounter counter ctx
  let printOccurrencesOf name ref = do
        putStrLn name
        occurrences <- readIORef ref
        ifor_ occurrences $ \item num ->
          putStrLn $ show item ++ ": " ++ show num
  printOccurrencesOf "Letters" lettersRef
  printOccurrencesOf "Words" wordsRef
