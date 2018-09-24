{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
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
import Test.Hspec

import HasReader
import HasState
import HasWriter

import Test.Common


-- | Accumulating key-value mapping.
--
-- The 'Monoid' instance will 'mappend'
-- values to keys that occur on both sides.
newtype Accum map = Accum map

instance (Ord k, Semigroup v)
  => Semigroup (Accum (Map k v)) where
    (<>) = coerce $ Map.unionWith @k @v (<>)

instance (Ord k, Semigroup v)
  => Monoid (Accum (Map k v)) where
    mempty = coerce $ Map.empty @k @v
    mappend = (<>)


-- | Counts occurrences of values @k@.
newtype Occurrences k = Occurrences (Map k Int)
  deriving (Monoid, Semigroup) via Accum (Map k (Sum Int))
  deriving Show

-- | A single occurrence of the given value.
oneOccurrence :: k -> Occurrences k
oneOccurrence k = Occurrences $ Map.singleton k 1


-- | Count the occurrence of a single letter.
countLetter ::
  HasWriter "letterCount" (Occurrences Char) m
  => Char -> m ()
countLetter letter = tell @"letterCount" (oneOccurrence letter)

-- | Count the occurrence of a single word.
countWord ::
  HasWriter "wordCount" (Occurrences Text) m
  => Text -> m ()
countWord word = tell @"wordCount" (oneOccurrence word)


-- | Count the occurrence of a single word and all the letters in it.
countWordAndLetters ::
  ( HasWriter "letterCount" (Occurrences Char) m
  , HasWriter "wordCount" (Occurrences Text) m )
  => Text -> m ()
countWordAndLetters word = do
  countWord word
  mapM_ countLetter (Text.unpack word)


-- | Count the occurrences of words and letters in a text,
-- excluding white space.
countWordsAndLettersInText ::
  ( HasWriter "letterCount" (Occurrences Char) m
  , HasWriter "wordCount" (Occurrences Text) m )
  => Text -> m ()
countWordsAndLettersInText text =
  mapM_ countWordAndLetters (Text.words text)


-- | Counter application context.
data CounterCtx = CounterCtx
  { letterCount :: IORef (Occurrences Char)
    -- ^ Counting letter occurrences.
  , wordCount :: IORef (Occurrences Text)
    -- ^ Counting word occurrences.
  } deriving Generic

-- | Counter application monad.
newtype Counter a = Counter { runCounter :: CounterCtx -> IO a }
  deriving (Functor, Applicative, Monad) via (ReaderT CounterCtx IO)
  deriving (HasWriter "letterCount" (Occurrences Char)) via
    (WriterLog  -- Generate HasWriter using HasState of Monoid
    (ReaderIORef  -- Generate HasState from HasReader of IORef
    (Field "letterCount" "ctx"  -- Focus on the field letterCount
    (MonadReader  -- Generate HasReader using mtl MonadReader
    (ReaderT CounterCtx IO)))))  -- Use mtl ReaderT newtype
  deriving (HasWriter "wordCount" (Occurrences Text)) via
    WriterLog (ReaderIORef
    (Field "wordCount" "ctx" (MonadReader (ReaderT CounterCtx IO))))


-- | Given a text count the occurrences of all words and letters in it,
-- excluding white space, and print the outcome to standard output.
wordAndLetterCount :: Text -> IO ()
wordAndLetterCount text = do
  lettersRef <- newIORef $ Occurrences Map.empty
  wordsRef <- newIORef $ Occurrences Map.empty
  let ctx = CounterCtx
        { letterCount = lettersRef
        , wordCount = wordsRef
        }
      counter :: Counter ()
      counter = countWordsAndLettersInText text
  runCounter counter ctx
  let printOccurrencesOf name ref = do
        putStrLn name
        Occurrences occurrences <- readIORef ref
        ifor_ occurrences $ \item num ->
          putStrLn $ show item ++ ": " ++ show num
  printOccurrencesOf "Letters" lettersRef
  printOccurrencesOf "Words" wordsRef


----------------------------------------------------------------------
-- Test Cases

spec :: Spec
spec = do
  describe "Counter" $ do
    it "handles the empty text" $
      wordAndLetterCount "" `shouldPrint`
        "Letters\n\
        \Words\n"
    it "handles one word" $
      wordAndLetterCount "banana" `shouldPrint`
        "Letters\n'a': 3\n'b': 1\n'n': 2\n\
        \Words\n\"banana\": 1\n"
    it "handles two words" $
      wordAndLetterCount "mississipi river" `shouldPrint`
        "Letters\n'e': 1\n'i': 5\n'm': 1\n'p': 1\n'r': 2\n's': 4\n'v': 1\n\
        \Words\n\"mississipi\": 1\n\"river\": 1\n"
    it "handles two lines" $
      wordAndLetterCount "banana apple\napple banana" `shouldPrint`
        "Letters\n'a': 8\n'b': 2\n'e': 2\n'l': 2\n'n': 4\n'p': 4\n\
        \Words\n\"apple\": 2\n\"banana\": 2\n"
