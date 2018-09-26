{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Example uses and instances of the @HasError@ capability.
module Error where

import Capability.Error
import Control.Monad (when)
import Control.Monad.Except (ExceptT (..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import GHC.Generics (Generic)
import Test.Common
import Test.Hspec
import Text.Read (readEither)

----------------------------------------------------------------------
-- Example Programs

-- Calculator Example ------------------------------------------------

data ParserError
  = InvalidInput String
  deriving (Show, Typeable)
  deriving anyclass Exception

parseNumber :: HasThrow "parser" ParserError m
  => String -> m Int
parseNumber input = case readEither input of
  Left err -> throw @"parser" $ InvalidInput err
  Right num -> pure num


data MathError
  = NegativeInput
  deriving (Show, Typeable)
  deriving anyclass Exception

sqrtNumber :: HasThrow "math" MathError m
  => Int -> m Int
sqrtNumber num
  | num < 0 = throw @"math" NegativeInput
  | otherwise = pure $ round $ sqrt @Double $ fromIntegral num


-- | Errors that can occur in the calculator application.
data CalcError
    -- | The parser component failed.
  = ParserError ParserError
    -- | The math component failed.
  | MathError MathError
  deriving (Generic, Show, Typeable)
  deriving anyclass Exception

-- | Calculator application
--
-- Prompts for positive numbers and prints their square roots.
calculator ::
  ( HasCatch "calc" CalcError m, MonadIO m )
  => m ()
calculator = do
  liftIO $ putStr "Enter positive number or 'Q' to quit\n> "
  line <- liftIO getLine
  case line of
    "Q" -> pure ()
    input -> do
      catch @"calc"
        do
          -- Errors in the parser or math component are converted to a
          -- @CalcError@ by wrapping with the corresponding constructor.
          let wrapParserError = wrapError @"calc" @"parser"
                @(Rename "ParserError" :.: Ctor "ParserError" "calc")
              wrapMathError = wrapError @"calc" @"math"
                @(Rename "MathError" :.: Ctor "MathError" "calc")
          num <- wrapParserError $ parseNumber input
          root <- wrapMathError $ sqrtNumber num
          liftIO $ putStrLn $ "sqrt = " ++ show root
        \e -> liftIO $ putStrLn $ "Error: " ++ show e
      calculator


-- Nested Example ----------------------------------------------------

nested :: (HasThrow "foo" String m, HasThrow "bar" () m) => Int -> m Int
nested n = do
  when (n < 0) $
    throw @"foo" "negative number"
  when (odd n) $
    throw @"bar" ()
  pure n


----------------------------------------------------------------------
-- Instances

-- | Deriving @HasThrow/HasCatch@ from @unliftio@.
newtype Calculator a = Calculator { runCalculator :: IO a }
  deriving newtype (Functor, Applicative, Monad, MonadIO)
  deriving
    ( HasThrow "calc" CalcError
    , HasCatch "calc" CalcError
    ) via MonadUnliftIO CalcError IO


-- | Deriving separate @HasThrow@ capabilities from different transformer
-- layers.
newtype MaybeEither a =
  MaybeEither { runMaybeEither :: Maybe (Either String a) }
  deriving (Functor, Applicative, Monad) via
    ExceptT String Maybe
  deriving (HasThrow "foo" String) via
    MonadError (ExceptT String Maybe)
  deriving (HasThrow "bar" ()) via
    Lift (ExceptT String (MonadError Maybe))


----------------------------------------------------------------------
-- Test Cases

spec :: Spec
spec = do
  describe "Calculator" $
    it "evaluates calculator" $ do
      let input = "4\n-1\nxyz\nQ\n"
          output =
            "Enter positive number or 'Q' to quit\n\
            \> sqrt = 2\n\
            \Enter positive number or 'Q' to quit\n\
            \> Error: MathError NegativeInput\n\
            \Enter positive number or 'Q' to quit\n\
            \> Error: ParserError (InvalidInput \"Prelude.read: no parse\")\n\
            \Enter positive number or 'Q' to quit\n\
            \> "
      runCalculator calculator
        `withInput` input
        `shouldPrint` output
  describe "MaybeEither" $
    it "evaluates nested" $ do
      runMaybeEither (nested 2) `shouldBe` Just (Right 2)
      runMaybeEither (nested (-1)) `shouldBe` Just (Left "negative number")
      runMaybeEither (nested 1) `shouldBe` Nothing
