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
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Example uses and instances of the @HasError@ capability.
module Error where

import Control.Monad (when)
import Control.Monad.Except (ExceptT (..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import GHC.Generics (Generic)
import Test.Hspec
import Text.Read (readEither)

import HasError

import Test.Common


----------------------------------------------------------------------
-- Example Programs

-- Calculator Example ------------------------------------------------

data ParserError
  = InvalidInput String
  deriving (Show, Typeable)
  deriving anyclass Exception

parseNumber :: HasThrow "ParserError" ParserError m
  => String -> m Int
parseNumber input = case readEither input of
  Left err -> throw @"ParserError" $ InvalidInput err
  Right num -> pure num


data MathError
  = NegativeInput
  deriving (Show, Typeable)
  deriving anyclass Exception

sqrtNumber :: HasThrow "MathError" MathError m
  => Int -> m Int
sqrtNumber num
  | num < 0 = throw @"MathError" NegativeInput
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
--
-- The @HasCatch "calc"@ constraint is used to catch errors that can occur in
-- any of the components: "parser", or "math".
--
-- The @HasThrow "parser"/"math"@ constraints are imposed by the components.
--
-- XXX: It might be preferable to have a combinator that eliminates the
--   @HasThrow "parser"/"math"@ constraints by wrapping the corresponding
--   exceptions with @ParserError/MathError@ and renaming the tags.
--   @calculator@ would then have just one @HasThrow "calc"@ constraint.
calculator ::
  ( HasThrow "ParserError" ParserError m
  , HasThrow "MathError" MathError m
  , HasCatch "CalcError" CalcError m, MonadIO m )
  => m ()
calculator = do
  liftIO $ putStr "Enter positive number or 'Q' to quit\n> "
  line <- liftIO getLine
  case line of
    "Q" -> pure ()
    input -> do
      catch @"CalcError"
        do
          num <- parseNumber input
          root <- sqrtNumber num
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
--
-- @ParserError@s are wrapped as @CalcError@s using the @ParserError@
-- constructor, and @MathError@s are wrapped as @CalcError@s using the
-- @MathError@ constructor.
newtype Calculator a = Calculator { runCalculator :: IO a }
  deriving newtype (Functor, Applicative, Monad, MonadIO)
  deriving (HasThrow "ParserError" ParserError) via
    Ctor "ParserError" "CalcError" (MonadUnliftIO CalcError IO)
  deriving (HasThrow "MathError" MathError) via
    Ctor "MathError" "CalcError" (MonadUnliftIO CalcError IO)
  deriving (HasThrow "CalcError" CalcError) via
    MonadUnliftIO CalcError IO
  deriving (HasCatch "CalcError" CalcError) via
    MonadUnliftIO CalcError IO


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
