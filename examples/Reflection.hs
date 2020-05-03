{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Reflection where

import Capability.Error
import Capability.Reader
import Capability.Reflection
import Capability.Sink
import Capability.Source
import Capability.State
import Capability.Writer
import Control.Monad (forM_, replicateM)
import qualified Control.Monad.Except as MTL
import qualified Control.Monad.State as MTL
import Data.DList (DList)
import qualified Data.DList as DList
import Data.Monoid (Product (..))
import Test.Hspec

----------------------------------------------------------------------
-- Instances

newtype Logger s a = Logger (MTL.State (DList String, s) a)
  deriving (Functor, Applicative, Monad)
  deriving
    (HasSink "log" String)
    via SinkDList (SinkLog (Rename 1 (Pos 1 () (MonadState (MTL.State (DList String, s))))))
  deriving
    (HasState "test" s, HasSource "test" s, HasSink "test" s)
    via Rename 2 (Pos 2 () (MonadState (MTL.State (DList String, s))))

runLogger :: s -> Logger s a -> (a, s, [String])
runLogger s0 (Logger m) =
  case MTL.runState m (mempty, s0) of
    (a, (log', s)) -> (a, s, DList.toList log')

----------------------------------------------------------------------
-- Test Cases

spec :: Spec
spec = do
  describe "interpret_" $ do
    it "evaluates HasSource" $ do
      runLogger
        [1, 2, 3 :: Int]
        ( interpret_ @"source"
            ReifiedSource
              { _await = do
                  yield @"log" "await"
                  state @"test" $ \case
                    (x : xs) -> (x, xs)
                    [] -> error "empty stream"
              }
            (replicateM 3 $ await @"source")
        )
        `shouldBe` ([1, 2, 3], [], ["await", "await", "await"])
    it "evaluates HasSink" $ do
      runLogger
        []
        ( interpret_ @"sink"
            ReifiedSink
              { _yield = \x -> do
                  yield @"log" "yield"
                  modify @"test" (x :)
              }
            (forM_ [1, 2, 3 :: Int] $ yield @"sink")
        )
        `shouldBe` ((), [3, 2, 1], ["yield", "yield", "yield"])
    it "evaluates HasReader" $ do
      runLogger
        (42 :: Int)
        ( interpret_ @"reader"
            ReifiedReader
              { _readerSource =
                  ReifiedSource
                    { _await = do
                        yield @"log" "ask"
                        get @"test"
                    },
                _local = \f m -> do
                  yield @"log" "local"
                  r <- state @"test" $ \r -> (r, f r)
                  a <- m
                  put @"test" r
                  pure a,
                _reader = \f -> do
                  yield @"log" "reader"
                  f <$> get @"test"
              }
            ( sequence
                [ ask @"reader",
                  local @"reader" succ $ do
                    asks @"reader" succ,
                  reader @"reader" pred
                ]
            )
        )
        `shouldBe` ([42, 44, 41], 42, ["ask", "local", "ask", "reader"])
    it "evaluates HasState" $ do
      runLogger
        (42 :: Int)
        ( interpret_ @"state"
            ReifiedState
              { _stateSource =
                  ReifiedSource
                    { _await = do
                        yield @"log" "get"
                        get @"test"
                    },
                _stateSink =
                  ReifiedSink
                    { _yield = \x -> do
                        yield @"log" "put"
                        put @"test" x
                    },
                _state = \f -> do
                  yield @"log" "state"
                  state @"test" f
              }
            ( do
                old <- get @"state"
                put @"state" 0
                state @"state" (\new -> (old, succ new))
            )
        )
        `shouldBe` (42, 1, ["get", "put", "state"])
    it "evaluates HasWriter" $ do
      runLogger
        (mempty :: Product Int)
        ( interpret_ @"writer"
            ReifiedWriter
              { _writerSink =
                  ReifiedSink
                    { _yield = \w -> do
                        yield @"log" "tell"
                        modify @"test" (<> w)
                    },
                _writer = \(a, w) -> do
                  yield @"log" "writer"
                  modify @"test" (<> w)
                  pure a,
                _listen = \m -> do
                  yield @"log" "listen"
                  w0 <- state @"test" $ \s -> (s, mempty)
                  a <- m
                  w <- state @"test" $ \s -> (s, w0 <> s)
                  pure (a, w),
                _pass = \m -> do
                  yield @"log" "pass"
                  w0 <- state @"test" $ \s -> (s, mempty)
                  (a, f) <- m
                  modify' @"test" $ \s -> w0 <> f s
                  pure a
              }
            ( do
                tell @"writer" 2
                writer @"writer" ((), 3)
                ((), w) <- listen @"writer" $ do
                  tell @"writer" 5
                pass @"writer" $ do
                  tell @"writer" 6
                  pure ((), (+ 1))
                pure w
            )
        )
        `shouldBe` (5, 2 * 3 * 5 * 7, ["tell", "writer", "listen", "tell", "pass", "tell"])
    it "evaluates HasThrow" $ do
      runLogger
        ()
        ( MTL.runExceptT $
            interpret_ @"throw"
              ReifiedThrow
                { _throw = \e -> do
                    MTL.lift $ yield @"log" "throw"
                    MTL.throwError e
                }
              ( do
                  _ <- throw @"throw" "some error"
                  pure $! tail ""
              )
        )
        `shouldBe` (Left "some error", (), ["throw"])
    it "evaluates HasCatch" $ do
      runLogger
        ()
        ( MTL.runExceptT $
            interpret_ @"catch"
              ReifiedCatch
                { _catchThrow =
                    ReifiedThrow
                      { _throw = \e -> do
                          MTL.lift $ yield @"log" "throw"
                          MTL.throwError e
                      },
                  _catch = \m h -> do
                    MTL.lift $ yield @"log" "catch"
                    MTL.catchError m h,
                  _catchJust = \f m h -> do
                    MTL.lift $ yield @"log" "catchJust"
                    MTL.catchError m $ \e ->
                      case f e of
                        Just b -> h b
                        Nothing -> MTL.throwError e
                }
              ( do
                  e1 <-
                    catch @"catch"
                      ( do
                          _ <- throw @"catch" "some error"
                          pure $! tail ""
                      )
                      pure
                  catchJust @"catch"
                    (const Nothing)
                    ( do
                        _ <- throw @"catch" ("again " ++ e1)
                        pure $! tail ""
                    )
                    pure
              )
        )
        `shouldBe` (Left "again some error", (), ["catch", "throw", "catchJust", "throw"])
  describe "interpret" $ do
    it "can be nested" $ do
      runLogger
        ()
        ( interpret @"source" @'[HasSink "log" String]
            ReifiedSource
              { _await = do
                  yield @"log" "await"
                  pure ()
              }
            ( interpret @"sink" @'[HasSink "log" String, HasSource "source" ()]
                ReifiedSink
                  { _yield = \() -> do
                      yield @"log" "yield"
                      pure ()
                  }
                ( do
                    yield @"log" "begin"
                    await @"source"
                    yield @"sink" ()
                    yield @"log" "end"
                )
            )
        )
        `shouldBe` ((), (), ["begin", "await", "yield", "end"])
