module Test.Common
  ( shouldPrint
  , shouldNotPrint
  ) where

import System.IO.Silently (capture_)
import Test.Hspec


shouldPrint :: IO a -> String -> IO ()
shouldPrint action expected = do
  actual <- capture_ action
  actual `shouldBe` expected


shouldNotPrint :: IO a -> String -> IO ()
shouldNotPrint action expected = do
  actual <- capture_ action
  actual `shouldNotBe` expected
