module Main
  ( main
  ) where

import Test.Hspec

import qualified Reader


main :: IO ()
main = hspec $
  Reader.spec
