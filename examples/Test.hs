module Main
  ( main
  ) where

import Test.Hspec

import qualified CountLog
import qualified Error
import qualified Reader
import qualified State
import qualified Stream
import qualified Writer


main :: IO ()
main = hspec $ do
  describe "CountLog" CountLog.spec
  describe "Error" Error.spec
  describe "Reader" Reader.spec
  describe "State" State.spec
  describe "Stream" Stream.spec
  describe "Writer" Writer.spec
