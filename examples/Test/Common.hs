module Test.Common
  ( shouldPrint
  , shouldNotPrint
  , withInput
  ) where

import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import System.IO
import System.IO.Silently (capture_)
import System.IO.Temp (withSystemTempFile)
import Test.Hspec
import UnliftIO.Exception (bracket)


shouldPrint :: IO a -> String -> IO ()
shouldPrint action expected = do
  actual <- capture_ action
  actual `shouldBe` expected


shouldNotPrint :: IO a -> String -> IO ()
shouldNotPrint action expected = do
  actual <- capture_ action
  actual `shouldNotBe` expected


-- | Execute the given action with @stdin@ redirected to read the given input.
withInput :: IO a -> String -> IO a
withInput action input =
  withSystemTempFile "capabilities-via-mock-input" $ \tmpFile tmpOut -> do
    -- write input to temp-file
    hPutStr tmpOut input
    hClose tmpOut
    -- read stdin from temp-file
    buffering <- hGetBuffering stdin
    withFile tmpFile ReadMode $ \tmpIn -> do
      let redirect = do
            old <- hDuplicate stdin
            hDuplicateTo tmpIn stdin
            pure old
          restore old = do
            hDuplicateTo old stdin
            hSetBuffering stdin buffering
            hClose old
      bracket redirect restore (\_ -> action)
