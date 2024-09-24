module Main (main) where

import Control.Monad ( unless )
import Data.IORef as IORef

import Test.Tasty ( TestTree, testGroup, defaultMain, withResource )
import Test.Tasty.Flaky ( limitRetries, flakyTest )
import Test.Tasty.HUnit ( testCase, assertFailure )


main :: IO ()
main = defaultMain 
     $ testGroup "Test suite" 
     [ testSuccessOnFirstTry
     , flakyTest (limitRetries 4) 
        $ withResource (IORef.newIORef 3) (const $ pure ()) testFlakyWithRetries
     ]


testSuccessOnFirstTry :: TestTree
testSuccessOnFirstTry = flakyTest (limitRetries 0) $ testCase "succeeds on the first try" $ do
    pure ()


-- This test will fail until the contents of the IORef is zero
testFlakyWithRetries :: IO (IORef Int) -> TestTree
testFlakyWithRetries getioref = testCase "effectful" $ do 
    ioref <- getioref
    n <- IORef.readIORef ioref
    unless (n == 0) $ do
        IORef.modifyIORef' ioref (\m -> m - 1)
        assertFailure "Not yet"