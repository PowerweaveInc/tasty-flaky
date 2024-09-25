{-# LANGUAGE NumericUnderscores #-}
module Main (main) where

import Control.Monad ( unless )
import Control.Concurrent ( threadDelay )
import Data.IORef as IORef

import Test.Tasty ( TestTree, testGroup, defaultMain, withResource )
import Test.Tasty.Flaky ( limitRetries, flakyTest )
import Test.Tasty.HUnit ( testCase, assertFailure )


main :: IO ()
main = defaultMain 
     $ testGroup "Test suite" 
     [ testSuccessOnFirstTry
     , testFlakyWithRetries
     , testFlakyWithRetriesProgressCallback
     ]


testSuccessOnFirstTry :: TestTree
testSuccessOnFirstTry = flakyTest (limitRetries 0) $ testCase "succeeds on the first try" $ do
    pure ()


-- This test will fail until the contents of the IORef is zero
testFlakyWithRetries :: TestTree
testFlakyWithRetries 
    = flakyTest (limitRetries 4) 
        $ withResource (IORef.newIORef (3 :: Int)) (const $ pure ()) 
            $ \getioref -> testCase "effectful" $ do
                ioref <- getioref
                n <- IORef.readIORef ioref
                unless (n == 0) $ do
                    IORef.modifyIORef' ioref (\m -> m - 1)
                    assertFailure "Not yet"


-- This test will fail until the contents of the IORef is zero
testFlakyWithRetriesProgressCallback :: TestTree
testFlakyWithRetriesProgressCallback 
    = flakyTest (limitRetries 4) 
        $ withResource (IORef.newIORef (3 :: Int)) (const $ pure ()) 
            $ \getioref -> testCase "Showcasing progress report" $ do
                -- Wait 1 second for tasty to consider this test 'long-running'
                threadDelay 1_000_000
                ioref <- getioref
                n <- IORef.readIORef ioref
                unless (n == 0) $ do
                    IORef.modifyIORef' ioref (\m -> m - 1)
                    assertFailure "Not yet"


