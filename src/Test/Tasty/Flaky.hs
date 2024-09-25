{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  $module
-- Copyright   :  (c) Powerweave Inc.
-- License     :  BSD-3-Clause
-- Maintainer  :  Laurent René de Cotret
-- Portability :  portable
--
-- This module defines a single function, 'flakyTest', to declare a test
-- which intermittently fails. Flaky tests can be retries using retry policies
-- provided by the "Control.Retry" module (from the @retry@ package).
--
--
-- For example, you can retry test cases from @tasty-hunit@ like so:
--
-- @
-- import Test.Tasty.HUnit ( testCase ) -- from tasty-hunit
-- 
-- myFlakyTest :: TestTree
-- myFlakyTest = 'flakyTest' ('limitRetries' 5 <> 'constantDelay' 1000) $ testCase "some test case" $ do ... 
-- @
--
-- In the example above, the test will be retried up to 5 times, with a delay of 1000 microseconds between tries,
-- if a failure occurs.
--
module Test.Tasty.Flaky (
    -- * Test wrapper
    flakyTest

    -- * Re-exports
    -- 
    -- | The following functions allow to construct 'RetryPolicyM IO' 
    -- from the "Control.Retry" module.
    , constantDelay
    , exponentialBackoff
    , fullJitterBackoff
    , fibonacciBackoff
    , limitRetries

    -- * Policy Transformers
    , limitRetriesByDelay
    , limitRetriesByCumulativeDelay
    , capDelay

) where

import Control.Retry hiding (RetryPolicy)
import Data.Functor ( (<&>) )
import Data.Tagged (Tagged, retag )
import Test.Tasty.Providers ( IsTest(..), Progress, Result, TestTree )
import Test.Tasty.Runners ( TestTree(..), Result(..), Progress(..), emptyProgress, resultSuccessful )
import Test.Tasty.Options ( OptionDescription, OptionSet )


-- | A test tree of type @t@, with an associated retry policy
data FlakyTest t
    = MkFlakyTest (RetryPolicyM IO) t


-- | Mark any test as flaky.
--
-- If this test is not successful, it will be retries according to the supplied @'RetryPolicyM' 'IO'@. 
-- See "Control.Retry" for documentation on how to specify a @'RetryPolicyM' 'IO'@.
--
-- For example, you can retry test cases from @tasty-hunit@ like so:
--
-- @
-- import Test.Tasty.HUnit ( testCase ) -- from tasty-hunit
-- 
-- myFlakyTest :: TestTree
-- myFlakyTest = 'flakyTest' ('limitRetries' 5 <> 'constantDelay' 1000) $ testCase "some test case" $ do ... 
-- @
--
-- You can retry individual tests (like the example above), or retry entire groups by wrapping
-- 'Test.Tasty.testGroup'.
--
flakyTest :: (RetryPolicyM IO) -> TestTree -> TestTree
flakyTest policy (SingleTest name t) = SingleTest name (MkFlakyTest policy t)
flakyTest policy (TestGroup name subtree) = TestGroup name (map (flakyTest policy) subtree)
flakyTest policy (WithResource spec f) = WithResource spec (f <&> flakyTest policy)
flakyTest _ other = other


instance IsTest t => IsTest (FlakyTest t) where
    run :: IsTest t => OptionSet -> FlakyTest t -> (Progress -> IO ()) -> IO Result
    run opts (MkFlakyTest policy test) callback = go defaultRetryStatus
        where
            -- The logic below mimics the `retry` package's Control.Retry.retrying
            -- with one major difference: we annotate the final result
            -- to report how many retries have been performed, regardless of
            -- the final result.
            go :: RetryStatus -> IO Result
            go status = do
                result <- run opts test callback
                let consultPolicy policy' = do
                        rs <- applyAndDelay policy' status
                        case rs of
                            -- We are done: no more retries
                            Nothing -> pure $ annotateResult status result
                            -- At least one more retry
                            Just rs' -> do 
                                callback (emptyProgress{progressText=mconcat ["Attempt #", show (rsIterNumber status + 1), " failed"]})
                                go $! rs'

                if resultSuccessful result
                    then pure $ annotateResult status result
                    else consultPolicy policy
            
            annotateResult :: RetryStatus -> Result -> Result
            annotateResult status result 
                = result { resultDescription = resultDescription result <> annotate status }
                where
                    annotate :: RetryStatus -> String
                    annotate (RetryStatus iternum cumdelay _) 
                        | iternum == 0 = ""
                        | otherwise    = mconcat [" [", show iternum, " retries, ", show cumdelay, " μs delay]"]


    testOptions :: Tagged (FlakyTest t) [OptionDescription]
    testOptions = retag (testOptions :: Tagged t [OptionDescription])
