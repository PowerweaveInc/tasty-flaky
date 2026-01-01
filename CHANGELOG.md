# Revision history for tasty-flaky

## 0.1.3.1 --  2026-01-01

* Added support for GHC 9.14

## 0.1.3.0 --  2025-12-13

* Exceptions in tests will now be caught and considered test failures, even if the test runner
  doesn't catch the exception. This allows, for example, to retry on pattern match failure:

  ```haskell
  test :: TestTree
  test = testCase "Pattern match" $ do
      Just x <- f
      ...
  ```
  
  This test will now be retried instead of throwing an exception.

## 0.1.2.0 --  2025-02-10

* Add `flakyTestWithRetryAction`. Retry logic can now be based on test results (#1).

## 0.1.1.0 -- 2024-12-19

* Explicit support and testing for GHC 9.12

## 0.1.0.0 -- 2024-09-24

* First version
