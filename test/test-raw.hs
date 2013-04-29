-- Adapted from an example by Don Stewart. For licensing information
-- please see the file "example/Test/Framework/Example.lhs" in the source tree.
import Test.Framework (defaultMain, testGroup, Test(..))
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck (testProperty)

import Test.QuickCheck
import Test.HUnit hiding (Test)

import Control.Monad (when)

import Data.NetCDF.Raw

main = defaultMain tests

tests :: [Test]
tests = [ testGroup "Raw NetCDF file access 1" [
             testCase "quickTry" quickTry
             ]
        ]

quickTry :: Assertion
quickTry = do
  (res1, ncid) <- ncOpen "test/tst.nc" 0
  assertBool ("ncOpen error:" ++ ncStrError res1) (res1 == 0)
  (res2, ndims) <- ncInqNdims ncid
  assertBool ("ncInqNdims error:" ++ ncStrError res2) (res2 == 0)
  assertEqual "ndims /= 2" 2 ndims

