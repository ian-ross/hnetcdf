import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import Control.Monad

import Data.NetCDF.Raw

main :: IO ()
main = defaultMain tests

infile :: FilePath
infile = "test/tst-var1.nc"

tests :: [Test]
tests =
  [ testGroup "Single item access functions"
    [ testCase "Read single items" rawGetVar1,
      testCase "Write single items" rawPutVar1 ]
  ]


rawGetVar1 :: Assertion
rawGetVar1 = do
  (res1, ncid) <- nc_open infile 0
  assertBool ("nc_open error:" ++ nc_strerror res1) (res1 == 0)

  (res2, ndims) <- nc_inq_ndims ncid
  assertBool ("nc_inq_ndims error:" ++ nc_strerror res2) (res2 == 0)
  assertEqual "ndims /= 3" 3 ndims
  (res3, nvars) <- nc_inq_nvars ncid
  assertBool ("nc_inq_nvars error:" ++ nc_strerror res3) (res3 == 0)
  assertEqual "nvars /= 4" 4 nvars

  (res4, xname, nx) <- nc_inq_dim ncid 0
  (res5, yname, ny) <- nc_inq_dim ncid 1
  (res6, zname, nz) <- nc_inq_dim ncid 2
  assertBool "nc_inq_dim error!" $
    res4 == 0 && res5 == 0 && res6 == 0 &&
    xname == "x" && yname == "y" && zname == "z"

  (res7, vvarid) <- nc_inq_varid ncid "v"
  assertBool ("nc_inq_var error:" ++ nc_strerror res7) (res7 == 0)

  forM_ [1..nx] $ \ix -> do
    forM_ [1..ny] $ \iy -> do
      forM_ [1..nz] $ \iz -> do
        (res, val) <- nc_get_var1_float ncid vvarid [iz-1, iy-1, ix-1]
        assertBool ("nc_get_var1_float error:" ++ nc_strerror res) (res == 0)
        let trueval = fromIntegral $ ix + 10 * iy + 100 * iz
        assertBool ("value error: " ++ show val ++
                    " instead of " ++ show trueval)
          (val == trueval)

  res8 <- nc_close ncid
  assertBool ("nc_close error:" ++ nc_strerror res8) (res8 == 0)


rawPutVar1 :: Assertion
rawPutVar1 = do
  assertBool "" True
