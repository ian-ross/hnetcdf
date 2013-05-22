import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)
import System.Directory

import Control.Monad

import Data.NetCDF.Raw

main :: IO ()
main = defaultMain tests

infile, outfile :: FilePath
infile = "test/tst-var1.nc"
outfile = "test/tmp-tst-var1.nc"

tests :: [Test]
tests =
  [ testGroup "Single item access functions"
    [ testCase "Read single items" (rawGetVar1 infile),
      testCase "Write single items" rawPutVar1 ]
  ]


rawGetVar1 :: FilePath -> Assertion
rawGetVar1 f = do
  (res1, ncid) <- nc_open f 0
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
  ex <- doesFileExist outfile
  when ex $ removeFile outfile

  (res1, ncid) <- nc_create outfile 1
  assertBool ("nc_create error:" ++ nc_strerror res1) (res1 == 0)

  (res2, xdimid) <- nc_def_dim ncid "x" 10
  (res3, ydimid) <- nc_def_dim ncid "y" 5
  (res4, zdimid) <- nc_def_dim ncid "z" 3
  assertBool "nc_def_dim error!" $ res2 == 0 && res3 == 0 && res4 == 0

  (res5, xvarid) <- nc_def_var ncid "x" 5 1 [xdimid]
  (res6, yvarid) <- nc_def_var ncid "y" 5 1 [ydimid]
  (res7, zvarid) <- nc_def_var ncid "z" 5 1 [zdimid]
  assertBool "nc_def_var error!" $ res5 == 0 && res6 == 0 && res7 == 0

  (res8, vvarid) <- nc_def_var ncid "v" 5 3 [zdimid, ydimid, xdimid]
  assertBool ("nc_def_var error:" ++ nc_strerror res8) (res8 == 0)

  res9 <- nc_enddef ncid
  assertBool ("nc_enddef error:" ++ nc_strerror res9) (res9 == 0)

  forM_ [1..10] $ \ix -> do
    res <- nc_put_var1_float ncid xvarid [ix-1] (fromIntegral ix)
    assertBool ("nc_put_var1_float error:" ++ nc_strerror res) (res == 0)

  forM_ [1..5] $ \iy -> do
    res <- nc_put_var1_float ncid yvarid [iy-1] (fromIntegral iy)
    assertBool ("nc_put_var1_float error:" ++ nc_strerror res) (res == 0)

  forM_ [1..3] $ \iz -> do
    res <- nc_put_var1_float ncid zvarid [iz-1] (fromIntegral iz)
    assertBool ("nc_put_var1_float error:" ++ nc_strerror res) (res == 0)

  forM_ [1..10] $ \ix -> do
    forM_ [1..5] $ \iy -> do
      forM_ [1..3] $ \iz -> do
        let val = fromIntegral $ ix + 10 * iy + 100 * iz
        res <- nc_put_var1_float ncid vvarid [iz-1, iy-1, ix-1] val
        assertBool ("nc_put_var1_float error:" ++ nc_strerror res) (res == 0)

  res10 <- nc_close ncid
  assertBool ("nc_close error:" ++ nc_strerror res10) (res10 == 0)

  rawGetVar1 outfile
