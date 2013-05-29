import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit hiding (Test, assert)
import Test.QuickCheck
import Test.QuickCheck.Monadic
import System.Directory
import Data.List (zip4)
import qualified Data.Vector.Storable as SV
import Control.Monad

import Data.NetCDF.Raw

main :: IO ()
main = defaultMain tests

infile, outfile :: FilePath
infile = "test/tst-raw-get-put.nc"
outfile = "test/tmp-tst-raw-get-put.nc"

tests :: [Test]
tests =
  [ testGroup "Single item access functions"
    [ testCase "Read (float)" (rawGetVar1 infile "vf" nc_get_var1_float)
    , testCase "Read (int)"   (rawGetVar1 infile "vi" nc_get_var1_int)
    , testCase "Read (short)" (rawGetVar1 infile "vs" nc_get_var1_short)
    , testCase "Write" rawPutVar1 ],
    testGroup "Whole variable access functions"
    [ testCase "Read (float)" (rawGetVar infile "vf" nc_get_var_float)
    , testCase "Read (int)"   (rawGetVar infile "vi" nc_get_var_int)
    , testCase "Read (short)" (rawGetVar infile "vs" nc_get_var_short)
    , testCase "Write" rawPutVar ],
    testGroup "Array access functions"
    [ testCase "Read (float)" (rawGetVarA infile "vf" nc_get_vara_float)
    , testCase "Read (int)"   (rawGetVarA infile "vi" nc_get_vara_int)
    , testCase "Read (short)" (rawGetVarA infile "vs" nc_get_vara_short)
    , testCase "Write" rawPutVarA ],
    testGroup "Stride access functions"
    [ testProperty "Read (float)"
        (monadicIO $ rawGetVarS infile "vf" nc_get_vars_float)
    , testProperty "Read (int)"
        (monadicIO $ rawGetVarS infile "vi" nc_get_vars_int)
    , testProperty "Read (short)"
        (monadicIO $ rawGetVarS infile "vs" nc_get_vars_short) ]
  ]


--------------------------------------------------------------------------------
--
--  SINGLE ITEM READ/WRITE
--
--------------------------------------------------------------------------------

rawGetVar1 :: (Num a, Show a, Eq a) => FilePath -> String
           -> (Int -> Int -> [Int] -> IO (Int, a)) -> Assertion
rawGetVar1 f v rdfn = do
  (res1, ncid) <- nc_open f 0
  assertBool ("nc_open error:" ++ nc_strerror res1) (res1 == 0)

  (res2, ndims) <- nc_inq_ndims ncid
  assertBool ("nc_inq_ndims error:" ++ nc_strerror res2) (res2 == 0)
  assertEqual "ndims /= 3" 3 ndims
  (res3, nvars) <- nc_inq_nvars ncid
  assertBool ("nc_inq_nvars error:" ++ nc_strerror res3) (res3 == 0)
  assertEqual "nvars /= 6" 6 nvars

  (res4, xname, nx) <- nc_inq_dim ncid 0
  (res5, yname, ny) <- nc_inq_dim ncid 1
  (res6, zname, nz) <- nc_inq_dim ncid 2
  assertBool "nc_inq_dim error!" $
    res4 == 0 && res5 == 0 && res6 == 0 &&
    xname == "x" && yname == "y" && zname == "z"

  (res7, vvarid) <- nc_inq_varid ncid v
  assertBool ("nc_inq_var error:" ++ nc_strerror res7) (res7 == 0)

  forM_ [1..nx] $ \ix -> do
    forM_ [1..ny] $ \iy -> do
      forM_ [1..nz] $ \iz -> do
        (res, val) <- rdfn ncid vvarid [iz-1, iy-1, ix-1]
        assertBool ("nc_get_var1 error:" ++ nc_strerror res) (res == 0)
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

  (res1, ncid) <- nc_create outfile 0
  assertBool ("nc_create error:" ++ nc_strerror res1) (res1 == 0)

  (res2, xdimid) <- nc_def_dim ncid "x" 10
  (res3, ydimid) <- nc_def_dim ncid "y" 5
  (res4, zdimid) <- nc_def_dim ncid "z" 3
  assertBool "nc_def_dim error!" $ res2 == 0 && res3 == 0 && res4 == 0

  (res5, xvarid) <- nc_def_var ncid "x" 5 1 [xdimid]
  (res6, yvarid) <- nc_def_var ncid "y" 5 1 [ydimid]
  (res7, zvarid) <- nc_def_var ncid "z" 5 1 [zdimid]
  assertBool "nc_def_var error!" $ res5 == 0 && res6 == 0 && res7 == 0

  (res8, vfvarid) <- nc_def_var ncid "vf" 5 3 [zdimid, ydimid, xdimid]
  assertBool ("nc_def_var error:" ++ nc_strerror res8) (res8 == 0)

  (res9, vivarid) <- nc_def_var ncid "vi" 4 3 [zdimid, ydimid, xdimid]
  assertBool ("nc_def_var error:" ++ nc_strerror res9) (res9 == 0)

  (res10, vsvarid) <- nc_def_var ncid "vs" 3 3 [zdimid, ydimid, xdimid]
  assertBool ("nc_def_var error:" ++ nc_strerror res10) (res10 == 0)

  res11 <- nc_enddef ncid
  assertBool ("nc_enddef error:" ++ nc_strerror res11) (res11 == 0)

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
        res <- nc_put_var1_float ncid vfvarid [iz-1, iy-1, ix-1] val
        assertBool ("nc_put_var1_float error:" ++ nc_strerror res) (res == 0)

  forM_ [1..10] $ \ix -> do
    forM_ [1..5] $ \iy -> do
      forM_ [1..3] $ \iz -> do
        let val = fromIntegral $ ix + 10 * iy + 100 * iz
        res <- nc_put_var1_int ncid vivarid [iz-1, iy-1, ix-1] val
        assertBool ("nc_put_var1_int error:" ++ nc_strerror res) (res == 0)

  forM_ [1..10] $ \ix -> do
    forM_ [1..5] $ \iy -> do
      forM_ [1..3] $ \iz -> do
        let val = fromIntegral $ ix + 10 * iy + 100 * iz
        res <- nc_put_var1_short ncid vsvarid [iz-1, iy-1, ix-1] val
        assertBool ("nc_put_var1_short error:" ++ nc_strerror res) (res == 0)

  res12 <- nc_close ncid
  assertBool ("nc_close error:" ++ nc_strerror res12) (res12 == 0)

  rawGetVar1 outfile "vf" nc_get_var1_float
  rawGetVar1 outfile "vi" nc_get_var1_int
  rawGetVar1 outfile "vs" nc_get_var1_short


--------------------------------------------------------------------------------
--
--  WHOLE VARIABLE READ/WRITE
--
--------------------------------------------------------------------------------

rawGetVar :: (Num a, Show a, Eq a, SV.Storable a) => FilePath -> String
          -> (Int -> Int -> Int -> IO (Int, SV.Vector a)) -> Assertion
rawGetVar f v rdfn = do
  (res1, ncid) <- nc_open f 0
  assertBool ("nc_open error:" ++ nc_strerror res1) (res1 == 0)

  (res2, ndims) <- nc_inq_ndims ncid
  assertBool ("nc_inq_ndims error:" ++ nc_strerror res2) (res2 == 0)
  assertEqual "ndims /= 3" 3 ndims
  (res3, nvars) <- nc_inq_nvars ncid
  assertBool ("nc_inq_nvars error:" ++ nc_strerror res3) (res3 == 0)
  assertEqual "nvars /= 6" 6 nvars

  (res4, xname, nx) <- nc_inq_dim ncid 0
  (res5, yname, ny) <- nc_inq_dim ncid 1
  (res6, zname, nz) <- nc_inq_dim ncid 2
  assertBool "nc_inq_dim error!" $
    res4 == 0 && res5 == 0 && res6 == 0 &&
    xname == "x" && yname == "y" && zname == "z"

  (res7, vvarid) <- nc_inq_varid ncid v
  assertBool ("nc_inq_var error:" ++ nc_strerror res7) (res7 == 0)

  (res8, vals) <- rdfn ncid vvarid (nx * ny * nz)
  assertBool ("nc_get_var error:" ++ nc_strerror res8) (res8 == 0)

  forM_ [1..nx] $ \ix -> do
    forM_ [1..ny] $ \iy -> do
      forM_ [1..nz] $ \iz -> do
        let trueval = fromIntegral $ ix + 10 * iy + 100 * iz
            idx = (iz - 1) * ny * nx + (iy - 1) * nx + (ix - 1)
        assertBool ("value error: " ++ show (vals SV.! idx) ++
                    " instead of " ++ show trueval)
          ((vals SV.! idx) == trueval)

  res8 <- nc_close ncid
  assertBool ("nc_close error:" ++ nc_strerror res8) (res8 == 0)


rawPutVar :: Assertion
rawPutVar = do
  ex <- doesFileExist outfile
  when ex $ removeFile outfile

  (res1, ncid) <- nc_create outfile 0
  assertBool ("nc_create error:" ++ nc_strerror res1) (res1 == 0)

  let nx = 10
      ny = 5
      nz = 3

  (res2, xdimid) <- nc_def_dim ncid "x" nx
  (res3, ydimid) <- nc_def_dim ncid "y" ny
  (res4, zdimid) <- nc_def_dim ncid "z" nz
  assertBool "nc_def_dim error!" $ res2 == 0 && res3 == 0 && res4 == 0

  (res5, xvarid) <- nc_def_var ncid "x" 5 1 [xdimid]
  (res6, yvarid) <- nc_def_var ncid "y" 5 1 [ydimid]
  (res7, zvarid) <- nc_def_var ncid "z" 5 1 [zdimid]
  assertBool "nc_def_var error!" $ res5 == 0 && res6 == 0 && res7 == 0

  (res8, vfvarid) <- nc_def_var ncid "vf" 5 3 [zdimid, ydimid, xdimid]
  assertBool ("nc_def_var error:" ++ nc_strerror res8) (res8 == 0)

  (res9, vivarid) <- nc_def_var ncid "vi" 4 3 [zdimid, ydimid, xdimid]
  assertBool ("nc_def_var error:" ++ nc_strerror res9) (res9 == 0)

  (res10, vsvarid) <- nc_def_var ncid "vs" 3 3 [zdimid, ydimid, xdimid]
  assertBool ("nc_def_var error:" ++ nc_strerror res10) (res10 == 0)

  res11 <- nc_enddef ncid
  assertBool ("nc_enddef error:" ++ nc_strerror res11) (res11 == 0)

  let xvals = SV.generate nx (fromIntegral . (+1))
      yvals = SV.generate ny (fromIntegral . (*10) . (+1))
      zvals = SV.generate nz (fromIntegral . (*100) . (+1))
      vfvals = gen nx ny nz
      vivals = gen nx ny nz
      vsvals = gen nx ny nz

  res12 <- nc_put_var_float ncid xvarid xvals
  assertBool ("nc_put_var_float error:" ++ nc_strerror res12) (res12 == 0)

  res13 <- nc_put_var_float ncid yvarid yvals
  assertBool ("nc_put_var_float error:" ++ nc_strerror res13) (res13 == 0)

  res14 <- nc_put_var_float ncid zvarid zvals
  assertBool ("nc_put_var_float error:" ++ nc_strerror res14) (res14 == 0)

  res15 <- nc_put_var_float ncid vfvarid vfvals
  assertBool ("nc_put_var_float error:" ++ nc_strerror res15) (res15 == 0)

  res16 <- nc_put_var_int ncid vivarid vivals
  assertBool ("nc_put_var_int error:" ++ nc_strerror res16) (res16 == 0)

  res17 <- nc_put_var_short ncid vsvarid vsvals
  assertBool ("nc_put_var_short error:" ++ nc_strerror res17) (res17 == 0)

  res18 <- nc_close ncid
  assertBool ("nc_close error:" ++ nc_strerror res18) (res18 == 0)

  rawGetVar1 outfile "vf" nc_get_var1_float
  rawGetVar1 outfile "vi" nc_get_var1_int
  rawGetVar1 outfile "vs" nc_get_var1_short


gen :: (Num a, SV.Storable a) => Int -> Int -> Int -> SV.Vector a
gen nx ny nz = SV.generate (nx * ny * nz) $ \i ->
  let ix = i `mod` nx + 1
      iy = (i `div` nx) `mod` ny + 1
      iz = i `div` (nx * ny) + 1
  in fromIntegral $ 100 * iz + 10 * iy + ix


--------------------------------------------------------------------------------
--
--  ARRAY READ/WRITE
--
--------------------------------------------------------------------------------

rawGetVarA :: (Num a, Show a, Eq a, SV.Storable a) => FilePath -> String
           -> (Int -> Int -> [Int] -> [Int] -> IO (Int, SV.Vector a))
           -> Assertion
rawGetVarA f v rdfn = do
  (res1, ncid) <- nc_open f 0
  assertBool ("nc_open error:" ++ nc_strerror res1) (res1 == 0)

  (res2, ndims) <- nc_inq_ndims ncid
  assertBool ("nc_inq_ndims error:" ++ nc_strerror res2) (res2 == 0)
  assertEqual "ndims /= 3" 3 ndims
  (res3, nvars) <- nc_inq_nvars ncid
  assertBool ("nc_inq_nvars error:" ++ nc_strerror res3) (res3 == 0)
  assertEqual "nvars /= 6" 6 nvars

  (res4, xname, nx) <- nc_inq_dim ncid 0
  (res5, yname, ny) <- nc_inq_dim ncid 1
  (res6, zname, nz) <- nc_inq_dim ncid 2
  assertBool "nc_inq_dim error!" $
    res4 == 0 && res5 == 0 && res6 == 0 &&
    xname == "x" && yname == "y" && zname == "z"

  (res7, vvarid) <- nc_inq_varid ncid v
  assertBool ("nc_inq_var error:" ++ nc_strerror res7) (res7 == 0)

  forM_ [1..nx] $ \ix -> do
    forM_ [1..ny] $ \iy -> do
      let start = [0, iy-1, ix-1]
          count = [nz, 1, 1]
      (res, vals) <- rdfn ncid vvarid start count
      assertBool ("nc_get_var error:" ++ nc_strerror res) (res == 0)
      let truevals = SV.generate nz
                     (\iz -> fromIntegral $ ix + 10 * iy + 100 * (iz+1))
      assertBool ("1: value error: " ++ show vals ++
                  " instead of " ++ show truevals) (vals == truevals)

  forM_ [1..nx] $ \ix -> do
    forM_ [1..nz] $ \iz -> do
      let start = [iz-1, 0, ix-1]
          count = [1, ny, 1]
      (res, vals) <- rdfn ncid vvarid start count
      assertBool ("nc_get_var error:" ++ nc_strerror res) (res == 0)
      let truevals = SV.generate ny
                     (\iy -> fromIntegral $ ix + 10 * (iy+1) + 100 * iz)
      assertBool ("2: value error: " ++ show vals ++
                  " instead of " ++ show truevals) (vals == truevals)

  forM_ [1..ny] $ \iy -> do
    forM_ [1..nz] $ \iz -> do
      let start = [iz-1, iy-1, 0]
          count = [1, 1, nx]
      (res, vals) <- rdfn ncid vvarid start count
      assertBool ("nc_get_var error:" ++ nc_strerror res) (res == 0)
      let truevals = SV.generate nx
                     (\ix -> fromIntegral $ (ix+1) + 10 * iy + 100 * iz)
      assertBool ("3: value error: " ++ show vals ++
                  " instead of " ++ show truevals) (vals == truevals)

  res8 <- nc_close ncid
  assertBool ("nc_close error:" ++ nc_strerror res8) (res8 == 0)


rawPutVarA :: Assertion
rawPutVarA = do
  ex <- doesFileExist outfile
  when ex $ removeFile outfile

  (res1, ncid) <- nc_create outfile 0
  assertBool ("nc_create error:" ++ nc_strerror res1) (res1 == 0)

  let nx = 10
      ny = 5
      nz = 3

  (res2, xdimid) <- nc_def_dim ncid "x" nx
  (res3, ydimid) <- nc_def_dim ncid "y" ny
  (res4, zdimid) <- nc_def_dim ncid "z" nz
  assertBool "nc_def_dim error!" $ res2 == 0 && res3 == 0 && res4 == 0

  (res5, xvarid) <- nc_def_var ncid "x" 5 1 [xdimid]
  (res6, yvarid) <- nc_def_var ncid "y" 5 1 [ydimid]
  (res7, zvarid) <- nc_def_var ncid "z" 5 1 [zdimid]
  assertBool "nc_def_var error!" $ res5 == 0 && res6 == 0 && res7 == 0

  (res8, vfvarid) <- nc_def_var ncid "vf" 5 3 [zdimid, ydimid, xdimid]
  assertBool ("nc_def_var error:" ++ nc_strerror res8) (res8 == 0)

  (res9, vivarid) <- nc_def_var ncid "vi" 4 3 [zdimid, ydimid, xdimid]
  assertBool ("nc_def_var error:" ++ nc_strerror res9) (res9 == 0)

  (res10, vsvarid) <- nc_def_var ncid "vs" 3 3 [zdimid, ydimid, xdimid]
  assertBool ("nc_def_var error:" ++ nc_strerror res10) (res10 == 0)

  res11 <- nc_enddef ncid
  assertBool ("nc_enddef error:" ++ nc_strerror res11) (res11 == 0)

  let xvals = SV.generate nx (fromIntegral . (+1))
      yvals = SV.generate ny (fromIntegral . (*10) . (+1))
      zvals = SV.generate nz (fromIntegral . (*100) . (+1))

  res12 <- nc_put_var_float ncid xvarid xvals
  assertBool ("nc_put_var_float error:" ++ nc_strerror res12) (res12 == 0)

  res13 <- nc_put_var_float ncid yvarid yvals
  assertBool ("nc_put_var_float error:" ++ nc_strerror res13) (res13 == 0)

  res14 <- nc_put_var_float ncid zvarid zvals
  assertBool ("nc_put_var_float error:" ++ nc_strerror res14) (res14 == 0)

  forM_ [1..nx] $ \ix -> do
    forM_ [1..ny] $ \iy -> do
      let start = [0, iy-1, ix-1]
          count = [nz, 1, 1]
          vals = SV.generate nz
                 (\iz -> fromIntegral $ ix + 10 * iy + 100 * (iz+1))
      res <- nc_put_vara_float ncid vfvarid start count vals
      assertBool ("nc_put_vara_float error:" ++ nc_strerror res) (res == 0)

  forM_ [1..nx] $ \ix -> do
    forM_ [1..nz] $ \iz -> do
      let start = [iz-1, 0, ix-1]
          count = [1, ny, 1]
          vals = SV.generate ny
                 (\iy -> fromIntegral $ ix + 10 * (iy+1) + 100 * iz)
      res <- nc_put_vara_int ncid vivarid start count vals
      assertBool ("nc_put_vara_int error:" ++ nc_strerror res) (res == 0)

  forM_ [1..ny] $ \iy -> do
    forM_ [1..nz] $ \iz -> do
      let start = [iz-1, iy-1, 0]
          count = [1, 1, nx]
          vals = SV.generate nx
                 (\ix -> fromIntegral $ (ix+1) + 10 * iy + 100 * iz)
      res <- nc_put_vara_short ncid vsvarid start count vals
      assertBool ("nc_put_vara_short error:" ++ nc_strerror res) (res == 0)

  res18 <- nc_close ncid
  assertBool ("nc_close error:" ++ nc_strerror res18) (res18 == 0)

  rawGetVarA outfile "vf" nc_get_vara_float
  rawGetVarA outfile "vi" nc_get_vara_int
  rawGetVarA outfile "vs" nc_get_vara_short


--------------------------------------------------------------------------------
--
--  STRIDED READ/WRITE
--
--------------------------------------------------------------------------------

rawGetVarS :: (Num a, Show a, Eq a, SV.Storable a) => FilePath -> String
           -> (Int -> Int -> [Int] -> [Int] -> [Int] -> IO (Int, SV.Vector a))
           -> PropertyM IO ()
rawGetVarS f v rdfn = do
  [nx, ny, nz] <- run $ do
    (_, ncid) <- nc_open f 0
    (_, _, nx) <- nc_inq_dim ncid 0
    (_, _, ny) <- nc_inq_dim ncid 1
    (_, _, nz) <- nc_inq_dim ncid 2
    _ <- nc_close ncid
    return [nx, ny, nz]
  start <- mapM pick $ map choose [(0, nz - 1), (0, ny - 1), (0, nx - 1)]
  let left = zipWith (-) [nz, ny, nx] start
  stride <- mapM pick $ map choose $ zip (repeat 1) left
  count <- mapM pick $ map choose $ zip (repeat 1) (zipWith div left stride)
  let [izs, iys, ixs] =
        map (\(s, c, str, n) -> [i | i <- take c [s, s + str..], i < n]) $
        zip4 start count stride [nz, ny, nx]
      vs = [fromIntegral $ 100 * (iz + 1) + 10 * (iy + 1) + (ix + 1) |
            iz <- izs, iy <- iys, ix <- ixs]
      truevals = SV.fromList vs
  vals <- run $ do
    (_, ncid) <- nc_open f 0
    (_, vvarid) <- nc_inq_varid ncid v
    (_, vals) <- rdfn ncid vvarid start count stride
    _ <- nc_close ncid
    return vals
  assert $ vals == truevals
