import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)
import Data.Char
import Control.Monad

import Data.NetCDF.Raw

main :: IO ()
main = defaultMain tests

infile :: FilePath
infile = "test/tst-bathymetry.nc"

tests :: [Test]
tests =
  [ testGroup "Raw basic functions"
    [ testCase "Raw basic functions #1" rawBasic1 ]
  , testGroup "Raw NetCDF file access 1"
    [ testCase "Raw meta-data access: counts" rawMetaDataNums
    , testCase "Raw meta-data access: dimensions" rawMetaDataDims
    , testCase "Raw meta-data access: variables" rawMetaDataVars ]
  ]


rawBasic1 :: Assertion
rawBasic1 = do
  let vers = nc_inq_libvers
  assertBool ("bad nc_inq_libvers value: " ++ vers) (isDigit $ head vers)


rawMetaDataNums :: Assertion
rawMetaDataNums = do
  (res1, ncid) <- nc_open infile 0
  assertBool ("nc_open error:" ++ nc_strerror res1) (res1 == 0)

  (res2, ndims) <- nc_inq_ndims ncid
  assertBool ("nc_inq_ndims error:" ++ nc_strerror res2) (res2 == 0)
  assertEqual "ndims /= 2" 2 ndims
  (res3, nvars) <- nc_inq_nvars ncid
  assertBool ("nc_inq_nvars error:" ++ nc_strerror res3) (res3 == 0)
  assertEqual "nvars /= 4" 4 nvars
  (res4, natts) <- nc_inq_natts ncid
  assertBool ("nc_inq_natts error:" ++ nc_strerror res4) (res4 == 0)
  assertEqual "natts /= 3" 3 natts

  res5 <- nc_close ncid
  assertBool ("nc_close error:" ++ nc_strerror res5) (res5 == 0)


rawMetaDataDims :: Assertion
rawMetaDataDims = do
  (res1, ncid) <- nc_open infile 0
  assertBool ("nc_open error:" ++ nc_strerror res1) (res1 == 0)

  (res2, did1) <- nc_inq_dimid ncid "lat"
  assertBool ("nc_inq_dimid error:" ++ nc_strerror res2) (res2 == 0)
  assertEqual "did(lat) /= 0" 0 did1
  (res3, did2) <- nc_inq_dimid ncid "lon"
  assertBool ("nc_inq_dimid error:" ++ nc_strerror res3) (res3 == 0)
  assertEqual "did(lon) /= 1" 1 did2
  (res4, _) <- nc_inq_dimid ncid "foo"
  assertBool "nc_inq_dimid error" (res4 /= 0)

  (res5, dimname0) <- nc_inq_dimname ncid 0
  assertBool ("nc_inq_dimname error:" ++ nc_strerror res5) (res5 == 0)
  assertEqual "dimname0 /= 'lat'" "lat" dimname0
  (res6, dimname1) <- nc_inq_dimname ncid 1
  assertBool ("nc_inq_dimname error:" ++ nc_strerror res6) (res6 == 0)
  assertEqual "dimname1 /= 'lon'" "lon" dimname1
  (res7, _) <- nc_inq_dimname ncid 2
  assertBool "nc_inq_dimname error" (res7 /= 0)

  (res8, dimlen0) <- nc_inq_dimlen ncid 0
  assertBool ("nc_inq_dimlen error:" ++ nc_strerror res8) (res8 == 0)
  assertEqual "dimlen0 /= 144" 144 dimlen0
  (res9, dimlen1) <- nc_inq_dimlen ncid 1
  assertBool ("nc_inq_dimlen error:" ++ nc_strerror res9) (res9 == 0)
  assertEqual "dimlen1 /= 288" 288 dimlen1
  (res10, _) <- nc_inq_dimlen ncid 2
  assertBool "nc_inq_dimlen error" (res10 /= 0)

  (res11, dimname0', dimlen0') <- nc_inq_dim ncid 0
  assertBool ("nc_inq_dim error:" ++ nc_strerror res11) (res11 == 0)
  assertEqual "dimname0' /= 'lat'" "lat" dimname0'
  assertEqual "dimlen0' /= 144" 144 dimlen0'
  (res12, dimname1', dimlen1') <- nc_inq_dim ncid 1
  assertBool ("nc_inq_dim error:" ++ nc_strerror res12) (res12 == 0)
  assertEqual "dimname1' /= 'lon'" "lon" dimname1'
  assertEqual "dimlen1' /= 288" 288 dimlen1'
  (res13, _, _) <- nc_inq_dim ncid 2
  assertBool "nc_inq_dim error" (res13 /= 0)

  res14 <- nc_close ncid
  assertBool ("nc_close error:" ++ nc_strerror res14) (res14 == 0)


data Var = Var { _varId :: Int
               , _varName :: String
               , _varNcType :: Int
               , _varDims :: [Int]
               , _varNAtts :: Int }

vars :: [Var]
vars = [ Var 0 "bathorig" 5 [0,1] 3
       , Var 1 "depthmask" 5 [0,1] 3
       , Var 2 "lat" 5 [0] 3
       , Var 3 "lon" 5 [1] 3 ]

rawMetaDataVars :: Assertion
rawMetaDataVars = do
  (res1, ncid) <- nc_open infile 0
  assertBool ("nc_open error:" ++ nc_strerror res1) (res1 == 0)

  forM_ vars $ \(Var vid' name' nctype' dims' natts') -> do
    (res2, vid) <- nc_inq_varid ncid name'
    assertBool ("nc_inq_varid error:" ++ nc_strerror res2) (res2 == 0)
    assertEqual ("vid(" ++ name' ++ ") /= " ++ show vid') vid' vid
    (res3, name, nctype, ndims, dims, natts) <- nc_inq_var ncid vid
    assertBool ("nc_inq_var error:" ++ nc_strerror res3) (res3 == 0)
    assertEqual ("name(" ++ name' ++ ") /= '" ++ name' ++ "'") name' name
    assertEqual ("nctype(" ++ name' ++ ") /= " ++ show nctype') nctype' nctype
    assertEqual ("ndims(" ++ name' ++ ") /= " ++ show (length dims'))
      (length dims') ndims
    assertEqual ("dims(" ++ name' ++ ") /= " ++ show dims')
      dims' (take ndims dims)
    assertEqual ("natts(" ++ name' ++ ") /= " ++ show natts') natts' natts

  res4 <- nc_close ncid
  assertBool ("nc_close error:" ++ nc_strerror res4) (res4 == 0)
