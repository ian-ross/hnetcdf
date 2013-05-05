-- Adapted from an example by Don Stewart. For licensing information
-- please see the file "example/Test/Framework/Example.lhs" in the source tree.
import Test.Framework (defaultMain, testGroup, Test(..))
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck (testProperty)

import Test.QuickCheck
import Test.HUnit hiding (Test)

import Data.Char
import Control.Monad

import Data.NetCDF.Raw

main = defaultMain tests

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
  (res, ncid) <- nc_open "test/tst1.nc" 0
  assertBool ("nc_open error:" ++ nc_strerror res) (res == 0)

  (res, ndims) <- nc_inq_ndims ncid
  assertBool ("nc_inq_ndims error:" ++ nc_strerror res) (res == 0)
  assertEqual "ndims /= 2" 2 ndims
  (res, nvars) <- nc_inq_nvars ncid
  assertBool ("nc_inq_nvars error:" ++ nc_strerror res) (res == 0)
  assertEqual "nvars /= 4" 4 nvars
  (res, natts) <- nc_inq_natts ncid
  assertBool ("nc_inq_natts error:" ++ nc_strerror res) (res == 0)
  assertEqual "natts /= 3" 3 natts

  res <- nc_close ncid
  assertBool ("nc_close error:" ++ nc_strerror res) (res == 0)


rawMetaDataDims :: Assertion
rawMetaDataDims = do
  (res, ncid) <- nc_open "test/tst1.nc" 0
  assertBool ("nc_open error:" ++ nc_strerror res) (res == 0)

  (res, did) <- nc_inq_dimid ncid "lat"
  assertBool ("nc_inq_dimid error:" ++ nc_strerror res) (res == 0)
  assertEqual "did(lat) /= 0" 0 did
  (res, did) <- nc_inq_dimid ncid "lon"
  assertBool ("nc_inq_dimid error:" ++ nc_strerror res) (res == 0)
  assertEqual "did(lon) /= 1" 1 did
  (res, did) <- nc_inq_dimid ncid "foo"
  assertBool "nc_inq_dimid error" (res /= 0)

  (res, dimname0) <- nc_inq_dimname ncid 0
  assertBool ("nc_inq_dimname error:" ++ nc_strerror res) (res == 0)
  assertEqual "dimname0 /= 'lat'" "lat" dimname0
  (res, dimname1) <- nc_inq_dimname ncid 1
  assertBool ("nc_inq_dimname error:" ++ nc_strerror res) (res == 0)
  assertEqual "dimname1 /= 'lon'" "lon" dimname1
  (res, dimname2) <- nc_inq_dimname ncid 2
  assertBool "nc_inq_dimname error" (res /= 0)

  (res, dimlen0) <- nc_inq_dimlen ncid 0
  assertBool ("nc_inq_dimlen error:" ++ nc_strerror res) (res == 0)
  assertEqual "dimlen0 /= 144" 144 dimlen0
  (res, dimlen1) <- nc_inq_dimlen ncid 1
  assertBool ("nc_inq_dimlen error:" ++ nc_strerror res) (res == 0)
  assertEqual "dimlen1 /= 288" 288 dimlen1
  (res, dimlen2) <- nc_inq_dimlen ncid 2
  assertBool "nc_inq_dimlen error" (res /= 0)

  (res, dimname0, dimlen0) <- nc_inq_dim ncid 0
  assertBool ("nc_inq_dim error:" ++ nc_strerror res) (res == 0)
  assertEqual "dimname0 /= 'lat'" "lat" dimname0
  assertEqual "dimlen0 /= 144" 144 dimlen0
  (res, dimname1, dimlen1) <- nc_inq_dim ncid 1
  assertBool ("nc_inq_dim error:" ++ nc_strerror res) (res == 0)
  assertEqual "dimname1 /= 'lon'" "lon" dimname1
  assertEqual "dimlen1 /= 288" 288 dimlen1
  (res, dimname2, dimlen2) <- nc_inq_dim ncid 2
  assertBool "nc_inq_dim error" (res /= 0)

  res <- nc_close ncid
  assertBool ("nc_close error:" ++ nc_strerror res) (res == 0)


data Var = Var { varId :: Int
               , varName :: String
               , varNcType :: Int
               , varDims :: [Int]
               , varNAtts :: Int }

vars :: [Var]
vars = [ Var 0 "bathorig" 5 [0,1] 3
       , Var 1 "depthmask" 5 [0,1] 3
       , Var 2 "lat" 5 [0] 3
       , Var 3 "lon" 5 [1] 3 ]

rawMetaDataVars :: Assertion
rawMetaDataVars = do
  (res, ncid) <- nc_open "test/tst1.nc" 0
  assertBool ("nc_open error:" ++ nc_strerror res) (res == 0)

  forM_ vars $ \(Var vid' name' nctype' dims' natts') -> do
    (res, vid) <- nc_inq_varid ncid name'
    assertBool ("nc_inq_varid error:" ++ nc_strerror res) (res == 0)
    assertEqual ("vid(" ++ name' ++ ") /= " ++ show vid') vid' vid
    (res, name, nctype, ndims, dims, natts) <- nc_inq_var ncid vid
    assertBool ("nc_inq_var error:" ++ nc_strerror res) (res == 0)
    assertEqual ("name(" ++ name' ++ ") /= '" ++ name' ++ "'") name' name
    assertEqual ("nctype(" ++ name' ++ ") /= " ++ show nctype') nctype' nctype
    assertEqual ("ndims(" ++ name' ++ ") /= " ++ show (length dims'))
      (length dims') ndims
    assertEqual ("dims(" ++ name' ++ ") /= " ++ show dims')
      dims' (take ndims dims)
    assertEqual ("natts(" ++ name' ++ ") /= " ++ show natts') natts' natts

  res <- nc_close ncid
  assertBool ("nc_close error:" ++ nc_strerror res) (res == 0)
