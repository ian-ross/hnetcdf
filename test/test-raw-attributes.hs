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
outfile = "test/tmp-tst-attributes.nc"

-- Fake variable ID for global attributes.
nc_global :: Int
nc_global = -1

tests :: [Test]
tests =
  [ testGroup "Attribute processing"
    [ testCase "Read" rawReadAttributes
--    , testCase "Write" rawWriteAttributes
    ]
  ]


--------------------------------------------------------------------------------
--
--  READING ATTRIBUTES
--
--------------------------------------------------------------------------------

rawReadAttributes :: Assertion
rawReadAttributes = do
  (res1, ncid) <- nc_open infile 0
  assertBool ("nc_open error:" ++ nc_strerror res1) (res1 == 0)

  (res7, xvarid) <- nc_inq_varid ncid "x"
  assertBool ("nc_inq_var error:" ++ nc_strerror res7) (res7 == 0)
  (res8, yvarid) <- nc_inq_varid ncid "y"
  assertBool ("nc_inq_var error:" ++ nc_strerror res8) (res8 == 0)
  (res9, zvarid) <- nc_inq_varid ncid "z"
  assertBool ("nc_inq_var error:" ++ nc_strerror res9) (res9 == 0)
  (res10, vfvarid) <- nc_inq_varid ncid "vf"
  assertBool ("nc_inq_var error:" ++ nc_strerror res10) (res10 == 0)
  (res11, vivarid) <- nc_inq_varid ncid "vi"
  assertBool ("nc_inq_var error:" ++ nc_strerror res11) (res11 == 0)
  (res12, vsvarid) <- nc_inq_varid ncid "vs"
  assertBool ("nc_inq_var error:" ++ nc_strerror res12) (res12 == 0)

  (res13, att1len) <- nc_inq_attlen ncid xvarid "long_name"
  assertBool ("nc_inq_attlen error:" ++ nc_strerror res13) (res13 == 0)
  (res14, att1) <- nc_get_att_text ncid xvarid "long_name" att1len
  assertBool ("nc_get_att_text error:" ++ nc_strerror res14)
    (res14 == 0 && att1 == "longitude")
  (res15, att2len) <- nc_inq_attlen ncid xvarid "units"
  assertBool ("nc_inq_attlen error:" ++ nc_strerror res15) (res15 == 0)
  (res16, att2) <- nc_get_att_text ncid xvarid "units" att2len
  assertBool ("nc_get_att_text error:" ++ nc_strerror res16)
    (res16 == 0 && att2 == "degrees_east")
  (res17, att3len) <- nc_inq_attlen ncid xvarid "missing_value"
  assertBool ("nc_inq_attlen error:" ++ nc_strerror res17) (res17 == 0)
  (res18, att3) <- nc_get_att_float ncid xvarid "missing_value" att3len
  assertBool ("nc_get_att_text error:" ++ nc_strerror res18)
    (res18 == 0 && length att3 == 1 && abs (head att3 + 99999.0) < 1E-6)

  (res19, att4len) <- nc_inq_attlen ncid vivarid "missing_value"
  assertBool ("nc_inq_attlen error:" ++ nc_strerror res19) (res19 == 0)
  (res20, att4) <- nc_get_att_int ncid vivarid "missing_value" att4len
  assertBool ("nc_get_att_int error:" ++ nc_strerror res20)
    (res20 == 0 && att4 == [-99999])
  (res21, att5len) <- nc_inq_attlen ncid vsvarid "missing_value"
  assertBool ("nc_inq_attlen error:" ++ nc_strerror res21) (res21 == 0)
  (res22, att5) <- nc_get_att_short ncid vsvarid "missing_value" att5len
  assertBool ("nc_get_att_short error:" ++ nc_strerror res22)
    (res22 == 0 && att5 == [-9999])

  (res23, att6len) <- nc_inq_attlen ncid nc_global "title"
  assertBool ("nc_inq_attlen error:" ++ nc_strerror res23) (res23 == 0)
  (res24, att6) <- nc_get_att_text ncid nc_global "title" att6len
  assertBool ("nc_get_att_text error:" ++ nc_strerror res24)
    (res24 == 0 && att6 == "Produced using Emacs by IR")

  res8 <- nc_close ncid
  assertBool ("nc_close error:" ++ nc_strerror res8) (res8 == 0)


