import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test, assert)
import qualified Data.Vector.Storable as SV
import qualified Data.Array.Repa as R
import Data.Array.Repa.Repr.ForeignPtr (F)
import Control.Error
import Control.Monad

import Foreign.C
import Data.NetCDF

main :: IO ()
main = defaultMain tests

infile :: FilePath
infile = "test/tst-raw-get-put.nc"

tests :: [Test]
tests =
  [ testGroup "Single item access functions"
    [ testCase "Read (float)" (getVar1Float infile "vf")
    ],
    testGroup "Whole variable access functions"
    [ testCase "Read (float, SV)" (getVarFloatSV infile "vf")
    , testCase "Read (float, Repa)" (getVarFloatRepa infile "vf")
    ]
  ]


--------------------------------------------------------------------------------
--
--  SINGLE ITEM READ
--
--------------------------------------------------------------------------------

getVar1Float :: FilePath -> String -> Assertion
getVar1Float f v = do
  enc <- openFile f ReadMode
  assertBool "failed to open file" $ isRight enc
  let Right nc = enc
  case ncVar nc v of
    Nothing -> assertBool "missing variable" False
    Just var -> do
      let [nz, ny, nx] = map ncDimLength $ ncVarDims var
      forM_ [1..nx] $ \ix -> do
        forM_ [1..ny] $ \iy -> do
          forM_ [1..nz] $ \iz -> do
            eval <- get1 nc var [iz-1, iy-1, ix-1] :: IO (Either NcError CFloat)
            case eval of
              Left e -> assertBool ("read error: " ++ show e) False
              Right val -> do
                let trueval = fromIntegral $ ix + 10 * iy + 100 * iz :: Double
                assertBool ("value error: " ++ show val ++
                            " instead of " ++ show trueval)
                  (realToFrac val == trueval)
  void $ closeFile nc



--------------------------------------------------------------------------------
--
--  WHOLE VARIABLE READ
--
--------------------------------------------------------------------------------

getVarFloatSV :: FilePath -> String -> Assertion
getVarFloatSV f v = do
  enc <- openFile f ReadMode
  assertBool "failed to open file" $ isRight enc
  let Right nc = enc
  case ncVar nc v of
    Nothing -> assertBool "missing variable" False
    Just var -> do
      eval <- get nc var :: IO (Either NcError (SV.Vector CFloat))
      case eval of
        Left e -> assertBool ("read error: " ++ show e) False
        Right val -> do
          let [nz, ny, nx] = map ncDimLength $ ncVarDims var
              truevals = [fromIntegral $ ix + 10 * iy + 100 * iz |
                          ix <- [1..nx], iy <- [1..ny], iz <- [1..nz]]
              idxs = [(iz - 1) * ny * nx + (iy - 1) * nx + (ix - 1) |
                      ix <- [1..nx], iy <- [1..ny], iz <- [1..nz]]
              tstvals = [val SV.! i | i <- idxs]
          assertBool ("value error: " ++ show tstvals ++
                      " instead of " ++ show truevals)
            (and $ zipWith (==) tstvals truevals)
  void $ closeFile nc

getVarFloatRepa :: FilePath -> String -> Assertion
getVarFloatRepa f v = do
  enc <- openFile f ReadMode
  assertBool "failed to open file" $ isRight enc
  let Right nc = enc
  case ncVar nc v of
    Nothing -> assertBool "missing variable" False
    Just var -> do
      eval <- get nc var :: IO (Either NcError (R.Array F R.DIM3 CFloat))
      case eval of
        Left e -> assertBool ("read error: " ++ show e) False
        Right val -> do
          let [nz, ny, nx] = map ncDimLength $ ncVarDims var
              truevals = [fromIntegral $ ix + 10 * iy + 100 * iz |
                          ix <- [1..nx], iy <- [1..ny], iz <- [1..nz]]
              idxs = [R.ix3 (iz-1) (iy-1) (ix-1) |
                      ix <- [1..nx], iy <- [1..ny], iz <- [1..nz]]
              tstvals = [val R.! i | i <- idxs]
          assertBool ("Value error: " ++ show tstvals ++
                      "instead of " ++ show truevals)
            (and $ zipWith (==) tstvals truevals)
  void $ closeFile nc
