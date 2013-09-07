{-# LANGUAGE ScopedTypeVariables #-}
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

fwit :: CFloat
fwit = undefined

iwit :: CInt
iwit = undefined

swit :: CShort
swit = undefined

tests :: [Test]
tests =
  [ testGroup "Single item access functions"
    [ testCase "Read (float)" (getVar1 infile "vf" fwit)
    , testCase "Read (int)" (getVar1 infile "vi" iwit)
    , testCase "Read (short)" (getVar1 infile "vs" swit)
    ],
    testGroup "Whole variable access functions"
    [ testCase "Read (float, SV)" (getVarSV infile "vf" fwit)
    , testCase "Read (int, SV)" (getVarSV infile "vi" iwit)
    , testCase "Read (short, SV)" (getVarSV infile "vs" swit)
    , testCase "Read (float, Repa)" (getVarRepa infile "vf" fwit)
    , testCase "Read (int, Repa)" (getVarRepa infile "vi" iwit)
    , testCase "Read (short, Repa)" (getVarRepa infile "vs" swit)
    ]
  ]


--------------------------------------------------------------------------------
--
--  SINGLE ITEM READ
--
--------------------------------------------------------------------------------

getVar1 :: forall a. (Eq a, Num a, Show a, NcStorable a)
        => FilePath -> String -> a -> Assertion
getVar1 f v _ = do
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
            eval <- get1 nc var [iz-1, iy-1, ix-1] :: IO (Either NcError a)
            case eval of
              Left e -> assertBool ("read error: " ++ show e) False
              Right val -> do
                let trueval = fromIntegral $ ix + 10 * iy + 100 * iz
                assertBool ("value error: " ++ show val ++
                            " instead of " ++ show trueval)
                  (val == trueval)
  void $ closeFile nc



--------------------------------------------------------------------------------
--
--  WHOLE VARIABLE READ
--
--------------------------------------------------------------------------------

getVarSV :: forall a. (Eq a, Num a, Show a, NcStorable a)
         => FilePath -> String -> a -> Assertion
getVarSV f v _ = do
  enc <- openFile f ReadMode
  assertBool "failed to open file" $ isRight enc
  let Right nc = enc
  case ncVar nc v of
    Nothing -> assertBool "missing variable" False
    Just var -> do
      eval <- get nc var :: IO (Either NcError (SV.Vector a))
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

getVarRepa :: forall a. (Eq a, Num a, Show a, NcStorable a)
           => FilePath -> String -> a -> Assertion
getVarRepa f v _ = do
  enc <- openFile f ReadMode
  assertBool "failed to open file" $ isRight enc
  let Right nc = enc
  case ncVar nc v of
    Nothing -> assertBool "missing variable" False
    Just var -> do
      eval <- get nc var :: IO (Either NcError (R.Array F R.DIM3 a))
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
