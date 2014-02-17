{-# LANGUAGE ScopedTypeVariables #-}
import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test, assert)
import qualified Data.Vector.Storable as SV
import qualified Data.Array.Repa as R
import qualified Data.Array.Repa.Eval as RE
import Data.Array.Repa.Repr.ForeignPtr (F)
import Control.Error
import Control.Monad
import Numeric.Container as C
import Foreign.C

import Data.NetCDF
import qualified Data.NetCDF.Vector as V
import qualified Data.NetCDF.Repa as R
import qualified Data.NetCDF.HMatrix as H

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
    , testCase "Read (float, hmatrix)" (getVarHMV infile "vf" fwit)
    , testCase "Read (int, hmatrix)" (getVarHMV infile "vi" iwit)
    , testCase "Read (short, hmatrix)" (getVarHMV infile "vs" swit)
    ],
    testGroup "Variable slice access functions"
    [ testCase "Read (float, SV)" (getVarASV infile "vf" fwit)
    , testCase "Read (int, SV)" (getVarASV infile "vi" iwit)
    , testCase "Read (short, SV)" (getVarASV infile "vs" swit)
    , testCase "Read (float, Repa)" (getVarARepa infile "vf" fwit)
    , testCase "Read (int, Repa)" (getVarARepa infile "vi" iwit)
    , testCase "Read (short, Repa)" (getVarARepa infile "vs" swit)
    , testCase "Read (float, hmatrix)" (getVarAHMV infile "vf" fwit)
    , testCase "Read (int, hmatrix)" (getVarAHMV infile "vi" iwit)
    , testCase "Read (short, hmatrix)" (getVarAHMV infile "vs" swit)
    ],
    testGroup "Strided slice access functions"
    [ testCase "Read (float, SV)" (getVarSSV infile "vf" fwit)
    , testCase "Read (int, SV)" (getVarSSV infile "vi" iwit)
    , testCase "Read (short, SV)" (getVarSSV infile "vs" swit)
    , testCase "Read (float, Repa)" (getVarSRepa infile "vf" fwit)
    , testCase "Read (int, Repa)" (getVarSRepa infile "vi" iwit)
    , testCase "Read (short, Repa)" (getVarSRepa infile "vs" swit)
    , testCase "Read (float, hmatrix)" (getVarSHMV infile "vf" fwit)
    , testCase "Read (int, hmatrix)" (getVarSHMV infile "vi" iwit)
    , testCase "Read (short, hmatrix)" (getVarSHMV infile "vs" swit)
    ]
  ]


type SVRet a = IO (Either NcError (SV.Vector a))
type RepaRet3 a = IO (Either NcError (R.Array F R.DIM3 a))
type RepaRet1 a = IO (Either NcError (R.Array F R.DIM1 a))
type HMVRet a = IO (Either NcError (H.HVector a))
type HMMRet a = IO (Either NcError (H.HRowMajorMatrix a))


--------------------------------------------------------------------------------
--
--  SINGLE ITEM READ
--
--------------------------------------------------------------------------------

getVar1 :: forall a. (Eq a, Num a, Show a, NcStorable a)
        => FilePath -> String -> a -> Assertion
getVar1 f v _ = do
  enc <- openFile f
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
  enc <- openFile f
  assertBool "failed to open file" $ isRight enc
  let Right nc = enc
  case ncVar nc v of
    Nothing -> assertBool "missing variable" False
    Just var -> do
      eval <- get nc var :: SVRet a
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
  enc <- openFile f
  assertBool "failed to open file" $ isRight enc
  let Right nc = enc
  case ncVar nc v of
    Nothing -> assertBool "missing variable" False
    Just var -> do
      eval <- get nc var :: RepaRet3 a
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

getVarHMV :: forall a. (Eq a, Num a, Show a, NcStorable a)
          => FilePath -> String -> a -> Assertion
getVarHMV f v _ = do
  enc <- openFile f
  assertBool "failed to open file" $ isRight enc
  let Right nc = enc
  case ncVar nc v of
    Nothing -> assertBool "missing variable" False
    Just var -> do
      eval <- get nc var :: HMVRet a
      case eval of
        Left e -> assertBool ("read error: " ++ show e) False
        Right (H.HVector val) -> do
          let [nz, ny, nx] = map ncDimLength $ ncVarDims var
              truevals = [fromIntegral $ ix + 10 * iy + 100 * iz |
                          ix <- [1..nx], iy <- [1..ny], iz <- [1..nz]]
              idxs = [(iz - 1) * ny * nx + (iy - 1) * nx + (ix - 1) |
                      ix <- [1..nx], iy <- [1..ny], iz <- [1..nz]]
              tstvals = [val C.@> i | i <- idxs]
          assertBool ("value error: " ++ show tstvals ++
                      " instead of " ++ show truevals)
            (and $ zipWith (==) tstvals truevals)
  void $ closeFile nc



--------------------------------------------------------------------------------
--
--  VARIABLE SLICE READ
--
--------------------------------------------------------------------------------

getVarASV :: forall a. (Num a, Show a, Eq a, NcStorable a)
          => FilePath -> String -> a -> Assertion
getVarASV f v _ = do
  enc <- openFile f
  assertBool "failed to open file" $ isRight enc
  let Right nc = enc
  case ncVar nc v of
    Nothing -> assertBool "missing variable" False
    Just var -> do
      let [nz, ny, nx] = map ncDimLength $ ncVarDims var
      forM_ [1..nx] $ \ix -> do
        forM_ [1..ny] $ \iy -> do
          let start = [0, iy-1, ix-1]
              count = [nz, 1, 1]
          eval <- getA nc var start count :: SVRet a
          case eval of
            Left e -> assertBool ("read error: " ++ show e) False
            Right vals -> do
              let truevals = SV.generate nz
                             (\iz -> fromIntegral $ ix + 10 * iy + 100 * (iz+1))
              assertBool ("1: value error: " ++ show vals ++
                          " instead of " ++ show truevals) (vals == truevals)
      forM_ [1..nx] $ \ix -> do
        forM_ [1..nz] $ \iz -> do
          let start = [iz-1, 0, ix-1]
              count = [1, ny, 1]
          eval <- getA nc var start count :: SVRet a
          case eval of
            Left e -> assertBool ("read error: " ++ show e) False
            Right vals -> do
              let truevals = SV.generate ny
                             (\iy -> fromIntegral $ ix + 10 * (iy+1) + 100 * iz)
              assertBool ("2: value error: " ++ show vals ++
                          " instead of " ++ show truevals) (vals == truevals)
      forM_ [1..ny] $ \iy -> do
        forM_ [1..nz] $ \iz -> do
          let start = [iz-1, iy-1, 0]
              count = [1, 1, nx]
          eval <- getA nc var start count :: SVRet a
          case eval of
            Left e -> assertBool ("read error: " ++ show e) False
            Right vals -> do
              let truevals = SV.generate nx
                             (\ix -> fromIntegral $ (ix+1) + 10 * iy + 100 * iz)
              assertBool ("3: value error: " ++ show vals ++
                          " instead of " ++ show truevals) (vals == truevals)

getVarARepa :: forall a. (Num a, Show a, Eq a, NcStorable a)
            => FilePath -> String -> a -> Assertion
getVarARepa f v _ = do
  enc <- openFile f
  assertBool "failed to open file" $ isRight enc
  let Right nc = enc
  case ncVar nc v of
    Nothing -> assertBool "missing variable" False
    Just var -> do
      let [nz, ny, nx] = map ncDimLength $ ncVarDims var
      forM_ [1..nx] $ \ix -> do
        forM_ [1..ny] $ \iy -> do
          let start = [0, iy-1, ix-1]
              count = [nz, 1, 1]
          eval <- getA nc var start count :: RepaRet1 a
          case eval of
            Left e -> assertBool ("read error: " ++ show e) False
            Right vals -> do
              let truevals = [fromIntegral $ ix+10*iy+100*iz | iz <- [1..nz]]
                  tstvals = [vals R.! (R.ix1 (iz - 1)) | iz <- [1..nz]]
              assertBool ("1: value error: " ++ show tstvals ++
                          " instead of " ++ show truevals) (tstvals == truevals)
      forM_ [1..nx] $ \ix -> do
        forM_ [1..nz] $ \iz -> do
          let start = [iz-1, 0, ix-1]
              count = [1, ny, 1]
          eval <- getA nc var start count :: RepaRet1 a
          case eval of
            Left e -> assertBool ("read error: " ++ show e) False
            Right vals -> do
              let truevals = [fromIntegral $ ix+10*iy+100*iz | iy <- [1..ny]]
                  tstvals = [vals R.! (R.ix1 (iy - 1)) | iy <- [1..ny]]
              assertBool ("2: value error: " ++ show tstvals ++
                          " instead of " ++ show truevals) (tstvals == truevals)
      forM_ [1..ny] $ \iy -> do
        forM_ [1..nz] $ \iz -> do
          let start = [iz-1, iy-1, 0]
              count = [1, 1, nx]
          eval <- getA nc var start count :: RepaRet1 a
          case eval of
            Left e -> assertBool ("read error: " ++ show e) False
            Right vals -> do
              let truevals = [fromIntegral $ ix+10*iy+100*iz | ix<-[1..nx]]
                  tstvals = [vals R.! (R.ix1 (ix - 1)) | ix <- [1..nx]]
              assertBool ("3: value error: " ++ show tstvals ++
                          " instead of " ++ show truevals) (tstvals == truevals)

getVarAHMV :: forall a. (Num a, Show a, Eq a, NcStorable a)
           => FilePath -> String -> a -> Assertion
getVarAHMV f v _ = do
  enc <- openFile f
  assertBool "failed to open file" $ isRight enc
  let Right nc = enc
  case ncVar nc v of
    Nothing -> assertBool "missing variable" False
    Just var -> do
      let [nz, ny, nx] = map ncDimLength $ ncVarDims var
      forM_ [1..nx] $ \ix -> do
        forM_ [1..ny] $ \iy -> do
          let start = [0, iy-1, ix-1]
              count = [nz, 1, 1]
          eval <- getA nc var start count :: HMVRet a
          case eval of
            Left e -> assertBool ("read error: " ++ show e) False
            Right vals -> do
              let truevals = H.HVector $ C.buildVector (fromIntegral nz)
                             (\iz -> fromIntegral $ ix + 10 * iy + 100 * (iz+1))
              assertBool ("1: value error: " ++ show vals ++
                          " instead of " ++ show truevals) (vals == truevals)
      forM_ [1..nx] $ \ix -> do
        forM_ [1..nz] $ \iz -> do
          let start = [iz-1, 0, ix-1]
              count = [1, ny, 1]
          eval <- getA nc var start count :: HMVRet a
          case eval of
            Left e -> assertBool ("read error: " ++ show e) False
            Right vals -> do
              let truevals = H.HVector $ C.buildVector (fromIntegral ny)
                             (\iy -> fromIntegral $ ix + 10 * (iy+1) + 100 * iz)
              assertBool ("2: value error: " ++ show vals ++
                          " instead of " ++ show truevals) (vals == truevals)
      forM_ [1..ny] $ \iy -> do
        forM_ [1..nz] $ \iz -> do
          let start = [iz-1, iy-1, 0]
              count = [1, 1, nx]
          eval <- getA nc var start count :: HMVRet a
          case eval of
            Left e -> assertBool ("read error: " ++ show e) False
            Right vals -> do
              let truevals = H.HVector $ C.buildVector (fromIntegral nx)
                             (\ix -> fromIntegral $ (ix+1) + 10 * iy + 100 * iz)
              assertBool ("3: value error: " ++ show vals ++
                          " instead of " ++ show truevals) (vals == truevals)


--------------------------------------------------------------------------------
--
--  STRIDED SLICE READ
--
--------------------------------------------------------------------------------

getVarSSV :: forall a. (Num a, Show a, Eq a, NcStorable a)
          => FilePath -> String -> a -> Assertion
getVarSSV f v _ = do
  enc <- openFile f
  assertBool "failed to open file" $ isRight enc
  let Right nc = enc
  case ncVar nc v of
    Nothing -> assertBool "missing variable" False
    Just var -> do
      let [nz, ny, nx] = map ncDimLength $ ncVarDims var
      forM_ [1..nx] $ \ix -> do
        forM_ [1..ny] $ \iy -> do
          forM_ [1..nz] $ \s -> do
            let start = [0, iy-1, ix-1]
                count = [(nz + s - 1) `div` s, 1, 1]
                stride = [s, 1, 1]
            eval <- getS nc var start count stride :: SVRet a
            case eval of
              Left e -> assertBool ("read error: " ++ show e) False
              Right vals -> do
                let truevals = SV.generate ((nz + s - 1) `div` s)
                               (\iz -> fromIntegral $
                                       ix + 10 * iy + 100 * (iz * s + 1))
                assertBool ("1: value error: " ++ show vals ++
                            " instead of " ++ show truevals) (vals == truevals)
      forM_ [1..nx] $ \ix -> do
        forM_ [1..nz] $ \iz -> do
          forM_ [1..ny] $ \s -> do
            let start = [iz-1, 0, ix-1]
                count = [1, (ny + s - 1) `div` s, 1]
                stride = [1, s, 1]
            eval <- getS nc var start count stride :: SVRet a
            case eval of
              Left e -> assertBool ("read error: " ++ show e) False
              Right vals -> do
                let truevals = SV.generate ((ny + s - 1) `div` s)
                               (\iy -> fromIntegral $
                                       ix + 10 * (iy * s + 1) + 100 * iz)
                assertBool ("2: value error: " ++ show vals ++
                            " instead of " ++ show truevals) (vals == truevals)
      forM_ [1..ny] $ \iy -> do
        forM_ [1..nz] $ \iz -> do
          forM_ [1..nx] $ \s -> do
            let start = [iz-1, iy-1, 0]
                count = [1, 1, (nx + s - 1) `div` s]
                stride = [1, 1, s]
            eval <- getS nc var start count stride :: SVRet a
            case eval of
              Left e -> assertBool ("read error: " ++ show e) False
              Right vals -> do
                let truevals = SV.generate ((nx + s - 1) `div` s)
                               (\ix -> fromIntegral $
                                       (ix * s + 1) + 10 * iy + 100 * iz)
                assertBool ("3: value error: " ++ show vals ++
                            " instead of " ++ show truevals) (vals == truevals)

getVarSRepa :: forall a. (Num a, Show a, Eq a, NcStorable a)
            => FilePath -> String -> a -> Assertion
getVarSRepa f v _ = do
  enc <- openFile f
  assertBool "failed to open file" $ isRight enc
  let Right nc = enc
  case ncVar nc v of
    Nothing -> assertBool "missing variable" False
    Just var -> do
      let [nz, ny, nx] = map ncDimLength $ ncVarDims var
      forM_ [1..nx] $ \ix -> do
        forM_ [1..ny] $ \iy -> do
          forM_ [1..nz-1] $ \s -> do
            let start = [0, iy-1, ix-1]
                count = [(nz + s - 1) `div` s, 1, 1]
                stride = [s, 1, 1]
            eval <- getS nc var start count stride :: RepaRet1 a
            case eval of
              Left e -> assertBool ("read error: " ++ show e) False
              Right vals -> do
                let truevals = [fromIntegral $
                                ix + 10 * iy + 100 * ((iz - 1) * s + 1) |
                                iz <- [1..(nz + s - 1) `div` s]]
                    tstvals = [vals R.! (R.ix1 (iz - 1)) |
                               iz <- [1..(nz + s - 1) `div` s]]
                assertBool ("1: value error: " ++ show tstvals ++
                            " instead of " ++ show truevals)
                  (tstvals == truevals)
      forM_ [1..nx] $ \ix -> do
        forM_ [1..nz] $ \iz -> do
          forM_ [1..ny-1] $ \s -> do
            let start = [iz-1, 0, ix-1]
                count = [1, (ny + s - 1) `div` s, 1]
                stride = [1, s, 1]
            eval <- getS nc var start count stride :: RepaRet1 a
            case eval of
              Left e -> assertBool ("read error: " ++ show e) False
              Right vals -> do
                let truevals = [fromIntegral $
                                ix + 10 * ((iy - 1) * s + 1) + 100 * iz |
                                iy <- [1..(ny + s - 1) `div` s]]
                    tstvals = [vals R.! (R.ix1 (iy - 1)) |
                               iy <- [1..(ny + s - 1) `div` s]]
                assertBool ("2: value error: " ++ show tstvals ++
                            " instead of " ++ show truevals)
                  (tstvals == truevals)
      forM_ [1..ny] $ \iy -> do
        forM_ [1..nz] $ \iz -> do
          forM_ [1..nx-1] $ \s -> do
            let start = [iz-1, iy-1, 0]
                count = [1, 1, (nx + s - 1) `div` s]
                stride = [1, 1, s]
            eval <- getS nc var start count stride :: RepaRet1 a
            case eval of
              Left e -> assertBool ("read error: " ++ show e) False
              Right vals -> do
                let truevals = [fromIntegral $
                                ((ix - 1) * s + 1) + 10 * iy + 100 * iz |
                                ix <- [1..(nx + s - 1) `div` s]]
                    tstvals = [vals R.! (R.ix1 (ix - 1)) |
                               ix <- [1..(nx + s - 1) `div` s]]
                assertBool ("3: value error: " ++ show tstvals ++
                            " instead of " ++ show truevals)
                  (tstvals == truevals)

getVarSHMV :: forall a. (Num a, Show a, Eq a, NcStorable a)
          => FilePath -> String -> a -> Assertion
getVarSHMV f v _ = do
  enc <- openFile f
  assertBool "failed to open file" $ isRight enc
  let Right nc = enc
  case ncVar nc v of
    Nothing -> assertBool "missing variable" False
    Just var -> do
      let [nz, ny, nx] = map ncDimLength $ ncVarDims var
      forM_ [1..nx] $ \ix -> do
        forM_ [1..ny] $ \iy -> do
          forM_ [1..nz] $ \s -> do
            let start = [0, iy-1, ix-1]
                count = [(nz + s - 1) `div` s, 1, 1]
                stride = [s, 1, 1]
            eval <- getS nc var start count stride :: HMVRet a
            case eval of
              Left e -> assertBool ("read error: " ++ show e) False
              Right vals -> do
                let truevals = H.HVector $ C.buildVector
                               (fromIntegral $ (nz + s - 1) `div` s)
                               (\iz -> fromIntegral $
                                       ix + 10 * iy + 100 * (iz * s + 1))
                assertBool ("1: value error: " ++ show vals ++
                            " instead of " ++ show truevals) (vals == truevals)
      forM_ [1..nx] $ \ix -> do
        forM_ [1..nz] $ \iz -> do
          forM_ [1..ny] $ \s -> do
            let start = [iz-1, 0, ix-1]
                count = [1, (ny + s - 1) `div` s, 1]
                stride = [1, s, 1]
            eval <- getS nc var start count stride :: HMVRet a
            case eval of
              Left e -> assertBool ("read error: " ++ show e) False
              Right vals -> do
                let truevals = H.HVector $ C.buildVector
                               (fromIntegral $ (ny + s - 1) `div` s)
                               (\iy -> fromIntegral $
                                       ix + 10 * (iy * s + 1) + 100 * iz)
                assertBool ("2: value error: " ++ show vals ++
                            " instead of " ++ show truevals) (vals == truevals)
      forM_ [1..ny] $ \iy -> do
        forM_ [1..nz] $ \iz -> do
          forM_ [1..nx] $ \s -> do
            let start = [iz-1, iy-1, 0]
                count = [1, 1, (nx + s - 1) `div` s]
                stride = [1, 1, s]
            eval <- getS nc var start count stride :: HMVRet a
            case eval of
              Left e -> assertBool ("read error: " ++ show e) False
              Right vals -> do
                let truevals = H.HVector $ C.buildVector
                               (fromIntegral $ (nx + s - 1) `div` s)
                               (\ix -> fromIntegral $
                                       (ix * s + 1) + 10 * iy + 100 * iz)
                assertBool ("3: value error: " ++ show vals ++
                            " instead of " ++ show truevals) (vals == truevals)
