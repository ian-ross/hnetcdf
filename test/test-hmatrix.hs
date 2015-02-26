{-# LANGUAGE ScopedTypeVariables #-}

import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit hiding (Test, assert)
import Test.QuickCheck
import Test.QuickCheck.Monadic

import qualified Data.Map as M
import Data.Maybe
import qualified Data.Vector.Storable as SV
import Numeric.LinearAlgebra.HMatrix
import Control.Error
import Control.Monad
import Foreign.C

import Data.NetCDF
import Data.NetCDF.Vector
import Data.NetCDF.HMatrix

type HMatrixRet a = IO (Either NcError (HMatrix a))

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
  [ testGroup "HMatrix handling"
    [ testCase "Data ordering" hmatrixDataOrdering
    ]
  ]

hmatrixDataOrdering :: Assertion
hmatrixDataOrdering = do
  let xdim = NcDim "x" 5 False
      ydim = NcDim "y" 10 False
      ncout = emptyNcInfo "tst-hmatrix.nc" #
           addNcDim xdim #
           addNcDim ydim #
           addNcVar (NcVar "x" NcDouble [xdim] M.empty) #
           addNcVar (NcVar "y" NcDouble [ydim] M.empty) #
           addNcVar (NcVar "z1" NcDouble [ydim, xdim] M.empty) #
           addNcVar (NcVar "z2" NcDouble [ydim, xdim] M.empty)
      z1 = cmap realToFrac $ matrix 5 [1..50] :: Matrix CDouble
      z2 = fromColumns $ toColumns z1 :: Matrix CDouble
  let write nc = do
        let xvar = fromJust $ ncVar nc "x"
            yvar = fromJust $ ncVar nc "y"
            z1var = fromJust $ ncVar nc "z1"
            z2var = fromJust $ ncVar nc "z2"
        put nc xvar $ SV.fromList [1..5 :: CDouble]
        put nc yvar $ SV.fromList [1..10 :: CDouble]
        put nc z1var $ HMatrix z1
        put nc z2var $ HMatrix z2
        return ()
  withCreateFile ncout write (putStrLn . ("ERROR: " ++) . show)
  let read nc = do
        let z1var = fromJust $ ncVar nc "z1"
            z2var = fromJust $ ncVar nc "z2"
        Right (HMatrix z1) <- get nc z1var :: HMatrixRet CDouble
        Right (HMatrix z2) <- get nc z2var :: HMatrixRet CDouble
        return (z1, z2)
  (z1tst, z2tst) <-
    withReadFile "tst-hmatrix.nc" read (error . ("ERROR: " ++) . show)
  assertBool "Straightforward write/read" (flatten z1tst == flatten z1)
  assertBool "Columnar write/read" (flatten z2tst == flatten z2)
