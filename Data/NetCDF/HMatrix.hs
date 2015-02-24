{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | NetCDF store instance for HMatrix vectors and matrices.
module Data.NetCDF.HMatrix
       ( HVector (..)
       , HRowMajorMatrix (..)
       , HColumnMajorMatrix (..)
       ) where

import Data.NetCDF.Store

import qualified Numeric.Container as C
import qualified Numeric.LinearAlgebra.Util as C
import Foreign.C
import Data.Packed.Foreign
import Data.Packed.Vector
import Data.Packed.Development

newtype HVector a = HVector (C.Vector a)
                  deriving (Eq, Show)

instance NcStore HVector where
  toForeignPtr (HVector v) = (\(x, _, _) -> x) $ unsafeToForeignPtr v
  fromForeignPtr p s = HVector $ unsafeFromForeignPtr p 0 (Prelude.product s)
  smap f (HVector v) = HVector $ C.mapVector f v

newtype HRowMajorMatrix a = HRowMajorMatrix (C.Matrix a)

instance NcStore HRowMajorMatrix where
  toForeignPtr (HRowMajorMatrix m) = fst $ unsafeMatrixToForeignPtr m
  fromForeignPtr p s =
    let c = last s
        d = product s
    in HRowMajorMatrix $ matrixFromVector RowMajor (d `div` c) (last s) $
       unsafeFromForeignPtr p 0 (Prelude.product s)
  smap f (HRowMajorMatrix m) = HRowMajorMatrix $ C.mapMatrix f m

newtype HColumnMajorMatrix a = HColumnMajorMatrix (C.Matrix a)

instance NcStore HColumnMajorMatrix where
  toForeignPtr (HColumnMajorMatrix m) = fst $ unsafeMatrixToForeignPtr m
  fromForeignPtr p s =
    let c = last s
        d = product s
    in HColumnMajorMatrix $ matrixFromVector ColumnMajor (d `div` c) (last s) $
       unsafeFromForeignPtr p 0 (Prelude.product s)
  smap f (HColumnMajorMatrix m) = HColumnMajorMatrix $ C.mapMatrix f m

instance C.Element CShort
instance C.Element CInt
instance C.Element CFloat
instance C.Element CDouble

instance C.Container C.Vector CFloat
instance C.Container C.Vector CDouble

instance C.Indexable (C.Vector CFloat) CFloat where (!) = (@>)
instance C.Indexable (C.Vector CDouble) CDouble where (!) = (@>)
