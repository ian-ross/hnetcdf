{-# LANGUAGE FlexibleInstances, ConstraintKinds, TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans -fwarn-missing-methods #-}
-- | NetCDF store instance for HMatrix vectors and matrices.
module Data.NetCDF.HMatrix
       ( HVector (..)
       , HMatrix (..)
       ) where

import Data.NetCDF.Store

import qualified Numeric.Container as C
import qualified Numeric.LinearAlgebra.Util as C
import Foreign.C
import Data.Packed.Vector
import Data.Packed.Development

newtype HVector a = HVector (C.Vector a)
                  deriving (Eq, Show)

instance NcStore HVector where
  type NcStoreExtraCon HVector a = C.Element a
  toForeignPtr (HVector v) = fst3 $ unsafeToForeignPtr v
  fromForeignPtr p s = HVector $ unsafeFromForeignPtr p 0 (Prelude.product s)
  smap f (HVector v) = HVector $ C.mapVector f v

data HMatrix a = HMatrix (C.Matrix a)

instance NcStore HMatrix where
  type NcStoreExtraCon HMatrix a = C.Element a
  toForeignPtr (HMatrix m) = fst3 $ unsafeToForeignPtr $ C.flatten m
  fromForeignPtr p s =
    let c = last s
        d = product s
    in HMatrix $ matrixFromVector RowMajor (d `div` c) (last s) $
       unsafeFromForeignPtr p 0 (Prelude.product s)
  smap f (HMatrix m) = HMatrix $ C.mapMatrix f m

instance C.Element CShort
instance C.Element CInt
instance C.Element CFloat
instance C.Element CDouble

instance C.Container C.Vector CFloat
instance C.Container C.Vector CDouble

instance C.Indexable (C.Vector CFloat) CFloat where (!) = (@>)
instance C.Indexable (C.Vector CDouble) CDouble where (!) = (@>)

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x
