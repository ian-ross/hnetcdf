{-# LANGUAGE FlexibleInstances, ConstraintKinds, TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans -fwarn-missing-methods #-}
-- | NetCDF store instance for HMatrix vectors and matrices.
module Data.NetCDF.HMatrix
       ( HVector (..)
       , HMatrix (..)
       ) where

import Data.NetCDF.Store

import qualified Numeric.LinearAlgebra as C
import qualified Numeric.LinearAlgebra.Devel as C
import Foreign.C.Types

newtype HVector a = HVector (C.Vector a)
                  deriving (Eq, Show)

instance NcStore HVector where
  type NcStoreExtraCon HVector a = (C.Element a, C.Container C.Vector a)
  toForeignPtr (HVector v) = fst3 $ C.unsafeToForeignPtr v
  fromForeignPtr p s = HVector $ C.unsafeFromForeignPtr p 0 (Prelude.product s)
  smap f (HVector v) = let f' _ = f
                        in HVector $ C.mapVectorWithIndex f' v

data HMatrix a = HMatrix (C.Matrix a)

instance NcStore HMatrix where
  type NcStoreExtraCon HMatrix a = (C.Element a, Num a, C.Container C.Vector a)
  toForeignPtr (HMatrix m) = fst3 $ C.unsafeToForeignPtr $ C.flatten m
  fromForeignPtr p s =
    let c = last s
        d = product s
    in HMatrix $ C.matrixFromVector C.RowMajor (d `div` c) (last s) $
       C.unsafeFromForeignPtr p 0 (Prelude.product s)
  smap f (HMatrix m) =
    let f' _ = f
        f''  = C.mapVectorWithIndex f'
    in HMatrix $ C.liftMatrix f''  m

instance C.Element CShort
instance C.Element CFloat
instance C.Element CDouble

instance C.Container C.Vector CShort
instance C.Container C.Vector CFloat
instance C.Container C.Vector CDouble

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x
