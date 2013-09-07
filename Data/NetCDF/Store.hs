{-# LANGUAGE FlexibleInstances #-}
module Data.NetCDF.Store where

import Data.List (reverse)
import Foreign.Storable
import Foreign.ForeignPtr

import Data.Vector.Storable hiding (reverse)

import Data.Array.Repa
import qualified Data.Array.Repa as R
import qualified Data.Array.Repa.Repr.ForeignPtr as RF
import Data.Array.Repa.Repr.ForeignPtr (F)

import Data.NetCDF.Types

class NcStore s where
  toForeignPtr :: Storable e => s e -> ForeignPtr e
  fromForeignPtr :: Storable e => ForeignPtr e -> [Int] -> s e
  smap :: (Storable a, Storable b) => (a -> b) -> s a -> s b

instance NcStore Vector where
  toForeignPtr = fst . unsafeToForeignPtr0
  fromForeignPtr p s = unsafeFromForeignPtr0 p (Prelude.product s)
  smap = Data.Vector.Storable.map

instance Shape sh => NcStore (Array F sh) where
  toForeignPtr = RF.toForeignPtr
  fromForeignPtr p s = RF.fromForeignPtr (shapeOfList $ reverse s) p
  smap f s = computeS $ R.map f s
