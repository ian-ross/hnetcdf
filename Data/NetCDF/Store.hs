{-# LANGUAGE TypeFamilies, FlexibleInstances #-}
module Data.NetCDF.Store where

import Foreign.Storable
import Foreign.ForeignPtr

import Data.Vector.Storable

import Data.Array.Repa
import qualified Data.Array.Repa as R
import qualified Data.Array.Repa.Repr.ForeignPtr as RF
import Data.Array.Repa.Repr.ForeignPtr (F)

import Data.NetCDF.Types

class NcStore s where
  type Size s :: *
  toForeignPtr :: Storable e => s e -> ForeignPtr e
  fromForeignPtr :: Storable e => ForeignPtr e -> Size s -> s e
  smap :: (Storable a, Storable b) => (a -> b) -> s a -> s b

instance NcStore Vector where
  type Size Vector = Int
  toForeignPtr = fst . unsafeToForeignPtr0
  fromForeignPtr = unsafeFromForeignPtr0
  smap = Data.Vector.Storable.map

instance Shape sh => NcStore (Array F sh) where
  type Size (Array F sh) = sh
  toForeignPtr = RF.toForeignPtr
  fromForeignPtr = flip RF.fromForeignPtr
  smap f s = computeS $ R.map f s
