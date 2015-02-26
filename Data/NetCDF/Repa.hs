{-# LANGUAGE FlexibleInstances, ConstraintKinds, TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | NetCDF store instance for Repa arrays.
module Data.NetCDF.Repa where

import Data.NetCDF.Store

import Data.Array.Repa
import qualified Data.Array.Repa as R
import qualified Data.Array.Repa.Repr.ForeignPtr as RF
import Data.Array.Repa.Repr.ForeignPtr (F)

instance Shape sh => NcStore (Array F sh) where
  type NcStoreExtraCon (Array F sh) a = ()
  toForeignPtr = RF.toForeignPtr
  fromForeignPtr p s = RF.fromForeignPtr (shapeOfList $ reverse s) p
  smap f s = computeS $ R.map f s
