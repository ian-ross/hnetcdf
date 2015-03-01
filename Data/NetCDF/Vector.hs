{-# LANGUAGE FlexibleInstances, ConstraintKinds, TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | NetCDF store instance for Storable Vectors.
module Data.NetCDF.Vector where

import Data.NetCDF.Store

import qualified Data.Vector.Storable as SV

instance NcStore SV.Vector where
  toForeignPtr = fst . SV.unsafeToForeignPtr0
  fromForeignPtr p s = SV.unsafeFromForeignPtr0 p (Prelude.product s)
  smap = SV.map
