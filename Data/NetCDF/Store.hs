{-# LANGUAGE FlexibleInstances #-}
-- | The /store polymorphism/ for the functions to get the values of
-- NetCDF variables relies on a simple `NcStore` typeclass for
-- converting between /store/ values and @ForeignPtr@s.

module Data.NetCDF.Store where

import Foreign.Storable
import Foreign.ForeignPtr


-- | Class representing containers suitable for storing values read
-- from NetCDF variables.  Just has methods to convert back and forth
-- between the store and a foreign pointer, and to perform simple
-- mapping over the store.
class NcStore s where
  toForeignPtr :: Storable e => s e -> ForeignPtr e
  fromForeignPtr :: Storable e => ForeignPtr e -> [Int] -> s e
  smap :: (Storable a, Storable b) => (a -> b) -> s a -> s b
