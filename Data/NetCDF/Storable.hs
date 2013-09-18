-- | The mapping between types that can be stored in a NetCDF file and
-- the FFI functions needed to read and write those values is
-- maintained by the `NcStorable` type class.

module Data.NetCDF.Storable
       ( NcStorable (..)
       ) where

import Foreign.C
import Foreign.Ptr
import Foreign.Storable

import Data.NetCDF.Types
import Data.NetCDF.Store
import Data.NetCDF.Raw.PutGet


-- | Class to collect the NetCDF FFI functions needed to read and
-- write values in a NetCDF file for a given type.
class Storable a => NcStorable a where
  ncType :: a -> NcType
  ffi_put_var1 :: CInt -> CInt -> Ptr CULong -> Ptr a -> IO CInt
  ffi_get_var1 :: CInt -> CInt -> Ptr CULong -> Ptr a -> IO CInt
  ffi_put_var :: CInt -> CInt -> Ptr a -> IO CInt
  ffi_get_var :: CInt -> CInt -> Ptr a -> IO CInt
  ffi_put_vara :: CInt -> CInt -> Ptr CULong -> Ptr CULong -> Ptr a -> IO CInt
  ffi_get_vara :: CInt -> CInt -> Ptr CULong -> Ptr CULong -> Ptr a -> IO CInt
  ffi_put_vars :: CInt -> CInt -> Ptr CULong -> Ptr CULong -> Ptr CULong
               -> Ptr a -> IO CInt
  ffi_get_vars :: CInt -> CInt -> Ptr CULong -> Ptr CULong -> Ptr CULong
               -> Ptr a -> IO CInt


instance NcStorable CSChar where
  -- #define NC_BYTE 1 /**< signed 1 byte integer */
  ncType _ = NcByte
  ffi_put_var1 = nc_put_var1_schar'_
  ffi_get_var1 = nc_get_var1_schar'_
  ffi_put_var = nc_put_var_schar'_
  ffi_get_var = nc_get_var_schar'_
  ffi_put_vara = nc_put_vara_schar'_
  ffi_get_vara = nc_get_vara_schar'_
  ffi_put_vars = nc_put_vars_schar'_
  ffi_get_vars = nc_get_vars_schar'_

instance NcStorable CChar where
  -- #define NC_CHAR 2 /**< ISO/ASCII character */
  ncType _ = NcChar
  ffi_put_var1 = nc_put_var1_text'_
  ffi_get_var1 = nc_get_var1_text'_
  ffi_put_var = nc_put_var_text'_
  ffi_get_var = nc_get_var_text'_
  ffi_put_vara = nc_put_vara_text'_
  ffi_get_vara = nc_get_vara_text'_
  ffi_put_vars = nc_put_vars_text'_
  ffi_get_vars = nc_get_vars_text'_

instance NcStorable CShort where
  -- #define NC_SHORT 3 /**< signed 2 byte integer */
  ncType _ = NcShort
  ffi_put_var1 = nc_put_var1_short'_
  ffi_get_var1 = nc_get_var1_short'_
  ffi_put_var = nc_put_var_short'_
  ffi_get_var = nc_get_var_short'_
  ffi_put_vara = nc_put_vara_short'_
  ffi_get_vara = nc_get_vara_short'_
  ffi_put_vars = nc_put_vars_short'_
  ffi_get_vars = nc_get_vars_short'_

instance NcStorable CInt where
  -- #define NC_INT 4 /**< signed 4 byte integer */
  ncType _ = NcInt
  ffi_put_var1 = nc_put_var1_int'_
  ffi_get_var1 = nc_get_var1_int'_
  ffi_put_var = nc_put_var_int'_
  ffi_get_var = nc_get_var_int'_
  ffi_put_vara = nc_put_vara_int'_
  ffi_get_vara = nc_get_vara_int'_
  ffi_put_vars = nc_put_vars_int'_
  ffi_get_vars = nc_get_vars_int'_

instance NcStorable CFloat where
  -- #define NC_FLOAT 5 /**< single precision floating point number */
  ncType _ = NcFloat
  ffi_put_var1 = nc_put_var1_float'_
  ffi_get_var1 = nc_get_var1_float'_
  ffi_put_var = nc_put_var_float'_
  ffi_get_var = nc_get_var_float'_
  ffi_put_vara = nc_put_vara_float'_
  ffi_get_vara = nc_get_vara_float'_
  ffi_put_vars = nc_put_vars_float'_
  ffi_get_vars = nc_get_vars_float'_

instance NcStorable CDouble where
  -- #define NC_DOUBLE 6 /**< double precision floating point number */
  ncType _ = NcDouble
  ffi_put_var1 = nc_put_var1_double'_
  ffi_get_var1 = nc_get_var1_double'_
  ffi_put_var = nc_put_var_double'_
  ffi_get_var = nc_get_var_double'_
  ffi_put_vara = nc_put_vara_double'_
  ffi_get_vara = nc_get_vara_double'_
  ffi_put_vars = nc_put_vars_double'_
  ffi_get_vars = nc_get_vars_double'_

instance NcStorable CUChar where
  -- #define NC_UBYTE 7 /**< unsigned 1 byte int */
  ncType _ = NcUByte
  ffi_put_var1 = nc_put_var1_uchar'_
  ffi_get_var1 = nc_get_var1_uchar'_
  ffi_put_var = nc_put_var_uchar'_
  ffi_get_var = nc_get_var_uchar'_
  ffi_put_vara = nc_put_vara_uchar'_
  ffi_get_vara = nc_get_vara_uchar'_
  ffi_put_vars = nc_put_vars_uchar'_
  ffi_get_vars = nc_get_vars_uchar'_

instance NcStorable CUShort where
  -- #define NC_USHORT 8 /**< unsigned 2-byte int */
  ncType _ = NcUShort
  ffi_put_var1 = nc_put_var1_ushort'_
  ffi_get_var1 = nc_get_var1_ushort'_
  ffi_put_var = nc_put_var_ushort'_
  ffi_get_var = nc_get_var_ushort'_
  ffi_put_vara = nc_put_vara_ushort'_
  ffi_get_vara = nc_get_vara_ushort'_
  ffi_put_vars = nc_put_vars_ushort'_
  ffi_get_vars = nc_get_vars_ushort'_

instance NcStorable CUInt where
  -- #define NC_UINT 9 /**< unsigned 4-byte int */
  ncType _ = NcUInt
  ffi_put_var1 = nc_put_var1_uint'_
  ffi_get_var1 = nc_get_var1_uint'_
  ffi_put_var = nc_put_var_uint'_
  ffi_get_var = nc_get_var_uint'_
  ffi_put_vara = nc_put_vara_uint'_
  ffi_get_vara = nc_get_vara_uint'_
  ffi_put_vars = nc_put_vars_uint'_
  ffi_get_vars = nc_get_vars_uint'_

instance NcStorable CLong where
  -- #define NC_INT64 10 /**< signed 8-byte int */
  ncType _ = NcInt64
  ffi_put_var1 = nc_put_var1_long'_
  ffi_get_var1 = nc_get_var1_long'_
  ffi_put_var = nc_put_var_long'_
  ffi_get_var = nc_get_var_long'_
  ffi_put_vara = nc_put_vara_long'_
  ffi_get_vara = nc_get_vara_long'_
  ffi_put_vars = nc_put_vars_long'_
  ffi_get_vars = nc_get_vars_long'_

