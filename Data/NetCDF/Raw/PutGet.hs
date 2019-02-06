{-# LANGUAGE ForeignFunctionInterface #-}

-- | Raw FFI bindings for NetCDF functions for reading and writing
-- data values.

module Data.NetCDF.Raw.PutGet where

import Data.NetCDF.Raw.Utils


-- WRITING AND READING SINGLE DATA VALUES

nc_put_var1_String :: CInt -> CInt -> Ptr CULong -> String -> IO CInt
nc_put_var1_String ncid varid idx s =
  withCStringPtr s $ nc_put_var1'_ ncid varid idx
foreign import ccall safe "netcdf.h nc_put_var1"
  nc_put_var1'_ :: CInt -> CInt -> Ptr CULong -> Ptr (Ptr CChar) -> IO CInt

nc_put_var1_htext :: CInt -> CInt -> Ptr CULong -> String -> IO CInt
nc_put_var1_htext ncid varid idx s =
  withCString s $ nc_put_var1_text'_ ncid varid idx
foreign import ccall safe "netcdf.h nc_put_var1_text"
  nc_put_var1_text'_ :: CInt -> CInt -> Ptr CULong -> Ptr CChar -> IO CInt
foreign import ccall safe "netcdf.h nc_put_var1_uchar"
  nc_put_var1_uchar'_ :: CInt -> CInt -> Ptr CULong -> Ptr CUChar -> IO CInt
foreign import ccall safe "netcdf.h nc_put_var1_schar"
  nc_put_var1_schar'_ :: CInt -> CInt -> Ptr CULong -> Ptr CSChar -> IO CInt
foreign import ccall safe "netcdf.h nc_put_var1_short"
  nc_put_var1_short'_ :: CInt -> CInt -> Ptr CULong -> Ptr CShort -> IO CInt
foreign import ccall safe "netcdf.h nc_put_var1_int"
  nc_put_var1_int'_ :: CInt -> CInt -> Ptr CULong -> Ptr CInt -> IO CInt
foreign import ccall safe "netcdf.h nc_put_var1_long"
  nc_put_var1_long'_ :: CInt -> CInt -> Ptr CULong -> Ptr CLong -> IO CInt
foreign import ccall safe "netcdf.h nc_put_var1_float"
  nc_put_var1_float'_ :: CInt -> CInt -> Ptr CULong -> Ptr CFloat -> IO CInt
foreign import ccall safe "netcdf.h nc_put_var1_double"
  nc_put_var1_double'_ :: CInt -> CInt -> Ptr CULong -> Ptr CDouble -> IO CInt
foreign import ccall safe "netcdf.h nc_put_var1_ushort"
  nc_put_var1_ushort'_ :: CInt -> CInt -> Ptr CULong -> Ptr CUShort -> IO CInt
foreign import ccall safe "netcdf.h nc_put_var1_uint"
  nc_put_var1_uint'_ :: CInt -> CInt -> Ptr CULong -> Ptr CUInt -> IO CInt
foreign import ccall safe "netcdf.h nc_put_var1_longlong"
  nc_put_var1_longlong'_ :: CInt -> CInt -> Ptr CULong -> Ptr CLLong -> IO CInt
foreign import ccall safe "netcdf.h nc_put_var1_ulonglong"
  nc_put_var1_ulonglong'_ :: CInt -> CInt -> Ptr CULong
                          -> Ptr CULLong -> IO CInt

nc_get_var1_htext :: CInt -> CInt -> Ptr CULong -> IO (CInt, String)
nc_get_var1_htext ncid varid idx =
  allocaArray 1 $ \sp -> do
    res <- nc_get_var1_text'_ ncid varid idx sp
    s <- peekCString sp
    return (res, s)
foreign import ccall safe "netcdf.h nc_get_var1_text"
  nc_get_var1_text'_ :: CInt -> CInt -> Ptr CULong -> Ptr CChar -> IO CInt
foreign import ccall safe "netcdf.h nc_get_var1_uchar"
  nc_get_var1_uchar'_ :: CInt -> CInt -> Ptr CULong -> Ptr CUChar -> IO CInt
foreign import ccall safe "netcdf.h nc_get_var1_schar"
  nc_get_var1_schar'_ :: CInt -> CInt -> Ptr CULong -> Ptr CSChar -> IO CInt
foreign import ccall safe "netcdf.h nc_get_var1_short"
  nc_get_var1_short'_ :: CInt -> CInt -> Ptr CULong -> Ptr CShort -> IO CInt
foreign import ccall safe "netcdf.h nc_get_var1_int"
  nc_get_var1_int'_ :: CInt -> CInt -> Ptr CULong -> Ptr CInt -> IO CInt
foreign import ccall safe "netcdf.h nc_get_var1_long"
  nc_get_var1_long'_ :: CInt -> CInt -> Ptr CULong -> Ptr CLong -> IO CInt
foreign import ccall safe "netcdf.h nc_get_var1_float"
  nc_get_var1_float'_ :: CInt -> CInt -> Ptr CULong -> Ptr CFloat -> IO CInt
foreign import ccall safe "netcdf.h nc_get_var1_double"
  nc_get_var1_double'_ :: CInt -> CInt -> Ptr CULong -> Ptr CDouble -> IO CInt
foreign import ccall safe "netcdf.h nc_get_var1_ushort"
  nc_get_var1_ushort'_ :: CInt -> CInt -> Ptr CULong -> Ptr CUShort -> IO CInt
foreign import ccall safe "netcdf.h nc_get_var1_uint"
  nc_get_var1_uint'_ :: CInt -> CInt -> Ptr CULong -> Ptr CUInt -> IO CInt
foreign import ccall safe "netcdf.h nc_get_var1_longlong"
  nc_get_var1_longlong'_ :: CInt -> CInt -> Ptr CULong -> Ptr CLLong -> IO CInt
foreign import ccall safe "netcdf.h nc_get_var1_ulonglong"
  nc_get_var1_ulonglong'_ :: CInt -> CInt -> Ptr CULong
                          -> Ptr CULLong -> IO CInt


-- WRITING AND READING WHOLE VARIABLES

nc_put_var_htext :: CInt -> CInt -> String -> IO CInt
nc_put_var_htext ncid varid s = withCString s $ nc_put_var_text'_ ncid varid
foreign import ccall safe "netcdf.h nc_put_var_text"
  nc_put_var_text'_ :: CInt -> CInt -> Ptr CChar -> IO CInt
foreign import ccall safe "netcdf.h nc_put_var_uchar"
  nc_put_var_uchar'_ :: CInt -> CInt -> Ptr CUChar -> IO CInt
foreign import ccall safe "netcdf.h nc_put_var_schar"
  nc_put_var_schar'_ :: CInt -> CInt -> Ptr CSChar -> IO CInt
foreign import ccall safe "netcdf.h nc_put_var_short"
  nc_put_var_short'_ :: CInt -> CInt -> Ptr CShort -> IO CInt
foreign import ccall safe "netcdf.h nc_put_var_int"
  nc_put_var_int'_ :: CInt -> CInt -> Ptr CInt -> IO CInt
foreign import ccall safe "netcdf.h nc_put_var_long"
  nc_put_var_long'_ :: CInt -> CInt -> Ptr CLong -> IO CInt
foreign import ccall safe "netcdf.h nc_put_var_float"
  nc_put_var_float'_ :: CInt -> CInt -> Ptr CFloat -> IO CInt
foreign import ccall safe "netcdf.h nc_put_var_double"
  nc_put_var_double'_ :: CInt -> CInt -> Ptr CDouble -> IO CInt
foreign import ccall safe "netcdf.h nc_put_var_ushort"
  nc_put_var_ushort'_ :: CInt -> CInt -> Ptr CUShort -> IO CInt
foreign import ccall safe "netcdf.h nc_put_var_uint"
  nc_put_var_uint'_ :: CInt -> CInt -> Ptr CUInt -> IO CInt
foreign import ccall safe "netcdf.h nc_put_var_longlong"
  nc_put_var_longlong'_ :: CInt -> CInt -> Ptr CLLong -> IO CInt
foreign import ccall safe "netcdf.h nc_put_var_ulonglong"
  nc_put_var_ulonglong'_ :: CInt -> CInt -> Ptr CULLong -> IO CInt

nc_get_var_htext :: CInt -> CInt -> Int -> IO (CInt, String)
nc_get_var_htext ncid varid len =
  allocaArray len $ \sp -> do
    res <- nc_get_var_text'_ ncid varid sp
    s <- peekCString sp
    return (res, s)
foreign import ccall safe "netcdf.h nc_get_var_text"
  nc_get_var_text'_ :: CInt -> CInt -> Ptr CChar -> IO CInt
foreign import ccall safe "netcdf.h nc_get_var_uchar"
  nc_get_var_uchar'_ :: CInt -> CInt -> Ptr CUChar -> IO CInt
foreign import ccall safe "netcdf.h nc_get_var_schar"
  nc_get_var_schar'_ :: CInt -> CInt -> Ptr CSChar -> IO CInt
foreign import ccall safe "netcdf.h nc_get_var_short"
  nc_get_var_short'_ :: CInt -> CInt -> Ptr CShort -> IO CInt
foreign import ccall safe "netcdf.h nc_get_var_int"
  nc_get_var_int'_ :: CInt -> CInt -> Ptr CInt -> IO CInt
foreign import ccall safe "netcdf.h nc_get_var_long"
  nc_get_var_long'_ :: CInt -> CInt -> Ptr CLong -> IO CInt
foreign import ccall safe "netcdf.h nc_get_var_float"
  nc_get_var_float'_ :: CInt -> CInt -> Ptr CFloat -> IO CInt
foreign import ccall safe "netcdf.h nc_get_var_double"
  nc_get_var_double'_ :: CInt -> CInt -> Ptr CDouble -> IO CInt
foreign import ccall safe "netcdf.h nc_get_var_ushort"
  nc_get_var_ushort'_ :: CInt -> CInt -> Ptr CUShort -> IO CInt
foreign import ccall safe "netcdf.h nc_get_var_uint"
  nc_get_var_uint'_ :: CInt -> CInt -> Ptr CUInt -> IO CInt
foreign import ccall safe "netcdf.h nc_get_var_longlong"
  nc_get_var_longlong'_ :: CInt -> CInt -> Ptr CLLong -> IO CInt
foreign import ccall safe "netcdf.h nc_get_var_ulonglong"
  nc_get_var_ulonglong'_ :: CInt -> CInt -> Ptr CULLong -> IO CInt


-- WRITING AND READING AN ARRAY

foreign import ccall safe "netcdf.h nc_put_vara_text"
  nc_put_vara_text'_ :: CInt -> CInt -> Ptr CULong -> Ptr CULong
                     -> Ptr CChar -> IO CInt
foreign import ccall safe "netcdf.h nc_put_vara_uchar"
  nc_put_vara_uchar'_ :: CInt -> CInt -> Ptr CULong -> Ptr CULong
                      -> Ptr CUChar -> IO CInt
foreign import ccall safe "netcdf.h nc_put_vara_schar"
  nc_put_vara_schar'_ :: CInt -> CInt -> Ptr CULong -> Ptr CULong
                     -> Ptr CSChar -> IO CInt
foreign import ccall safe "netcdf.h nc_put_vara_short"
  nc_put_vara_short'_ :: CInt -> CInt -> Ptr CULong -> Ptr CULong
                      -> Ptr CShort -> IO CInt
foreign import ccall safe "netcdf.h nc_put_vara_int"
  nc_put_vara_int'_ :: CInt -> CInt -> Ptr CULong -> Ptr CULong
                      -> Ptr CInt -> IO CInt
foreign import ccall safe "netcdf.h nc_put_vara_long"
  nc_put_vara_long'_ :: CInt -> CInt -> Ptr CULong -> Ptr CULong
                      -> Ptr CLong -> IO CInt
foreign import ccall safe "netcdf.h nc_put_vara_float"
  nc_put_vara_float'_ :: CInt -> CInt -> Ptr CULong -> Ptr CULong
                      -> Ptr CFloat -> IO CInt
foreign import ccall safe "netcdf.h nc_put_vara_double"
  nc_put_vara_double'_ :: CInt -> CInt -> Ptr CULong -> Ptr CULong
                      -> Ptr CDouble -> IO CInt
foreign import ccall safe "netcdf.h nc_put_vara_ushort"
  nc_put_vara_ushort'_ :: CInt -> CInt -> Ptr CULong -> Ptr CULong
                      -> Ptr CUShort -> IO CInt
foreign import ccall safe "netcdf.h nc_put_vara_uint"
  nc_put_vara_uint'_ :: CInt -> CInt -> Ptr CULong -> Ptr CULong
                      -> Ptr CUInt -> IO CInt
foreign import ccall safe "netcdf.h nc_put_vara_longlong"
  nc_put_vara_longlong'_ :: CInt -> CInt -> Ptr CULong -> Ptr CULong
                      -> Ptr CLLong -> IO CInt
foreign import ccall safe "netcdf.h nc_put_vara_ulonglong"
  nc_put_vara_ulonglong'_ :: CInt -> CInt -> Ptr CULong -> Ptr CULong
                      -> Ptr CULLong -> IO CInt

-- nc_get_vara_htext :: CInt -> CInt -> Ptr CULong -> Ptr CULong
--                   -> IO (CInt, String)
-- nc_get_vara_htext ncid varid start count =
--   allocaArray (product count) $ \sp -> do
--     res <- nc_get_vara_text'_ ncid varid start count sp
--     s <- peekCString sp
--     return (res, s)
foreign import ccall safe "netcdf.h nc_get_vara_text"
  nc_get_vara_text'_ :: CInt -> CInt -> Ptr CULong -> Ptr CULong
                     -> Ptr CChar -> IO CInt
foreign import ccall safe "netcdf.h nc_get_vara_uchar"
  nc_get_vara_uchar'_ :: CInt -> CInt -> Ptr CULong -> Ptr CULong
                     -> Ptr CUChar -> IO CInt
foreign import ccall safe "netcdf.h nc_get_vara_schar"
  nc_get_vara_schar'_ :: CInt -> CInt -> Ptr CULong -> Ptr CULong
                     -> Ptr CSChar -> IO CInt
foreign import ccall safe "netcdf.h nc_get_vara_short"
  nc_get_vara_short'_ :: CInt -> CInt -> Ptr CULong -> Ptr CULong
                     -> Ptr CShort -> IO CInt
foreign import ccall safe "netcdf.h nc_get_vara_int"
  nc_get_vara_int'_ :: CInt -> CInt -> Ptr CULong -> Ptr CULong
                     -> Ptr CInt -> IO CInt
foreign import ccall safe "netcdf.h nc_get_vara_long"
  nc_get_vara_long'_ :: CInt -> CInt -> Ptr CULong -> Ptr CULong
                     -> Ptr CLong -> IO CInt
foreign import ccall safe "netcdf.h nc_get_vara_float"
  nc_get_vara_float'_ :: CInt -> CInt -> Ptr CULong -> Ptr CULong
                     -> Ptr CFloat -> IO CInt
foreign import ccall safe "netcdf.h nc_get_vara_double"
  nc_get_vara_double'_ :: CInt -> CInt -> Ptr CULong -> Ptr CULong
                     -> Ptr CDouble -> IO CInt
foreign import ccall safe "netcdf.h nc_get_vara_ushort"
  nc_get_vara_ushort'_ :: CInt -> CInt -> Ptr CULong -> Ptr CULong
                     -> Ptr CUShort -> IO CInt
foreign import ccall safe "netcdf.h nc_get_vara_uint"
  nc_get_vara_uint'_ :: CInt -> CInt -> Ptr CULong -> Ptr CULong
                     -> Ptr CUInt -> IO CInt
foreign import ccall safe "netcdf.h nc_get_vara_longlong"
  nc_get_vara_longlong'_ :: CInt -> CInt -> Ptr CULong -> Ptr CULong
                     -> Ptr CLLong -> IO CInt
foreign import ccall safe "netcdf.h nc_get_vara_ulonglong"
  nc_get_vara_ulonglong'_ :: CInt -> CInt -> Ptr CULong -> Ptr CULong
                     -> Ptr CULLong -> IO CInt


-- WRITING AND READING A SLICED ARRAY

foreign import ccall safe "netcdf.h nc_put_vars_text"
  nc_put_vars_text'_ :: CInt -> CInt -> Ptr CULong -> Ptr CULong -> Ptr CULong
                     -> Ptr CChar -> IO CInt
foreign import ccall safe "netcdf.h nc_put_vars_uchar"
  nc_put_vars_uchar'_ :: CInt -> CInt -> Ptr CULong -> Ptr CULong -> Ptr CULong
                     -> Ptr CUChar -> IO CInt
foreign import ccall safe "netcdf.h nc_put_vars_schar"
  nc_put_vars_schar'_ :: CInt -> CInt -> Ptr CULong -> Ptr CULong -> Ptr CULong
                     -> Ptr CSChar -> IO CInt
foreign import ccall safe "netcdf.h nc_put_vars_short"
  nc_put_vars_short'_ :: CInt -> CInt -> Ptr CULong -> Ptr CULong -> Ptr CULong
                     -> Ptr CShort -> IO CInt
foreign import ccall safe "netcdf.h nc_put_vars_int"
  nc_put_vars_int'_ :: CInt -> CInt -> Ptr CULong -> Ptr CULong -> Ptr CULong
                     -> Ptr CInt -> IO CInt
foreign import ccall safe "netcdf.h nc_put_vars_long"
  nc_put_vars_long'_ :: CInt -> CInt -> Ptr CULong -> Ptr CULong -> Ptr CULong
                     -> Ptr CLong -> IO CInt
foreign import ccall safe "netcdf.h nc_put_vars_float"
  nc_put_vars_float'_ :: CInt -> CInt -> Ptr CULong -> Ptr CULong -> Ptr CULong
                     -> Ptr CFloat -> IO CInt
foreign import ccall safe "netcdf.h nc_put_vars_double"
  nc_put_vars_double'_ :: CInt -> CInt -> Ptr CULong -> Ptr CULong -> Ptr CULong
                     -> Ptr CDouble -> IO CInt
foreign import ccall safe "netcdf.h nc_put_vars_ushort"
  nc_put_vars_ushort'_ :: CInt -> CInt -> Ptr CULong -> Ptr CULong -> Ptr CULong
                     -> Ptr CUShort -> IO CInt
foreign import ccall safe "netcdf.h nc_put_vars_uint"
  nc_put_vars_uint'_ :: CInt -> CInt -> Ptr CULong -> Ptr CULong -> Ptr CULong
                     -> Ptr CUInt -> IO CInt
foreign import ccall safe "netcdf.h nc_put_vars_longlong"
  nc_put_vars_longlong'_ :: CInt -> CInt -> Ptr CULong -> Ptr CULong
                         -> Ptr CULong -> Ptr CLLong -> IO CInt
foreign import ccall safe "netcdf.h nc_put_vars_ulonglong"
  nc_put_vars_ulonglong'_ :: CInt -> CInt -> Ptr CULong -> Ptr CULong
                          -> Ptr CULong -> Ptr CULLong -> IO CInt

foreign import ccall safe "netcdf.h nc_get_vars_text"
  nc_get_vars_text'_ :: CInt -> CInt -> Ptr CULong -> Ptr CULong -> Ptr CULong
                     -> Ptr CChar -> IO CInt
foreign import ccall safe "netcdf.h nc_get_vars_uchar"
  nc_get_vars_uchar'_ :: CInt -> CInt -> Ptr CULong -> Ptr CULong -> Ptr CULong
                     -> Ptr CUChar -> IO CInt
foreign import ccall safe "netcdf.h nc_get_vars_schar"
  nc_get_vars_schar'_ :: CInt -> CInt -> Ptr CULong -> Ptr CULong -> Ptr CULong
                     -> Ptr CSChar -> IO CInt
foreign import ccall safe "netcdf.h nc_get_vars_short"
  nc_get_vars_short'_ :: CInt -> CInt -> Ptr CULong -> Ptr CULong -> Ptr CULong
                     -> Ptr CShort -> IO CInt
foreign import ccall safe "netcdf.h nc_get_vars_int"
  nc_get_vars_int'_ :: CInt -> CInt -> Ptr CULong -> Ptr CULong -> Ptr CULong
                     -> Ptr CInt -> IO CInt
foreign import ccall safe "netcdf.h nc_get_vars_long"
  nc_get_vars_long'_ :: CInt -> CInt -> Ptr CULong -> Ptr CULong -> Ptr CULong
                     -> Ptr CLong -> IO CInt
foreign import ccall safe "netcdf.h nc_get_vars_float"
  nc_get_vars_float'_ :: CInt -> CInt -> Ptr CULong -> Ptr CULong -> Ptr CULong
                     -> Ptr CFloat -> IO CInt
foreign import ccall safe "netcdf.h nc_get_vars_double"
  nc_get_vars_double'_ :: CInt -> CInt -> Ptr CULong -> Ptr CULong -> Ptr CULong
                     -> Ptr CDouble -> IO CInt
foreign import ccall safe "netcdf.h nc_get_vars_ushort"
  nc_get_vars_ushort'_ :: CInt -> CInt -> Ptr CULong -> Ptr CULong -> Ptr CULong
                     -> Ptr CUShort -> IO CInt
foreign import ccall safe "netcdf.h nc_get_vars_uint"
  nc_get_vars_uint'_ :: CInt -> CInt -> Ptr CULong -> Ptr CULong -> Ptr CULong
                     -> Ptr CUInt -> IO CInt
foreign import ccall safe "netcdf.h nc_get_vars_longlong"
  nc_get_vars_longlong'_ :: CInt -> CInt -> Ptr CULong -> Ptr CULong
                         -> Ptr CULong -> Ptr CLLong -> IO CInt
foreign import ccall safe "netcdf.h nc_get_vars_ulonglong"
  nc_get_vars_ulonglong'_ :: CInt -> CInt -> Ptr CULong -> Ptr CULong
                          -> Ptr CULong -> Ptr CULLong -> IO CInt


-- WRITING AND READING A MAPPED ARRAY

-- int nc_put_varm_text(int ncid, int varid, const size_t *startp,
--                      const size_t *countp, const ptrdiff_t *stridep,
--                      const ptrdiff_t *imapp, const char *op);
-- int nc_put_varm_uchar(int ncid, int varid, const size_t *startp,
--                       const size_t *countp, const ptrdiff_t *stridep,
--                       const ptrdiff_t *imapp, const unsigned char *op);
-- int nc_put_varm_schar(int ncid, int varid, const size_t *startp,
--                       const size_t *countp, const ptrdiff_t *stridep,
--                       const ptrdiff_t *imapp, const signed char *op);
-- int nc_put_varm_short(int ncid, int varid, const size_t *startp,
--                       const size_t *countp, const ptrdiff_t *stridep,
--                       const ptrdiff_t *imapp, const short *op);
-- int nc_put_varm_int(int ncid, int varid, const size_t *startp,
--                     const size_t *countp, const ptrdiff_t *stridep,
--                     const ptrdiff_t *imapp, const int *op);
-- int nc_put_varm_long(int ncid, int varid, const size_t *startp,
--                      const size_t *countp, const ptrdiff_t *stridep,
--                      const ptrdiff_t *imapp, const long *op);
-- int nc_put_varm_float(int ncid, int varid,const size_t *startp,
--                       const size_t *countp, const ptrdiff_t *stridep,
--                       const ptrdiff_t *imapp, const float *op);
-- int nc_put_varm_double(int ncid, int varid, const size_t *startp,
--                        const size_t *countp, const ptrdiff_t *stridep,
--                        const ptrdiff_t *imapp, const double *op);
-- int nc_put_varm_ushort(int ncid, int varid, const size_t *startp,
--                        const size_t *countp, const ptrdiff_t *stridep,
--                        const ptrdiff_t * imapp, const unsigned short *op);
-- int nc_put_varm_uint(int ncid, int varid, const size_t *startp,
--                      const size_t *countp, const ptrdiff_t *stridep,
--                      const ptrdiff_t * imapp, const unsigned int *op);
-- int nc_put_varm_longlong(int ncid, int varid, const size_t *startp,
--                          const size_t *countp, const ptrdiff_t *stridep,
--                          const ptrdiff_t * imapp, const long long *op);
-- int nc_put_varm_ulonglong(int ncid, int varid, const size_t *startp,
--                           const size_t *countp, const ptrdiff_t *stridep,
--                           const ptrdiff_t * imapp,
--                           const unsigned long long *op);

-- int nc_get_varm_text(int ncid, int varid, const size_t *startp,
--                      const size_t *countp, const ptrdiff_t *stridep,
--                      const ptrdiff_t *imapp, char *ip);
-- int nc_get_varm_uchar(int ncid, int varid, const size_t *startp,
--                       const size_t *countp, const ptrdiff_t *stridep,
--                       const ptrdiff_t *imapp, unsigned char *ip);
-- int nc_get_varm_schar(int ncid, int varid, const size_t *startp,
--                       const size_t *countp, const ptrdiff_t *stridep,
--                       const ptrdiff_t *imapp, signed char *ip);
-- int nc_get_varm_short(int ncid, int varid, const size_t *startp,
--                       const size_t *countp, const ptrdiff_t *stridep,
--                       const ptrdiff_t *imapp, short *ip);
-- int nc_get_varm_int(int ncid, int varid, const size_t *startp,
--                     const size_t *countp, const ptrdiff_t *stridep,
--                     const ptrdiff_t *imapp, int *ip);
-- int nc_get_varm_long(int ncid, int varid, const size_t *startp,
--                      const size_t *countp, const ptrdiff_t *stridep,
--                      const ptrdiff_t *imapp, long *ip);
-- int nc_get_varm_float(int ncid, int varid,const size_t *startp,
--                       const size_t *countp, const ptrdiff_t *stridep,
--                       const ptrdiff_t *imapp, float *ip);
-- int nc_get_varm_double(int ncid, int varid, const size_t *startp,
--                        const size_t *countp, const ptrdiff_t *stridep,
--                        const ptrdiff_t * imapp, double *ip);
-- int nc_get_varm_ushort(int ncid, int varid, const size_t *startp,
--                        const size_t *countp, const ptrdiff_t *stridep,
--                        const ptrdiff_t * imapp, unsigned short *ip);
-- int nc_get_varm_uint(int ncid, int varid, const size_t *startp,
--                      const size_t *countp, const ptrdiff_t *stridep,
--                      const ptrdiff_t * imapp, unsigned int *ip);
-- int nc_get_varm_longlong(int ncid, int varid, const size_t *startp,
--                          const size_t *countp, const ptrdiff_t *stridep,
--                          const ptrdiff_t * imapp, long long *ip);
-- int nc_get_varm_ulonglong(int ncid, int varid, const size_t *startp,
--                           const size_t *countp, const ptrdiff_t *stridep,
--                           const ptrdiff_t * imapp, unsigned long long *ip);
