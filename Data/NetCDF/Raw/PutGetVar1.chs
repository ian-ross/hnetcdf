-- WRITING AND READING ONE DATUM

{-# LANGUAGE ForeignFunctionInterface #-}

module Data.NetCDF.Raw.PutGetVar1 where

import C2HS

import Data.NetCDF.Raw.Utils

#include <netcdf.h>

-- int nc_put_var1_text(int ncid, int varid, const size_t *indexp,
--                      const char *op);
{#fun nc_put_var1_text { `Int', `Int', withSizeArray* `[Int]',
                         withIntPtrConv* `Word8' } -> `Int' #}

-- int nc_put_var1_uchar(int ncid, int varid, const size_t *indexp,
--                       const unsigned char *op);
{#fun nc_put_var1_uchar { `Int', `Int', withSizeArray* `[Int]',
                          withIntPtrConv* `Word8' } -> `Int' #}

-- int nc_put_var1_schar(int ncid, int varid, const size_t *indexp,
--                       const signed char *op);
{#fun nc_put_var1_schar { `Int', `Int', withSizeArray* `[Int]',
                          withIntPtrConv* `Word8' } -> `Int' #}

-- int nc_put_var1_short(int ncid, int varid, const size_t *indexp,
--                       const short *op);
{#fun nc_put_var1_short { `Int', `Int', withSizeArray* `[Int]',
                          withIntPtrConv* `Int' } -> `Int' #}

-- int nc_put_var1_int(int ncid, int varid, const size_t *indexp,
--                     const int *op);
{#fun nc_put_var1_int { `Int', `Int', withSizeArray* `[Int]',
                        withIntPtrConv* `Int' } -> `Int' #}

-- int nc_put_var1_long(int ncid, int varid, const size_t *indexp,
--                      const long *op);
{#fun nc_put_var1_long { `Int', `Int', withSizeArray* `[Int]',
                         withIntPtrConv* `Int' } -> `Int' #}

-- int nc_put_var1_float(int ncid, int varid, const size_t *indexp,
--                       const float *op);
{#fun nc_put_var1_float { `Int', `Int', withSizeArray* `[Int]',
                          withFloatPtrConv* `Float' } -> `Int' #}

-- int nc_put_var1_double(int ncid, int varid, const size_t *indexp,
--                        const double *op);
{#fun nc_put_var1_double { `Int', `Int', withSizeArray* `[Int]',
                           withFloatPtrConv* `Double' } -> `Int' #}

-- int nc_put_var1_ushort(int ncid, int varid, const size_t *indexp,
--                        const unsigned short *op);
{#fun nc_put_var1_ushort { `Int', `Int', withSizeArray* `[Int]',
                           withIntPtrConv* `Int' } -> `Int' #}

-- int nc_put_var1_uint(int ncid, int varid, const size_t *indexp,
--                      const unsigned int *op);
{#fun nc_put_var1_uint { `Int', `Int', withSizeArray* `[Int]',
                         withIntPtrConv* `Int' } -> `Int' #}

-- int nc_put_var1_longlong(int ncid, int varid, const size_t *indexp,
--                          const long long *op);
{#fun nc_put_var1_longlong { `Int', `Int', withSizeArray* `[Int]',
                             withIntPtrConv* `Int' } -> `Int' #}

-- int nc_put_var1_ulonglong(int ncid, int varid, const size_t *indexp,
--                           const unsigned long long *op);
{#fun nc_put_var1_ulonglong { `Int', `Int', withSizeArray* `[Int]',
                              withIntPtrConv* `Int' } -> `Int' #}


-- int nc_get_var1_text(int ncid, int varid, const size_t *indexp, char *ip);
{#fun nc_get_var1_text { `Int', `Int', withSizeArray* `[Int]',
                         alloca- `Int' peekIntConv* } -> `Int' #}

-- int nc_get_var1_uchar(int ncid, int varid, const size_t *indexp,
--                       unsigned char *ip);
{#fun nc_get_var1_uchar { `Int', `Int', withSizeArray* `[Int]',
                          alloca- `Int' peekIntConv* } -> `Int' #}

-- int nc_get_var1_schar(int ncid, int varid, const size_t *indexp,
--                       signed char *ip);
{#fun nc_get_var1_schar { `Int', `Int', withSizeArray* `[Int]',
                          alloca- `Int' peekIntConv* } -> `Int' #}

-- int nc_get_var1_short(int ncid, int varid, const size_t *indexp,
--                       short *ip);
{#fun nc_get_var1_short { `Int', `Int', withSizeArray* `[Int]',
                          alloca- `Int' peekIntConv* } -> `Int' #}

-- int nc_get_var1_int(int ncid, int varid, const size_t *indexp, int *ip);
{#fun nc_get_var1_int { `Int', `Int', withSizeArray* `[Int]',
                        alloca- `Int' peekIntConv* } -> `Int' #}

-- int nc_get_var1_long(int ncid, int varid, const size_t *indexp, long *ip);
{#fun nc_get_var1_long { `Int', `Int', withSizeArray* `[Int]',
                         alloca- `Int' peekIntConv* } -> `Int' #}

-- int nc_get_var1_float(int ncid, int varid, const size_t *indexp,
--                       float *ip);
{#fun nc_get_var1_float { `Int', `Int', withSizeArray* `[Int]',
                          alloca- `Float' peekFloatConv* } -> `Int' #}

-- int nc_get_var1_double(int ncid, int varid, const size_t *indexp,
--                        double *ip);
{#fun nc_get_var1_double { `Int', `Int', withSizeArray* `[Int]',
                           alloca- `Double' peekFloatConv* } -> `Int' #}

-- int nc_get_var1_ushort(int ncid, int varid, const size_t *indexp,
--                        unsigned short *ip);
{#fun nc_get_var1_ushort { `Int', `Int', withSizeArray* `[Int]',
                           alloca- `Int' peekIntConv* } -> `Int' #}

-- int nc_get_var1_uint(int ncid, int varid, const size_t *indexp,
--                      unsigned int *ip);
{#fun nc_get_var1_uint { `Int', `Int', withSizeArray* `[Int]',
                         alloca- `Int' peekIntConv* } -> `Int' #}

-- int nc_get_var1_longlong(int ncid, int varid, const size_t *indexp,
--                          long long *ip);
{#fun nc_get_var1_longlong { `Int', `Int', withSizeArray* `[Int]',
                             alloca- `Int' peekIntConv* } -> `Int' #}

-- int nc_get_var1_ulonglong(int ncid, int varid, const size_t *indexp,
--                           unsigned long long *ip);
{#fun nc_get_var1_ulonglong { `Int', `Int', withSizeArray* `[Int]',
                              alloca- `Int' peekIntConv* } -> `Int' #}
