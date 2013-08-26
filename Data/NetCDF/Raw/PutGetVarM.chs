-- WRITING AND READING A MAPPED ARRAY

{-# LANGUAGE ForeignFunctionInterface #-}

module Data.NetCDF.Raw.PutGetVarM where

import Data.Word
import qualified Data.Vector.Storable as SV

import Data.NetCDF.Raw.Utils

#include <netcdf.h>

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
