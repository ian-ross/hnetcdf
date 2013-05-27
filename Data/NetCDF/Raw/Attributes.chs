{-# LANGUAGE ForeignFunctionInterface #-}

module Data.NetCDF.Raw.Attributes where

import C2HS

import Data.NetCDF.Raw.Utils

#include <netcdf.h>

-- int nc_put_att_text(int ncid, int varid, const char *name,
--                     size_t len, const char *op);
-- int nc_put_att_uchar(int ncid, int varid, const char *name, nc_type xtype,
--                      size_t len, const unsigned char *op);
-- int nc_put_att_schar(int ncid, int varid, const char *name, nc_type xtype,
--                      size_t len, const signed char *op);
-- int nc_put_att_short(int ncid, int varid, const char *name, nc_type xtype,
--                      size_t len, const short *op);
-- int nc_put_att_int(int ncid, int varid, const char *name, nc_type xtype,
--                    size_t len, const int *op);
-- int nc_put_att_long(int ncid, int varid, const char *name, nc_type xtype,
--                     size_t len, const long *op);
-- int nc_put_att_float(int ncid, int varid, const char *name, nc_type xtype,
--                      size_t len, const float *op);
-- int nc_put_att_double(int ncid, int varid, const char *name, nc_type xtype,
--                       size_t len, const double *op);
-- int nc_put_att_ushort(int ncid, int varid, const char *name, nc_type xtype,
--                       size_t len, const unsigned short *op);
-- int nc_put_att_uint(int ncid, int varid, const char *name, nc_type xtype,
--                     size_t len, const unsigned int *op);
-- int nc_put_att_longlong(int ncid, int varid, const char *name, nc_type xtype,
--                         size_t len, const long long *op);
-- int nc_put_att_ulonglong(int ncid, int varid, const char *name,
--                          nc_type xtype, size_t len,
--                          const unsigned long long *op);

-- int nc_inq_attname(int ncid, int varid, int attnum, char *name);
{#fun nc_inq_attname { `Int', `Int', `Int',
                       allocaName- `String' peekCString* } -> `Int' #}

-- int nc_inq_att(int ncid, int varid, const char *name,
--                nc_type *xtypep, size_t *lenp);
{#fun nc_inq_att { `Int', `Int', `String', alloca- `Int' peekIntConv*,
                   alloca- `Int' peekIntConv* } -> `Int' #}

-- int nc_inq_attid(int ncid, int varid, const char *name, int *idp);
{#fun nc_inq_attid { `Int', `Int', `String',
                     alloca- `Int' peekIntConv* } -> `Int' #}

-- int nc_inq_atttype(int ncid, int varid, const char *name, nc_type *xtypep);
{#fun nc_inq_atttype { `Int', `Int', `String',
                       alloca- `Int' peekIntConv* } -> `Int' #}

-- int nc_inq_attlen(int ncid, int varid, const char *name, size_t *lenp);
{#fun nc_inq_attlen { `Int', `Int', `String',
                      alloca- `Int' peekIntConv* } -> `Int' #}

-- int nc_copy_att(int ncid_in, int varid_in, const char *name,
--                 int ncid_out, int varid_out);
{#fun nc_copy_att { `Int', `Int', `String', `Int', `Int' } -> `Int' #}

-- int nc_rename_att(int ncid, int varid, const char *name,
--                   const char *newname);
{#fun nc_rename_att { `Int', `Int', `String', `String' } -> `Int' #}

-- int nc_del_att(int ncid, int varid, const char *name);
{#fun nc_del_att { `Int', `Int', `String' } -> `Int' #}

-- int nc_get_att_text(int ncid, int varid, const char *name, char *ip);
-- int nc_get_att_uchar(int ncid, int varid, const char *name,
--                      unsigned char *ip);
-- int nc_get_att_schar(int ncid, int varid, const char *name, signed char *ip);
-- int nc_get_att_short(int ncid, int varid, const char *name, short *ip);
-- int nc_get_att_int(int ncid, int varid, const char *name, int *ip);
-- int nc_get_att_long(int ncid, int varid, const char *name, long *ip);
-- int nc_get_att_float(int ncid, int varid, const char *name, float *ip);
-- int nc_get_att_double(int ncid, int varid, const char *name, double *ip);
-- int nc_get_att_ushort(int ncid, int varid, const char *name,
--                       unsigned short *ip);
-- int nc_get_att_uint(int ncid, int varid, const char *name, unsigned int *ip);
-- int nc_get_att_longlong(int ncid, int varid, const char *name,
--                         long long *ip);
-- int nc_get_att_ulonglong(int ncid, int varid, const char *name,
--                          unsigned long long *ip);
