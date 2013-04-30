{-# LANGUAGE ForeignFunctionInterface #-}

module Data.NetCDF.Raw where

import Foreign hiding (unsafePerformIO)
import Foreign.C
import Control.Monad (liftM)
import System.IO.Unsafe (unsafePerformIO)

#include <netcdf.h>

type CNcType = Int

ncMaxName, ncMaxDims, ncMaxVars, ncMaxAttrs, ncMaxVarDims :: Int
ncMaxName = 256
ncMaxDims = 1024
ncMaxVars = 8192
ncMaxAttrs = 8192
ncMaxVarDims = 1024


-- Utilities.

peekInt :: (Storable a, Integral a, Integral b) => Ptr a -> IO b
peekInt = liftM fromIntegral . peek

withIntArray :: (Storable a, Integral a) => [a] -> (Ptr CInt -> IO b) -> IO b
withIntArray = withArray . liftM fromIntegral

allocaName :: (Ptr a -> IO b) -> IO b
allocaName = allocaBytes ncMaxName

allocaVarDims :: (Ptr CInt -> IO b) -> IO b
allocaVarDims = allocaArray ncMaxVarDims
peekVarDims :: Ptr CInt -> IO [Int]
peekVarDims = liftM (map fromIntegral) . peekArray ncMaxVarDims


-- LIBRARY VERSION

-- const char *nc_inq_libvers(void);
{#fun nc_inq_libvers as nc_inq_libvers' { } -> `String' #}
nc_inq_libvers :: String
nc_inq_libvers = unsafePerformIO nc_inq_libvers'



-- RETURN VALUES

-- const char *nc_strerror(int ncerr);
{#fun nc_strerror as nc_strerror' { `Int' } -> `String' #}
nc_strerror :: Int -> String
nc_strerror = unsafePerformIO . nc_strerror'



-- FILE OPERATIONS

-- int nc_create(const char *path, int cmode, int *ncidp);
{#fun nc_create { `String', `Int', alloca- `Int' peekInt* } -> `Int' #}

-- int nc__create(const char *path, int cmode, size_t initialsz,
--                size_t *chunksizehintp, int *ncidp);
{#fun nc__create { `String', `Int', `Int',
                   alloca- `Int' peekInt*, alloca- `Int' peekInt* } -> `Int' #}

-- int nc_open(const char *path, int mode, int *ncidp);
{#fun nc_open { `String', `Int', alloca- `Int' peekInt* } -> `Int' #}

-- int nc__open(const char *path, int mode,
--              size_t *chunksizehintp, int *ncidp);
{#fun nc__open { `String', `Int',
                 alloca- `Int' peekInt*, alloca- `Int' peekInt* } -> `Int' #}

-- int nc_redef(int ncid);
{#fun nc_redef { `Int' } -> `Int' #}

-- int nc_enddef(int ncid);
{#fun nc_enddef { `Int' } -> `Int' #}

-- int nc__enddef(int ncid, size_t h_minfree, size_t v_align,
--                size_t v_minfree, size_t r_align);
{#fun nc__enddef { `Int', `Int', `Int', `Int', `Int' } -> `Int' #}

-- int nc_sync(int ncid);
{#fun nc_sync { `Int' } -> `Int' #}

-- int nc_close(int ncid);
{#fun nc_close { `Int' } -> `Int' #}

-- int nc_inq(int ncid, int *ndimsp, int *nvarsp, int *nattsp,
--            int *unlimdimidp);
{#fun nc_inq { `Int', alloca- `Int' peekInt*, alloca- `Int' peekInt*,
               alloca- `Int' peekInt*, alloca- `Int' peekInt* } -> `Int' #}

-- int nc_inq_ndims(int ncid, int *ndimsp);
{#fun nc_inq_ndims { `Int', alloca- `Int' peekInt* } -> `Int' #}

-- int nc_inq_nvars(int ncid, int *nvarsp);
{#fun nc_inq_nvars { `Int', alloca- `Int' peekInt* } -> `Int' #}

-- int nc_inq_natts(int ncid, int *nattsp);
{#fun nc_inq_natts { `Int', alloca- `Int' peekInt* } -> `Int' #}

-- int nc_inq_unlimdim(int ncid, int *unlimdimidp);
{#fun nc_inq_unlimdim { `Int', alloca- `Int' peekInt* } -> `Int' #}

-- int nc_inq_format(int ncid, int *formatp);
{#fun nc_inq_format { `Int', alloca- `Int' peekInt* } -> `Int' #}

-- int nc_def_dim(int ncid, const char *name, size_t len, int *idp);
{#fun nc_def_dim { `Int', `String', `Int', alloca- `Int' peekInt* } -> `Int' #}



-- USER DEFINED TYPES

-- int nc_def_compound(int ncid, size_t size,
--                     const char *name, nc_type *typeidp);
{#fun nc_def_compound { `Int', `Int', `String',
                        alloca- `Int' peekInt* } -> `Int' #}

-- int nc_insert_compound(int ncid, nc_type xtype, const char *name,
--                        size_t offset, nc_type field_typeid);
{#fun nc_insert_compound { `Int', `Int', `String', `Int', `Int' } -> `Int' #}

-- int nc_insert_array_compound(int ncid, nc_type xtype, const char *name,
--                              size_t offset, nc_type field_typeid,
--                              int ndims, const int *dim_sizes);

-- int nc_inq_type(int ncid, nc_type xtype, char *name, size_t *size);

-- int nc_inq_compound(int ncid, nc_type xtype, char *name, size_t *sizep,
--                     size_t *nfieldsp);

-- int nc_inq_compound_name(int ncid, nc_type xtype, char *name);

-- int nc_inq_compound_size(int ncid, nc_type xtype, size_t *sizep);

-- int nc_inq_compound_nfields(int ncid, nc_type xtype, size_t *nfieldsp);

-- int nc_inq_compound_fieldname(int ncid, nc_type xtype, int fieldid,
--                               char *name);

-- int nc_inq_compound_fieldindex(int ncid, nc_type xtype, const char *name,
--                                int *fieldidp);

-- int nc_inq_compound_fieldoffset(int ncid, nc_type xtype, int fieldid,
--                                 size_t *offsetp);

-- int nc_inq_compound_fieldtype(int ncid, nc_type xtype, int fieldid,
--                               nc_type *field_typeidp);

-- int nc_inq_compound_fieldndims(int ncid, nc_type xtype, int fieldid,
--                                int *ndimsp);

-- int nc_inq_compound_fielddim_sizes(int ncid, nc_type xtype, int fieldid,
--                                    int *dim_sizes);

-- typedef struct {
--     size_t len; /**< Length of VL data (in base type units) */
--     void *p;    /**< Pointer to VL data */
-- } nc_vlen_t;

-- int nc_def_vlen(int ncid, const char *name,
--                 nc_type base_typeid, nc_type *xtypep);

-- int nc_inq_vlen(int ncid, nc_type xtype, char *name, size_t *datum_sizep,
--                 nc_type *base_nc_typep);

-- int nc_free_vlen(nc_vlen_t *vl);

-- int nc_put_vlen_element(int ncid, int typeid1, void *vlen_element,
--                         size_t len, const void *data);

-- int nc_get_vlen_element(int ncid, int typeid1, const void *vlen_element,
--                         size_t *len, void *data);

-- int nc_free_string(size_t len, char **data);

-- int nc_inq_user_type(int ncid, nc_type xtype, char *name, size_t *size,
--                      nc_type *base_nc_typep, size_t *nfieldsp, int *classp);

-- int nc_def_enum(int ncid, nc_type base_typeid, const char *name,
--                 nc_type *typeidp);

-- int nc_insert_enum(int ncid, nc_type xtype, const char *name,
--                    const void *value);

-- int nc_inq_enum_member(int ncid, nc_type xtype, int idx, char *name,
--                        void *value);

-- int nc_inq_enum_ident(int ncid, nc_type xtype, long long value,
--                       char *identifier);

-- int nc_def_opaque(int ncid, size_t size, const char *name, nc_type *xtypep);

-- int nc_inq_opaque(int ncid, nc_type xtype, char *name, size_t *sizep);



-- GROUPS

-- int nc_inq_grps(int ncid, int *numgrps, int *ncids);

-- int nc_inq_grpname(int ncid, char *name);

-- int nc_inq_grpname_full(int ncid, size_t *lenp, char *full_name);

-- int nc_inq_grpname_len(int ncid, size_t *lenp);

-- int nc_inq_grp_parent(int ncid, int *parent_ncid);

-- int nc_inq_grp_ncid(int ncid, const char *grp_name, int *grp_ncid);

-- int nc_inq_grp_full_ncid(int ncid, const char *full_name, int *grp_ncid);

-- int nc_inq_varids(int ncid, int *nvars, int *varids);

-- int nc_inq_dimids(int ncid, int *ndims, int *dimids, int include_parents);

-- int nc_inq_typeids(int ncid, int *ntypes, int *typeids);

-- int nc_def_grp(int parent_ncid, const char *name, int *new_ncid);



-- DIMENSIONS

-- int nc_inq_dimid(int ncid, const char *name, int *idp);
{#fun nc_inq_dimid { `Int', `String', alloca- `Int' peekInt* } -> `Int' #}

-- int nc_inq_dim(int ncid, int dimid, char *name, size_t *lenp);
{#fun nc_inq_dim { `Int', `Int', allocaName- `String' peekCString*,
                   alloca- `Int' peekInt* } -> `Int' #}

-- int nc_inq_dimname(int ncid, int dimid, char *name);
{#fun nc_inq_dimname { `Int', `Int',
                       allocaName- `String' peekCString* } -> `Int' #}

-- int nc_inq_dimlen(int ncid, int dimid, size_t *lenp);
{#fun nc_inq_dimlen { `Int', `Int', alloca- `Int' peekInt* } -> `Int' #}

-- int nc_rename_dim(int ncid, int dimid, const char *name);
{#fun nc_rename_dim { `Int', `Int', `String' } -> `Int' #}



-- VARIABLES

-- int nc_def_var(int ncid, const char *name, nc_type xtype, int ndims,
--                const int *dimidsp, int *varidp);
{#fun nc_def_var { `Int', `String', `Int', `Int',
                   withIntArray* `[Int]', alloca- `Int' peekInt* } -> `Int' #}

-- int nc_inq_varid(int ncid, const char *name, int *varidp);
{#fun nc_inq_varid { `Int', `String', alloca- `Int' peekInt* } -> `Int' #}

-- int nc_inq_var(int ncid, int varid, char *name, nc_type *xtypep,
--                int *ndimsp, int *dimidsp, int *nattsp);
{#fun nc_inq_var { `Int', `Int',
                   allocaName- `String' peekCString*,
                   alloca- `Int' peekInt*,
                   alloca- `Int' peekInt*,
                   allocaVarDims- `[Int]' peekVarDims*,
                   alloca- `Int' peekInt* } -> `Int' #}

-- int nc_inq_varname(int ncid, int varid, char *name);
{#fun nc_inq_varname { `Int', `Int',
                       allocaName- `String' peekCString* } -> `Int' #}

-- int nc_inq_vartype(int ncid, int varid, nc_type *xtypep);
{#fun nc_inq_vartype { `Int', `Int', alloca- `Int' peekInt* } -> `Int' #}

-- int nc_inq_varndims(int ncid, int varid, int *ndimsp);
{#fun nc_inq_varndims { `Int', `Int', alloca- `Int' peekInt* } -> `Int' #}

-- int nc_inq_vardimid(int ncid, int varid, int *dimidsp);
{#fun nc_inq_vardimid { `Int', `Int',
                        allocaVarDims- `[Int]' peekVarDims* } -> `Int' #}

-- int nc_inq_varnatts(int ncid, int varid, int *nattsp);
{#fun nc_inq_varnatts { `Int', `Int', alloca- `Int' peekInt* } -> `Int' #}

-- int nc_rename_var(int ncid, int varid, const char *name);
{#fun nc_rename_var { `Int', `Int', `String' } -> `Int' #}



-- NETCDF-4 VARIABLES

-- int nc_def_var_deflate(int ncid, int varid, int shuffle, int deflate,
--                        int deflate_level);

-- int nc_inq_var_deflate(int ncid, int varid, int *shufflep,
--                        int *deflatep, int *deflate_levelp);

-- int nc_def_var_fletcher32(int ncid, int varid, int fletcher32);

-- int nc_inq_var_fletcher32(int ncid, int varid, int *fletcher32p);

-- int nc_def_var_chunking(int ncid, int varid, int storage,
--                         const size_t *chunksizesp);

-- int nc_inq_var_chunking(int ncid, int varid, int *storagep,
--                         size_t *chunksizesp);

-- int nc_def_var_fill(int ncid, int varid, int no_fill,
--                     const void *fill_value);

-- int nc_inq_var_fill(int ncid, int varid, int *no_fill, void *fill_valuep);

-- int nc_def_var_endian(int ncid, int varid, int endian);

-- int nc_inq_var_endian(int ncid, int varid, int *endianp);



-- WRITING AND READING WHOLE VARIABLES

-- int nc_put_var_text(int ncid, int varid, const char *op);
-- ????
--{#fun nc_put_var_text { `Int', `Int', `String' } -> `Int' #}
-- int nc_put_var_uchar(int ncid, int varid, const unsigned char *op);
-- int nc_put_var_schar(int ncid, int varid, const signed char *op);
-- int nc_put_var_short(int ncid, int varid, const short *op);
-- int nc_put_var_int(int ncid, int varid, const int *op);
-- int nc_put_var_long(int ncid, int varid, const long *op);
-- int nc_put_var_float(int ncid, int varid, const float *op);
-- int nc_put_var_double(int ncid, int varid, const double *op);
-- int nc_put_var_ushort(int ncid, int varid, const unsigned short *op);
-- int nc_put_var_uint(int ncid, int varid, const unsigned int *op);
-- int nc_put_var_longlong(int ncid, int varid, const long long *op);
-- int nc_put_var_ulonglong(int ncid, int varid, const unsigned long long *op);
-- int nc_put_var_string(int ncid, int varid, const char **op);

-- int nc_get_var_text(int ncid, int varid, char *ip);
-- int nc_get_var_uchar(int ncid, int varid, unsigned char *ip);
-- int nc_get_var_schar(int ncid, int varid, signed char *ip);
-- int nc_get_var_short(int ncid, int varid, short *ip);
-- int nc_get_var_int(int ncid, int varid, int *ip);
-- int nc_get_var_long(int ncid, int varid, long *ip);
-- int nc_get_var_float(int ncid, int varid, float *ip);
-- int nc_get_var_double(int ncid, int varid, double *ip);
-- int nc_get_var_ushort(int ncid, int varid, unsigned short *ip);
-- int nc_get_var_uint(int ncid, int varid, unsigned int *ip);
-- int nc_get_var_longlong(int ncid, int varid, long long *ip);
-- int nc_get_var_ulonglong(int ncid, int varid, unsigned long long *ip);
-- int nc_get_var_string(int ncid, int varid, char **ip);



-- WRITING AND READING ONE DATUM

-- int nc_put_var1_text(int ncid, int varid, const size_t *indexp,
--                      const char *op);
-- int nc_put_var1_uchar(int ncid, int varid, const size_t *indexp,
--                       const unsigned char *op);
-- int nc_put_var1_schar(int ncid, int varid, const size_t *indexp,
--                       const signed char *op);
-- int nc_put_var1_short(int ncid, int varid, const size_t *indexp,
--                       const short *op);
-- int nc_put_var1_int(int ncid, int varid, const size_t *indexp,
--                     const int *op);
-- int nc_put_var1_long(int ncid, int varid, const size_t *indexp,
--                      const long *op);
-- int nc_put_var1_float(int ncid, int varid, const size_t *indexp,
--                       const float *op);
-- int nc_put_var1_double(int ncid, int varid, const size_t *indexp,
--                        const double *op);
-- int nc_put_var1_ushort(int ncid, int varid, const size_t *indexp,
--                        const unsigned short *op);
-- int nc_put_var1_uint(int ncid, int varid, const size_t *indexp,
--                      const unsigned int *op);
-- int nc_put_var1_longlong(int ncid, int varid, const size_t *indexp,
--                          const long long *op);
-- int nc_put_var1_ulonglong(int ncid, int varid, const size_t *indexp,
--                           const unsigned long long *op);
-- int nc_put_var1_string(int ncid, int varid, const size_t *indexp,
--                        const char **op);

-- int nc_get_var1_text(int ncid, int varid, const size_t *indexp, char *ip);
-- int nc_get_var1_uchar(int ncid, int varid, const size_t *indexp,
--                       unsigned char *ip);
-- int nc_get_var1_schar(int ncid, int varid, const size_t *indexp,
--                       signed char *ip);
-- int nc_get_var1_short(int ncid, int varid, const size_t *indexp,
--                       short *ip);
-- int nc_get_var1_int(int ncid, int varid, const size_t *indexp, int *ip);
-- int nc_get_var1_long(int ncid, int varid, const size_t *indexp, long *ip);
-- int nc_get_var1_float(int ncid, int varid, const size_t *indexp,
--                       float *ip);
-- int nc_get_var1_double(int ncid, int varid, const size_t *indexp,
--                        double *ip);
-- int nc_get_var1_ushort(int ncid, int varid, const size_t *indexp,
--                        unsigned short *ip);
-- int nc_get_var1_uint(int ncid, int varid, const size_t *indexp,
--                      unsigned int *ip);
-- int nc_get_var1_longlong(int ncid, int varid, const size_t *indexp,
--                          long long *ip);
-- int nc_get_var1_ulonglong(int ncid, int varid, const size_t *indexp,
--                           unsigned long long *ip);
-- int nc_get_var1_string(int ncid, int varid, const size_t *indexp,
--                        char **ip);


-- WRITING AND READING AN ARRAY

-- int nc_put_vara_text(int ncid, int varid, const size_t *startp,
--                      const size_t *countp, const char *op);
-- int nc_put_vara_uchar(int ncid, int varid, const size_t *startp,
--                       const size_t *countp, const unsigned char *op);
-- int nc_put_vara_schar(int ncid, int varid, const size_t *startp,
--                       const size_t *countp, const signed char *op);
-- int nc_put_vara_short(int ncid, int varid, const size_t *startp,
--                       const size_t *countp, const short *op);
-- int nc_put_vara_int(int ncid, int varid, const size_t *startp,
--                     const size_t *countp, const int *op);
-- int nc_put_vara_long(int ncid, int varid, const size_t *startp,
--                      const size_t *countp, const long *op);
-- int nc_put_vara_float(int ncid, int varid, const size_t *startp,
--                       const size_t *countp, const float *op);
-- int nc_put_vara_double(int ncid, int varid, const size_t *startp,
--                        const size_t *countp, const double *op);
-- int nc_put_vara_ushort(int ncid, int varid, const size_t *startp,
--                        const size_t *countp, const unsigned short *op);
-- int nc_put_vara_uint(int ncid, int varid, const size_t *startp,
--                      const size_t *countp, const unsigned int *op);
-- int nc_put_vara_longlong(int ncid, int varid, const size_t *startp,
--                          const size_t *countp, const long long *op);
-- int nc_put_vara_ulonglong(int ncid, int varid, const size_t *startp,
--                           const size_t *countp,
--                           const unsigned long long *op);
-- int nc_put_vara_string(int ncid, int varid, const size_t *startp,
--                        const size_t *countp, const char **op);

-- int nc_get_vara_text(int ncid, int varid, const size_t *startp,
--                      const size_t *countp, char *ip);
-- int nc_get_vara_uchar(int ncid, int varid, const size_t *startp,
--                       const size_t *countp, unsigned char *ip);
-- int nc_get_vara_schar(int ncid, int varid, const size_t *startp,
--                       const size_t *countp, signed char *ip);
-- int nc_get_vara_short(int ncid, int varid, const size_t *startp,
--                       const size_t *countp, short *ip);
-- int nc_get_vara_int(int ncid, int varid, const size_t *startp,
--                     const size_t *countp, int *ip);
-- int nc_get_vara_long(int ncid, int varid,
--                      const size_t *startp, const size_t *countp, long *ip);
-- int nc_get_vara_float(int ncid, int varid,
--                       const size_t *startp, const size_t *countp, float *ip);
-- int nc_get_vara_double(int ncid, int varid, const size_t *startp,
--                        const size_t *countp, double *ip);
-- int nc_get_vara_ushort(int ncid, int varid, const size_t *startp,
--                        const size_t *countp, unsigned short *ip);
-- int nc_get_vara_uint(int ncid, int varid, const size_t *startp,
--                      const size_t *countp, unsigned int *ip);
-- int nc_get_vara_longlong(int ncid, int varid, const size_t *startp,
--                          const size_t *countp, long long *ip);
-- int nc_get_vara_ulonglong(int ncid, int varid, const size_t *startp,
--                           const size_t *countp, unsigned long long *ip);
-- int nc_get_vara_string(int ncid, int varid, const size_t *startp,
--                        const size_t *countp, char **ip);



-- WRITING AND READING A SLICED ARRAY

-- int nc_put_vars_text(int ncid, int varid, const size_t *startp,
--                      const size_t *countp, const ptrdiff_t *stridep,
--                      const char *op);
-- int nc_put_vars_uchar(int ncid, int varid, const size_t *startp,
--                       const size_t *countp, const ptrdiff_t *stridep,
--                       const unsigned char *op);
-- int nc_put_vars_schar(int ncid, int varid, const size_t *startp,
--                       const size_t *countp, const ptrdiff_t *stridep,
--                       const signed char *op);
-- int nc_put_vars_short(int ncid, int varid, const size_t *startp,
--                       const size_t *countp, const ptrdiff_t *stridep,
--                       const short *op);
-- int nc_put_vars_int(int ncid, int varid, const size_t *startp,
--                     const size_t *countp, const ptrdiff_t *stridep,
--                     const int *op);
-- int nc_put_vars_long(int ncid, int varid, const size_t *startp,
--                      const size_t *countp, const ptrdiff_t *stridep,
--                      const long *op);
-- int nc_put_vars_float(int ncid, int varid, const size_t *startp,
--                       const size_t *countp, const ptrdiff_t *stridep,
--                       const float *op);
-- int nc_put_vars_double(int ncid, int varid, const size_t *startp,
--                        const size_t *countp, const ptrdiff_t *stridep,
--                        const double *op);
-- int nc_put_vars_ushort(int ncid, int varid, const size_t *startp,
--                        const size_t *countp, const ptrdiff_t *stridep,
--                        const unsigned short *op);
-- int nc_put_vars_uint(int ncid, int varid, const size_t *startp,
--                      const size_t *countp, const ptrdiff_t *stridep,
--                      const unsigned int *op);
-- int nc_put_vars_longlong(int ncid, int varid, const size_t *startp,
--                          const size_t *countp, const ptrdiff_t *stridep,
--                          const long long *op);
-- int nc_put_vars_ulonglong(int ncid, int varid, const size_t *startp,
--                           const size_t *countp, const ptrdiff_t *stridep,
--                           const unsigned long long *op);
-- int nc_put_vars_string(int ncid, int varid, const size_t *startp,
--                        const size_t *countp, const ptrdiff_t *stridep,
--                        const char **op);

-- int nc_get_vars_text(int ncid, int varid, const size_t *startp,
--                      const size_t *countp, const ptrdiff_t *stridep,
--                      char *ip);
-- int nc_get_vars_uchar(int ncid, int varid, const size_t *startp,
--                       const size_t *countp, const ptrdiff_t *stridep,
--                       unsigned char *ip);
-- int nc_get_vars_schar(int ncid, int varid, const size_t *startp,
--                       const size_t *countp, const ptrdiff_t *stridep,
--                       signed char *ip);
-- int nc_get_vars_short(int ncid, int varid, const size_t *startp,
--                       const size_t *countp, const ptrdiff_t *stridep,
--                       short *ip);
-- int nc_get_vars_int(int ncid, int varid, const size_t *startp,
--                     const size_t *countp, const ptrdiff_t *stridep,
--                     int *ip);
-- int nc_get_vars_long(int ncid, int varid, const size_t *startp,
--                      const size_t *countp, const ptrdiff_t *stridep,
--                      long *ip);
-- int nc_get_vars_float(int ncid, int varid, const size_t *startp,
--                       const size_t *countp, const ptrdiff_t *stridep,
--                       float *ip);
-- int nc_get_vars_double(int ncid, int varid, const size_t *startp,
--                        const size_t *countp, const ptrdiff_t *stridep,
--                        double *ip);
-- int nc_get_vars_ushort(int ncid, int varid, const size_t *startp,
--                        const size_t *countp, const ptrdiff_t *stridep,
--                        unsigned short *ip);
-- int nc_get_vars_uint(int ncid, int varid, const size_t *startp,
--                      const size_t *countp, const ptrdiff_t *stridep,
--                      unsigned int *ip);
-- int nc_get_vars_longlong(int ncid, int varid, const size_t *startp,
--                          const size_t *countp, const ptrdiff_t *stridep,
--                          long long *ip);
-- int nc_get_vars_ulonglong(int ncid, int varid, const size_t *startp,
--                           const size_t *countp, const ptrdiff_t *stridep,
--                           unsigned long long *ip);
-- int nc_get_vars_string(int ncid, int varid, const size_t *startp,
--                        const size_t *countp, const ptrdiff_t *stridep,
--                        char **ip);



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
-- int nc_put_varm_string(int ncid, int varid, const size_t *startp,
--                        const size_t *countp, const ptrdiff_t *stridep,
--                        const ptrdiff_t * imapp, const char **op);

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
-- int nc_get_varm_string(int ncid, int varid, const size_t *startp,
--                        const size_t *countp, const ptrdiff_t *stridep,
--                        const ptrdiff_t * imapp, char **ip);



-- ATTRIBUTES

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
-- int nc_put_att_string(int ncid, int varid, const char *name,
--                       size_t len, const char **op);

-- int nc_put_att(int ncid, int varid, const char *name, nc_type xtype,
--                size_t len, const void *op);

-- int nc_get_att(int ncid, int varid, const char *name, void *ip);

-- int nc_inq_attname(int ncid, int varid, int attnum, char *name);
{#fun nc_inq_attname { `Int', `Int', `Int',
                       allocaName- `String' peekCString* } -> `Int' #}

-- int nc_inq_att(int ncid, int varid, const char *name,
--                nc_type *xtypep, size_t *lenp);
{#fun nc_inq_att { `Int', `Int', `String',
                   alloca- `Int' peekInt*, alloca- `Int' peekInt* } -> `Int' #}

-- int nc_inq_attid(int ncid, int varid, const char *name, int *idp);
{#fun nc_inq_attid { `Int', `Int', `String',
                     alloca- `Int' peekInt* } -> `Int' #}

-- int nc_inq_atttype(int ncid, int varid, const char *name, nc_type *xtypep);
{#fun nc_inq_atttype { `Int', `Int', `String',
                       alloca- `Int' peekInt* } -> `Int' #}

-- int nc_inq_attlen(int ncid, int varid, const char *name, size_t *lenp);
{#fun nc_inq_attlen { `Int', `Int', `String',
                      alloca- `Int' peekInt* } -> `Int' #}

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
-- int nc_get_att_string(int ncid, int varid, const char *name, char **ip);




-- VARIABLE PREFILLING

-- int nc_set_fill(int ncid, int fillmode, int *old_modep);
{#fun nc_set_fill { `Int', `Int', alloca- `Int' peekInt* } -> `Int' #}



-- UNDOCUMENTED

-- int nc_inq_path(int ncid, size_t *pathlen, char *path);

-- int nc_var_par_access(int ncid, int varid, int par_access);

-- int nc_inq_ncid(int ncid, const char *name, int *grp_ncid);

-- int nc_inq_type_equal(int ncid1, nc_type typeid1, int ncid2,
--                       nc_type typeid2, int *equal);

-- int nc_inq_typeid(int ncid, const char *name, nc_type *typeidp);

-- int nc_inq_compound_field(int ncid, nc_type xtype, int fieldid, char *name,
--                           size_t *offsetp, nc_type *field_typeidp,
--                           int *ndimsp, int *dim_sizesp);

-- int nc_free_vlens(size_t len, nc_vlen_t vlens[]);

-- int nc_inq_enum(int ncid, nc_type xtype, char *name, nc_type *base_nc_typep,
--                 size_t *base_sizep, size_t *num_membersp);

-- int nc_put_var(int ncid, int varid,  const void *op);

-- int nc_get_var(int ncid, int varid,  void *ip);

-- int nc_put_var1(int ncid, int varid,  const size_t *indexp,
--                 const void *op);

-- int nc_get_var1(int ncid, int varid,  const size_t *indexp, void *ip);

-- int nc_put_vara(int ncid, int varid,  const size_t *startp,
--                 const size_t *countp, const void *op);

-- int nc_get_vara(int ncid, int varid,  const size_t *startp,
--                 const size_t *countp, void *ip);

-- int nc_put_vars(int ncid, int varid,  const size_t *startp,
--                 const size_t *countp, const ptrdiff_t *stridep,
--                 const void *op);

-- int nc_get_vars(int ncid, int varid,  const size_t *startp,
--                 const size_t *countp, const ptrdiff_t *stridep,
--                 void *ip);

-- int nc_put_varm(int ncid, int varid,  const size_t *startp,
--                 const size_t *countp, const ptrdiff_t *stridep,
--                 const ptrdiff_t *imapp, const void *op);

-- int nc_get_varm(int ncid, int varid,  const size_t *startp,
--                 const size_t *countp, const ptrdiff_t *stridep,
--                 const ptrdiff_t *imapp, void *ip);

-- int nc_inq_var_szip(int ncid, int varid, int *options_maskp,
--                     int *pixels_per_blockp);

-- int nc_set_default_format(int format, int *old_formatp);

-- int nc_set_chunk_cache(size_t size, size_t nelems, float preemption);

-- int nc_get_chunk_cache(size_t *sizep, size_t *nelemsp, float *preemptionp);

-- int nc_set_var_chunk_cache(int ncid, int varid, size_t size, size_t nelems,
--                            float preemption);

-- int nc_get_var_chunk_cache(int ncid, int varid, size_t *sizep,
--                            size_t *nelemsp, float *preemptionp);

-- int nc_inq_unlimdims(int ncid, int *nunlimdimsp, int *unlimdimidsp);

-- int nc_copy_var(int ncid_in, int varid, int ncid_out);
{#fun nc_copy_var { `Int', `Int', `Int' } -> `Int' #}
