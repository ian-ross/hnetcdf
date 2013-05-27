{-# LANGUAGE ForeignFunctionInterface #-}

module Data.NetCDF.Raw.Base where

import System.IO.Unsafe (unsafePerformIO)
import C2HS

import Data.NetCDF.Raw.Utils

#include <netcdf.h>

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
{#fun nc_create { `String', `Int', alloca- `Int' peekIntConv* } -> `Int' #}

-- int nc__create(const char *path, int cmode, size_t initialsz,
--                size_t *chunksizehintp, int *ncidp);
{#fun nc__create { `String', `Int', `Int',
                   alloca- `Int' peekIntConv*,
                   alloca- `Int' peekIntConv* } -> `Int' #}

-- int nc_open(const char *path, int mode, int *ncidp);
{#fun nc_open { `String', `Int', alloca- `Int' peekIntConv* } -> `Int' #}

-- int nc__open(const char *path, int mode,
--              size_t *chunksizehintp, int *ncidp);
{#fun nc__open { `String', `Int',
                 alloca- `Int' peekIntConv*,
                 alloca- `Int' peekIntConv* } -> `Int' #}

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

-- int nc_inq_path(int ncid, size_t *pathlen, char *path);
-- *** TODO ***

-- int nc_inq(int ncid, int *ndimsp, int *nvarsp, int *nattsp,
--            int *unlimdimidp);
{#fun nc_inq { `Int', alloca- `Int' peekIntConv*, alloca- `Int' peekIntConv*,
               alloca- `Int' peekIntConv*,
               alloca- `Int' peekIntConv* } -> `Int' #}

-- int nc_inq_ndims(int ncid, int *ndimsp);
{#fun nc_inq_ndims { `Int', alloca- `Int' peekIntConv* } -> `Int' #}

-- int nc_inq_nvars(int ncid, int *nvarsp);
{#fun nc_inq_nvars { `Int', alloca- `Int' peekIntConv* } -> `Int' #}

-- int nc_inq_natts(int ncid, int *nattsp);
{#fun nc_inq_natts { `Int', alloca- `Int' peekIntConv* } -> `Int' #}

-- int nc_inq_unlimdim(int ncid, int *unlimdimidp);
{#fun nc_inq_unlimdim { `Int', alloca- `Int' peekIntConv* } -> `Int' #}

-- int nc_inq_format(int ncid, int *formatp);
{#fun nc_inq_format { `Int', alloca- `Int' peekIntConv* } -> `Int' #}

-- int nc_def_dim(int ncid, const char *name, size_t len, int *idp);
{#fun nc_def_dim { `Int', `String', `Int',
                   alloca- `Int' peekIntConv* } -> `Int' #}



-- DIMENSIONS

-- int nc_inq_dimid(int ncid, const char *name, int *idp);
{#fun nc_inq_dimid { `Int', `String', alloca- `Int' peekIntConv* } -> `Int' #}

-- int nc_inq_dim(int ncid, int dimid, char *name, size_t *lenp);
{#fun nc_inq_dim { `Int', `Int', allocaName- `String' peekCString*,
                   alloca- `Int' peekIntConv* } -> `Int' #}

-- int nc_inq_dimname(int ncid, int dimid, char *name);
{#fun nc_inq_dimname { `Int', `Int',
                       allocaName- `String' peekCString* } -> `Int' #}

-- int nc_inq_dimlen(int ncid, int dimid, size_t *lenp);
{#fun nc_inq_dimlen { `Int', `Int', alloca- `Int' peekIntConv* } -> `Int' #}

-- int nc_rename_dim(int ncid, int dimid, const char *name);
{#fun nc_rename_dim { `Int', `Int', `String' } -> `Int' #}

-- int nc_inq_unlimdims(int ncid, int *nunlimdimsp, int *unlimdimidsp);
-- *** TODO ***


-- VARIABLES

-- int nc_def_var(int ncid, const char *name, nc_type xtype, int ndims,
--                const int *dimidsp, int *varidp);
{#fun nc_def_var { `Int', `String', `Int', `Int',
                   withIntArray* `[Int]',
                   alloca- `Int' peekIntConv* } -> `Int' #}

-- int nc_inq_varid(int ncid, const char *name, int *varidp);
{#fun nc_inq_varid { `Int', `String', alloca- `Int' peekIntConv* } -> `Int' #}

-- int nc_inq_var(int ncid, int varid, char *name, nc_type *xtypep,
--                int *ndimsp, int *dimidsp, int *nattsp);
{#fun nc_inq_var { `Int', `Int',
                   allocaName- `String' peekCString*,
                   alloca- `Int' peekIntConv*,
                   alloca- `Int' peekIntConv*,
                   allocaVarDims- `[Int]' peekVarDims*,
                   alloca- `Int' peekIntConv* } -> `Int' #}

-- int nc_inq_varname(int ncid, int varid, char *name);
{#fun nc_inq_varname { `Int', `Int',
                       allocaName- `String' peekCString* } -> `Int' #}

-- int nc_inq_vartype(int ncid, int varid, nc_type *xtypep);
{#fun nc_inq_vartype { `Int', `Int', alloca- `Int' peekIntConv* } -> `Int' #}

-- int nc_inq_varndims(int ncid, int varid, int *ndimsp);
{#fun nc_inq_varndims { `Int', `Int', alloca- `Int' peekIntConv* } -> `Int' #}

-- int nc_inq_vardimid(int ncid, int varid, int *dimidsp);
{#fun nc_inq_vardimid { `Int', `Int',
                        allocaVarDims- `[Int]' peekVarDims* } -> `Int' #}

-- int nc_inq_varnatts(int ncid, int varid, int *nattsp);
{#fun nc_inq_varnatts { `Int', `Int', alloca- `Int' peekIntConv* } -> `Int' #}

-- int nc_rename_var(int ncid, int varid, const char *name);
{#fun nc_rename_var { `Int', `Int', `String' } -> `Int' #}

-- int nc_copy_var(int ncid_in, int varid, int ncid_out);
{#fun nc_copy_var { `Int', `Int', `Int' } -> `Int' #}

-- int nc_set_fill(int ncid, int fillmode, int *old_modep);
{#fun nc_set_fill { `Int', `Int', alloca- `Int' peekIntConv* } -> `Int' #}
