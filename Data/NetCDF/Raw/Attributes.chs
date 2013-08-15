{-# LANGUAGE ForeignFunctionInterface #-}

module Data.NetCDF.Raw.Attributes where

import Data.Char
import Control.Monad (liftM)
import C2HS

import Data.NetCDF.Raw.Utils

#include <netcdf.h>

nc_put_att :: (Storable a, Storable b) =>
              (CInt -> CInt -> CString -> CInt -> CULong -> Ptr b -> IO CInt)
            -> (a -> b) -> Int -> Int -> String -> Int -> [a] -> IO Int
nc_put_att cfn conv nc var name xtype v = do
  let ncid = fromIntegral nc
      varid = fromIntegral var
      ncxtype = fromIntegral xtype
      ncsize = fromIntegral $ length v
  withCString name $ \namep -> do
    (withArray . liftM conv) v $ \vp -> do
      res <- cfn ncid varid namep ncxtype ncsize vp
      return $ fromIntegral res

-- int nc_put_att_text(int ncid, int varid, const char *name, nc_type xtype,
--                     size_t len, const char *op);
nc_put_att_text :: Int -> Int -> String -> Int -> String -> IO Int
nc_put_att_text = nc_put_att nc_put_att_text'_ convChar
  where convChar c
          | isAscii c = fromIntegral (ord c)
          | otherwise = 32
foreign import ccall safe "Data/NetCDF/Raw.chs.h nc_put_att_text"
  nc_put_att_text'_ :: CInt -> CInt -> CString -> CInt -> CULong
                     -> Ptr CChar -> IO CInt

-- int nc_put_att_uchar(int ncid, int varid, const char *name, nc_type xtype,
--                      size_t len, const unsigned char *op);
nc_put_att_uchar :: Int -> Int -> String -> Int -> [Word8] -> IO Int
nc_put_att_uchar = nc_put_att nc_put_att_uchar'_ fromIntegral
foreign import ccall safe "Data/NetCDF/Raw.chs.h nc_put_att_uchar"
  nc_put_att_uchar'_ :: CInt -> CInt -> CString -> CInt -> CULong
                      -> Ptr CUChar -> IO CInt

-- int nc_put_att_schar(int ncid, int varid, const char *name, nc_type xtype,
--                      size_t len, const signed char *op);
nc_put_att_schar :: Int -> Int -> String -> Int -> [Word8] -> IO Int
nc_put_att_schar = nc_put_att nc_put_att_schar'_ fromIntegral
foreign import ccall safe "Data/NetCDF/Raw.chs.h nc_put_att_schar"
  nc_put_att_schar'_ :: CInt -> CInt -> CString -> CInt -> CULong
                      -> Ptr CChar -> IO CInt

-- int nc_put_att_short(int ncid, int varid, const char *name, nc_type xtype,
--                      size_t len, const short *op);
nc_put_att_short :: Int -> Int -> String -> Int -> [Int] -> IO Int
nc_put_att_short = nc_put_att nc_put_att_short'_ fromIntegral
foreign import ccall safe "Data/NetCDF/Raw.chs.h nc_put_att_short"
  nc_put_att_short'_ :: CInt -> CInt -> CString -> CInt -> CULong
                      -> Ptr CShort -> IO CInt

-- int nc_put_att_int(int ncid, int varid, const char *name, nc_type xtype,
--                    size_t len, const int *op);
nc_put_att_int :: Int -> Int -> String -> Int -> [Int] -> IO Int
nc_put_att_int = nc_put_att nc_put_att_int'_ fromIntegral
foreign import ccall safe "Data/NetCDF/Raw.chs.h nc_put_att_int"
  nc_put_att_int'_ :: CInt -> CInt -> CString -> CInt -> CULong
                      -> Ptr CInt -> IO CInt

-- int nc_put_att_long(int ncid, int varid, const char *name, nc_type xtype,
--                     size_t len, const long *op);
nc_put_att_long :: Int -> Int -> String -> Int -> [Int] -> IO Int
nc_put_att_long = nc_put_att nc_put_att_long'_ fromIntegral
foreign import ccall safe "Data/NetCDF/Raw.chs.h nc_put_att_long"
  nc_put_att_long'_ :: CInt -> CInt -> CString -> CInt -> CULong
                      -> Ptr CLong -> IO CInt

-- int nc_put_att_float(int ncid, int varid, const char *name, nc_type xtype,
--                      size_t len, const float *op);
nc_put_att_float :: Int -> Int -> String -> Int -> [Float] -> IO Int
nc_put_att_float = nc_put_att nc_put_att_float'_ realToFrac
foreign import ccall safe "Data/NetCDF/Raw.chs.h nc_put_att_float"
  nc_put_att_float'_ :: CInt -> CInt -> CString -> CInt -> CULong
                      -> Ptr CFloat -> IO CInt

-- int nc_put_att_double(int ncid, int varid, const char *name, nc_type xtype,
--                       size_t len, const double *op);
nc_put_att_double :: Int -> Int -> String -> Int -> [Double] -> IO Int
nc_put_att_double = nc_put_att nc_put_att_double'_ realToFrac
foreign import ccall safe "Data/NetCDF/Raw.chs.h nc_put_att_double"
  nc_put_att_double'_ :: CInt -> CInt -> CString -> CInt -> CULong
                      -> Ptr CDouble -> IO CInt

-- int nc_put_att_ushort(int ncid, int varid, const char *name, nc_type xtype,
--                       size_t len, const unsigned short *op);
nc_put_att_ushort :: Int -> Int -> String -> Int -> [Int] -> IO Int
nc_put_att_ushort = nc_put_att nc_put_att_ushort'_ fromIntegral
foreign import ccall safe "Data/NetCDF/Raw.chs.h nc_put_att_ushort"
  nc_put_att_ushort'_ :: CInt -> CInt -> CString -> CInt -> CULong
                      -> Ptr CUShort -> IO CInt

-- int nc_put_att_uint(int ncid, int varid, const char *name, nc_type xtype,
--                     size_t len, const unsigned int *op);
nc_put_att_uint :: Int -> Int -> String -> Int -> [Int] -> IO Int
nc_put_att_uint = nc_put_att nc_put_att_uint'_ fromIntegral
foreign import ccall safe "Data/NetCDF/Raw.chs.h nc_put_att_uint"
  nc_put_att_uint'_ :: CInt -> CInt -> CString -> CInt -> CULong
                      -> Ptr CUInt -> IO CInt

-- int nc_put_att_longlong(int ncid, int varid, const char *name, nc_type xtype,
--                         size_t len, const long long *op);
nc_put_att_longlong :: Int -> Int -> String -> Int -> [Int] -> IO Int
nc_put_att_longlong = nc_put_att nc_put_att_longlong'_ fromIntegral
foreign import ccall safe "Data/NetCDF/Raw.chs.h nc_put_att_longlong"
  nc_put_att_longlong'_ :: CInt -> CInt -> CString -> CInt -> CULong
                      -> Ptr CLLong -> IO CInt

-- int nc_put_att_ulonglong(int ncid, int varid, const char *name,
--                          nc_type xtype, size_t len,
--                          const unsigned long long *op);
nc_put_att_ulonglong :: Int -> Int -> String -> Int -> [Int] -> IO Int
nc_put_att_ulonglong = nc_put_att nc_put_att_ulonglong'_ fromIntegral
foreign import ccall safe "Data/NetCDF/Raw.chs.h nc_put_att_ulonglong"
  nc_put_att_ulonglong'_ :: CInt -> CInt -> CString -> CInt -> CULong
                      -> Ptr CULLong -> IO CInt


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


nc_get_att :: (Storable a, Storable b) =>
              (CInt -> CInt -> CString -> Ptr a -> IO CInt)
            -> (a -> b) -> Int -> Int -> String -> Int -> IO (Int, [b])
nc_get_att cfn conv nc var name cnt = do
  let ncid = fromIntegral nc
      varid = fromIntegral var
  withCString name $ \namep -> do
    allocaArray cnt $ \vp -> do
      res <- cfn ncid varid namep vp
      vs <- peekArray cnt vp
      return $ (fromIntegral res, map conv vs)

-- int nc_get_att_text(int ncid, int varid, const char *name, char *ip);
nc_get_att_text :: Int -> Int -> String -> Int -> IO (Int, String)
nc_get_att_text ncid var name ip = do
  (s, str) <- nc_get_att nc_get_att_text'_ (chr . fromIntegral) ncid var name ip
  return (s, takeWhile (/='\NUL') str)
foreign import ccall safe "Data/NetCDF/Raw.chs.h nc_get_att_text"
  nc_get_att_text'_ :: CInt -> CInt -> CString -> Ptr CChar -> IO CInt

-- int nc_get_att_uchar(int ncid, int varid, const char *name,
--                      unsigned char *ip);
nc_get_att_uchar :: Int -> Int -> String -> Int -> IO (Int, [CChar])
nc_get_att_uchar = nc_get_att nc_get_att_uchar'_ fromIntegral
foreign import ccall safe "Data/NetCDF/Raw.chs.h nc_get_att_uchar"
  nc_get_att_uchar'_ :: CInt -> CInt -> CString -> Ptr CUChar -> IO CInt

-- int nc_get_att_schar(int ncid, int varid, const char *name, signed char *ip);
nc_get_att_schar :: Int -> Int -> String -> Int -> IO (Int, [CSChar])
nc_get_att_schar = nc_get_att nc_get_att_schar'_ fromIntegral
foreign import ccall safe "Data/NetCDF/Raw.chs.h nc_get_att_schar"
  nc_get_att_schar'_ :: CInt -> CInt -> CString -> Ptr CChar -> IO CInt

-- int nc_get_att_short(int ncid, int varid, const char *name, short *ip);
nc_get_att_short :: Int -> Int -> String -> Int -> IO (Int, [CShort])
nc_get_att_short = nc_get_att nc_get_att_short'_ fromIntegral
foreign import ccall safe "Data/NetCDF/Raw.chs.h nc_get_att_short"
  nc_get_att_short'_ :: CInt -> CInt -> CString -> Ptr CShort -> IO CInt

-- int nc_get_att_int(int ncid, int varid, const char *name, int *ip);
nc_get_att_int :: Int -> Int -> String -> Int -> IO (Int, [CInt])
nc_get_att_int = nc_get_att nc_get_att_int'_ fromIntegral
foreign import ccall safe "Data/NetCDF/Raw.chs.h nc_get_att_int"
  nc_get_att_int'_ :: CInt -> CInt -> CString -> Ptr CInt -> IO CInt

-- int nc_get_att_long(int ncid, int varid, const char *name, long *ip);
nc_get_att_long :: Int -> Int -> String -> Int -> IO (Int, [CLong])
nc_get_att_long = nc_get_att nc_get_att_long'_ fromIntegral
foreign import ccall safe "Data/NetCDF/Raw.chs.h nc_get_att_long"
  nc_get_att_long'_ :: CInt -> CInt -> CString -> Ptr CLong -> IO CInt

-- int nc_get_att_float(int ncid, int varid, const char *name, float *ip);
nc_get_att_float :: Int -> Int -> String -> Int -> IO (Int, [CFloat])
nc_get_att_float = nc_get_att nc_get_att_float'_ realToFrac
foreign import ccall safe "Data/NetCDF/Raw.chs.h nc_get_att_float"
  nc_get_att_float'_ :: CInt -> CInt -> CString -> Ptr CFloat -> IO CInt

-- int nc_get_att_double(int ncid, int varid, const char *name, double *ip);
nc_get_att_double :: Int -> Int -> String -> Int -> IO (Int, [CDouble])
nc_get_att_double = nc_get_att nc_get_att_double'_ realToFrac
foreign import ccall safe "Data/NetCDF/Raw.chs.h nc_get_att_double"
  nc_get_att_double'_ :: CInt -> CInt -> CString -> Ptr CDouble -> IO CInt

-- int nc_get_att_ushort(int ncid, int varid, const char *name,
--                       unsigned short *ip);
nc_get_att_ushort :: Int -> Int -> String -> Int -> IO (Int, [CUShort])
nc_get_att_ushort = nc_get_att nc_get_att_ushort'_ fromIntegral
foreign import ccall safe "Data/NetCDF/Raw.chs.h nc_get_att_ushort"
  nc_get_att_ushort'_ :: CInt -> CInt -> CString -> Ptr CUShort -> IO CInt

-- int nc_get_att_uint(int ncid, int varid, const char *name, unsigned int *ip);
nc_get_att_uint :: Int -> Int -> String -> Int -> IO (Int, [CUInt])
nc_get_att_uint = nc_get_att nc_get_att_uint'_ fromIntegral
foreign import ccall safe "Data/NetCDF/Raw.chs.h nc_get_att_uint"
  nc_get_att_uint'_ :: CInt -> CInt -> CString -> Ptr CUInt -> IO CInt

-- int nc_get_att_longlong(int ncid, int varid, const char *name,
--                         long long *ip);
nc_get_att_longlong :: Int -> Int -> String -> Int -> IO (Int, [CLLong])
nc_get_att_longlong = nc_get_att nc_get_att_longlong'_ fromIntegral
foreign import ccall safe "Data/NetCDF/Raw.chs.h nc_get_att_longlong"
  nc_get_att_longlong'_ :: CInt -> CInt -> CString -> Ptr CLLong -> IO CInt

-- int nc_get_att_ulonglong(int ncid, int varid, const char *name,
--                          unsigned long long *ip);
nc_get_att_ulonglong :: Int -> Int -> String -> Int -> IO (Int, [CULLong])
nc_get_att_ulonglong = nc_get_att nc_get_att_ulonglong'_ fromIntegral
foreign import ccall safe "Data/NetCDF/Raw.chs.h nc_get_att_ulonglong"
  nc_get_att_ulonglong'_ :: CInt -> CInt -> CString -> Ptr CULLong -> IO CInt
