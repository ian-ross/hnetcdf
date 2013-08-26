-- WRITING AND READING WHOLE VARIABLES

{-# LANGUAGE ForeignFunctionInterface #-}

module Data.NetCDF.Raw.PutGetVar where

import Data.Word
import qualified Data.Vector.Storable as SV


#include <netcdf.h>

nc_put_var :: (Storable a, Storable b) =>
              (CInt -> CInt -> Ptr b -> IO CInt) -> (a -> b)
           -> Int -> Int -> SV.Vector a -> IO Int
nc_put_var cfn conv nc var v = do
  let ncid = fromIntegral nc
      varid = fromIntegral var
      (is, _) = SV.unsafeToForeignPtr0 (SV.map conv v)
  withForeignPtr is $ \isp -> do
    res <- cfn ncid varid isp
    return $ fromIntegral res

-- int nc_put_var_text(int ncid, int varid, const char *op);
nc_put_var_text :: Int -> Int -> SV.Vector Word8 -> IO Int
nc_put_var_text = nc_put_var nc_put_var_text'_ fromIntegral
foreign import ccall safe "Data/NetCDF/Raw.chs.h nc_put_var_text"
  nc_put_var_text'_ :: CInt -> CInt -> Ptr CChar -> IO CInt

-- int nc_put_var_uchar(int ncid, int varid, const unsigned char *op);
nc_put_var_uchar :: Int -> Int -> SV.Vector Word8 -> IO Int
nc_put_var_uchar = nc_put_var nc_put_var_uchar'_ fromIntegral
foreign import ccall safe "Data/NetCDF/Raw.chs.h nc_put_var_uchar"
  nc_put_var_uchar'_ :: CInt -> CInt -> Ptr CUChar -> IO CInt

-- int nc_put_var_schar(int ncid, int varid, const signed char *op);
nc_put_var_schar :: Int -> Int -> SV.Vector Word8 -> IO Int
nc_put_var_schar = nc_put_var nc_put_var_short'_ fromIntegral
foreign import ccall safe "Data/NetCDF/Raw.chs.h nc_put_var_schar"
  nc_put_var_schar'_ :: CInt -> CInt -> Ptr CChar -> IO CInt

-- int nc_put_var_short(int ncid, int varid, const short *op);
nc_put_var_short :: Int -> Int -> SV.Vector Int -> IO Int
nc_put_var_short = nc_put_var nc_put_var_short'_ fromIntegral
foreign import ccall safe "Data/NetCDF/Raw.chs.h nc_put_var_short"
  nc_put_var_short'_ :: CInt -> CInt -> Ptr CShort -> IO CInt

-- int nc_put_var_int(int ncid, int varid, const int *op);
nc_put_var_int :: Int -> Int -> SV.Vector Int -> IO Int
nc_put_var_int = nc_put_var nc_put_var_int'_ fromIntegral
foreign import ccall safe "Data/NetCDF/Raw.chs.h nc_put_var_int"
  nc_put_var_int'_ :: CInt -> CInt -> Ptr CInt -> IO CInt

-- int nc_put_var_long(int ncid, int varid, const long *op);
nc_put_var_long :: Int -> Int -> SV.Vector Int -> IO Int
nc_put_var_long = nc_put_var nc_put_var_long'_ fromIntegral
foreign import ccall safe "Data/NetCDF/Raw.chs.h nc_put_var_long"
  nc_put_var_long'_ :: CInt -> CInt -> Ptr CLong -> IO CInt

-- int nc_put_var_float(int ncid, int varid, const float *op);
nc_put_var_float :: Int -> Int -> SV.Vector Float -> IO Int
nc_put_var_float = nc_put_var nc_put_var_float'_ realToFrac
foreign import ccall safe "Data/NetCDF/Raw.chs.h nc_put_var_float"
  nc_put_var_float'_ :: CInt -> CInt -> Ptr CFloat -> IO CInt

-- int nc_put_var_double(int ncid, int varid, const double *op);
nc_put_var_double :: Int -> Int -> SV.Vector Double -> IO Int
nc_put_var_double = nc_put_var nc_put_var_double'_ realToFrac
foreign import ccall safe "Data/NetCDF/Raw.chs.h nc_put_var_double"
  nc_put_var_double'_ :: CInt -> CInt -> Ptr CDouble -> IO CInt

-- int nc_put_var_ushort(int ncid, int varid, const unsigned short *op);
nc_put_var_ushort :: Int -> Int -> SV.Vector Int -> IO Int
nc_put_var_ushort = nc_put_var nc_put_var_ushort'_ fromIntegral
foreign import ccall safe "Data/NetCDF/Raw.chs.h nc_put_var_ushort"
  nc_put_var_ushort'_ :: CInt -> CInt -> Ptr CUShort -> IO CInt

-- int nc_put_var_uint(int ncid, int varid, const unsigned int *op);
nc_put_var_uint :: Int -> Int -> SV.Vector Int -> IO Int
nc_put_var_uint = nc_put_var nc_put_var_uint'_ fromIntegral
foreign import ccall safe "Data/NetCDF/Raw.chs.h nc_put_var_uint"
  nc_put_var_uint'_ :: CInt -> CInt -> Ptr CUInt -> IO CInt

-- int nc_put_var_longlong(int ncid, int varid, const long long *op);
nc_put_var_longlong :: Int -> Int -> SV.Vector Int -> IO Int
nc_put_var_longlong = nc_put_var nc_put_var_longlong'_ fromIntegral
foreign import ccall safe "Data/NetCDF/Raw.chs.h nc_put_var_longlong"
  nc_put_var_longlong'_ :: CInt -> CInt -> Ptr CLLong -> IO CInt

-- int nc_put_var_ulonglong(int ncid, int varid, const unsigned long long *op);
nc_put_var_ulonglong :: Int -> Int -> SV.Vector Int -> IO Int
nc_put_var_ulonglong = nc_put_var nc_put_var_ulonglong'_ fromIntegral
foreign import ccall safe "Data/NetCDF/Raw.chs.h nc_put_var_ulonglong"
  nc_put_var_ulonglong'_ :: CInt -> CInt -> Ptr CULLong -> IO CInt


nc_get_var :: (Storable a, Storable b) =>
              (CInt -> CInt -> Ptr b -> IO CInt) -> (b -> a)
           -> Int -> Int -> Int -> IO (Int, SV.Vector a)
nc_get_var cfn conv nc var sz = do
  let ncid = fromIntegral nc
      varid = fromIntegral var
  is <- mallocForeignPtrArray sz
  withForeignPtr is $ \isp -> do
    res <- cfn ncid varid isp
    return (fromIntegral res,
            SV.map conv $ SV.unsafeFromForeignPtr0 is sz)

-- int nc_get_var_text(int ncid, int varid, char *ip);
nc_get_var_text :: Int -> Int -> Int -> IO (Int, SV.Vector Word8)
nc_get_var_text = nc_get_var nc_get_var_text'_ fromIntegral
foreign import ccall safe "Data/NetCDF/Raw.chs.h nc_get_var_text"
  nc_get_var_text'_ :: CInt -> CInt -> Ptr CChar -> IO CInt

-- int nc_get_var_uchar(int ncid, int varid, unsigned char *ip);
nc_get_var_uchar :: Int -> Int -> Int -> IO (Int, SV.Vector Word8)
nc_get_var_uchar = nc_get_var nc_get_var_uchar'_ fromIntegral
foreign import ccall safe "Data/NetCDF/Raw.chs.h nc_get_var_uchar"
  nc_get_var_uchar'_ :: CInt -> CInt -> Ptr CUChar -> IO CInt

-- int nc_get_var_schar(int ncid, int varid, signed char *ip);
nc_get_var_schar :: Int -> Int -> Int -> IO (Int, SV.Vector Word8)
nc_get_var_schar = nc_get_var nc_get_var_schar'_ fromIntegral
foreign import ccall safe "Data/NetCDF/Raw.chs.h nc_get_var_schar"
  nc_get_var_schar'_ :: CInt -> CInt -> Ptr CChar -> IO CInt

-- int nc_get_var_short(int ncid, int varid, short *ip);
nc_get_var_short :: Int -> Int -> Int -> IO (Int, SV.Vector Int)
nc_get_var_short = nc_get_var nc_get_var_short'_ fromIntegral
foreign import ccall safe "Data/NetCDF/Raw.chs.h nc_get_var_short"
  nc_get_var_short'_ :: CInt -> CInt -> Ptr CShort -> IO CInt

-- int nc_get_var_int(int ncid, int varid, int *ip);
nc_get_var_int :: Int -> Int -> Int -> IO (Int, SV.Vector Int)
nc_get_var_int = nc_get_var nc_get_var_int'_ fromIntegral
foreign import ccall safe "Data/NetCDF/Raw.chs.h nc_get_var_int"
  nc_get_var_int'_ :: CInt -> CInt -> Ptr CInt -> IO CInt

-- int nc_get_var_long(int ncid, int varid, long *ip);
nc_get_var_long :: Int -> Int -> Int -> IO (Int, SV.Vector Int)
nc_get_var_long = nc_get_var nc_get_var_long'_ fromIntegral
foreign import ccall safe "Data/NetCDF/Raw.chs.h nc_get_var_long"
  nc_get_var_long'_ :: CInt -> CInt -> Ptr CLong -> IO CInt

-- int nc_get_var_float(int ncid, int varid, float *ip);
nc_get_var_float :: Int -> Int -> Int -> IO (Int, SV.Vector Float)
nc_get_var_float = nc_get_var nc_get_var_float'_ realToFrac
foreign import ccall safe "Data/NetCDF/Raw.chs.h nc_get_var_float"
  nc_get_var_float'_ :: CInt -> CInt -> Ptr CFloat -> IO CInt

-- int nc_get_var_double(int ncid, int varid, double *ip);
nc_get_var_double :: Int -> Int -> Int -> IO (Int, SV.Vector Double)
nc_get_var_double = nc_get_var nc_get_var_double'_ realToFrac
foreign import ccall safe "Data/NetCDF/Raw.chs.h nc_get_var_double"
  nc_get_var_double'_ :: CInt -> CInt -> Ptr CDouble -> IO CInt

-- int nc_get_var_ushort(int ncid, int varid, unsigned short *ip);
nc_get_var_ushort :: Int -> Int -> Int -> IO (Int, SV.Vector Int)
nc_get_var_ushort = nc_get_var nc_get_var_ushort'_ fromIntegral
foreign import ccall safe "Data/NetCDF/Raw.chs.h nc_get_var_ushort"
  nc_get_var_ushort'_ :: CInt -> CInt -> Ptr CUShort -> IO CInt

-- int nc_get_var_uint(int ncid, int varid, unsigned int *ip);
nc_get_var_uint :: Int -> Int -> Int -> IO (Int, SV.Vector Int)
nc_get_var_uint = nc_get_var nc_get_var_uint'_ fromIntegral
foreign import ccall safe "Data/NetCDF/Raw.chs.h nc_get_var_uint"
  nc_get_var_uint'_ :: CInt -> CInt -> Ptr CUInt -> IO CInt

-- int nc_get_var_longlong(int ncid, int varid, long long *ip);
nc_get_var_longlong :: Int -> Int -> Int -> IO (Int, SV.Vector Int)
nc_get_var_longlong = nc_get_var nc_get_var_longlong'_ fromIntegral
foreign import ccall safe "Data/NetCDF/Raw.chs.h nc_get_var_longlong"
  nc_get_var_longlong'_ :: CInt -> CInt -> Ptr CLLong -> IO CInt

-- int nc_get_var_ulonglong(int ncid, int varid, unsigned long long *ip);
nc_get_var_ulonglong :: Int -> Int -> Int -> IO (Int, SV.Vector Int)
nc_get_var_ulonglong = nc_get_var nc_get_var_ulonglong'_ fromIntegral
foreign import ccall safe "Data/NetCDF/Raw.chs.h nc_get_var_ulonglong"
  nc_get_var_ulonglong'_ :: CInt -> CInt -> Ptr CULLong -> IO CInt
