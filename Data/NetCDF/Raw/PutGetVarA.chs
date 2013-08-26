-- WRITING AND READING AN ARRAY

{-# LANGUAGE ForeignFunctionInterface #-}

module Data.NetCDF.Raw.PutGetVarA where

import Data.Word
import qualified Data.Vector.Storable as SV

import Data.NetCDF.Raw.Utils

#include <netcdf.h>

nc_put_vara :: (Storable a, Storable b) =>
               (CInt -> CInt -> Ptr CULong -> Ptr CULong -> Ptr b -> IO CInt)
            -> (a -> b) -> Int -> Int -> [Int] -> [Int] -> SV.Vector a -> IO Int
nc_put_vara cfn conv nc var start count v = do
  let ncid = fromIntegral nc
      varid = fromIntegral var
      (is, _) = SV.unsafeToForeignPtr0 (SV.map conv v)
  withForeignPtr is $ \isp ->
    withSizeArray start $ \startp ->
      withSizeArray count $ \countp -> do
        res <- cfn ncid varid startp countp isp
        return $ fromIntegral res

-- int nc_put_vara_text(int ncid, int varid, const size_t *startp,
--                      const size_t *countp, const char *op);
nc_put_vara_text :: Int -> Int -> [Int] -> [Int] -> SV.Vector Word8 -> IO Int
nc_put_vara_text = nc_put_vara nc_put_vara_text'_ fromIntegral
foreign import ccall safe "Data/NetCDF/Raw.chs.h nc_put_vara_text"
  nc_put_vara_text'_ :: CInt -> CInt -> Ptr CULong -> Ptr CULong
                     -> Ptr CChar -> IO CInt

-- int nc_put_vara_uchar(int ncid, int varid, const size_t *startp,
--                       const size_t *countp, const unsigned char *op);
nc_put_vara_uchar :: Int -> Int -> [Int] -> [Int] -> SV.Vector Word8 -> IO Int
nc_put_vara_uchar = nc_put_vara nc_put_vara_uchar'_ fromIntegral
foreign import ccall safe "Data/NetCDF/Raw.chs.h nc_put_vara_uchar"
  nc_put_vara_uchar'_ :: CInt -> CInt -> Ptr CULong -> Ptr CULong
                      -> Ptr CUChar -> IO CInt

-- int nc_put_vara_schar(int ncid, int varid, const size_t *startp,
--                       const size_t *countp, const signed char *op);
nc_put_vara_schar :: Int -> Int -> [Int] -> [Int] -> SV.Vector Word8 -> IO Int
nc_put_vara_schar = nc_put_vara nc_put_vara_schar'_ fromIntegral
foreign import ccall safe "Data/NetCDF/Raw.chs.h nc_put_vara_schar"
  nc_put_vara_schar'_ :: CInt -> CInt -> Ptr CULong -> Ptr CULong
                     -> Ptr CChar -> IO CInt

-- int nc_put_vara_short(int ncid, int varid, const size_t *startp,
--                       const size_t *countp, const short *op);
nc_put_vara_short :: Int -> Int -> [Int] -> [Int] -> SV.Vector Int -> IO Int
nc_put_vara_short = nc_put_vara nc_put_vara_short'_ fromIntegral
foreign import ccall safe "Data/NetCDF/Raw.chs.h nc_put_vara_short"
  nc_put_vara_short'_ :: CInt -> CInt -> Ptr CULong -> Ptr CULong
                      -> Ptr CShort -> IO CInt

-- int nc_put_vara_int(int ncid, int varid, const size_t *startp,
--                     const size_t *countp, const int *op);
nc_put_vara_int :: Int -> Int -> [Int] -> [Int] -> SV.Vector Int -> IO Int
nc_put_vara_int = nc_put_vara nc_put_vara_int'_ fromIntegral
foreign import ccall safe "Data/NetCDF/Raw.chs.h nc_put_vara_int"
  nc_put_vara_int'_ :: CInt -> CInt -> Ptr CULong -> Ptr CULong
                      -> Ptr CInt -> IO CInt

-- int nc_put_vara_long(int ncid, int varid, const size_t *startp,
--                      const size_t *countp, const long *op);
nc_put_vara_long :: Int -> Int -> [Int] -> [Int] -> SV.Vector Int -> IO Int
nc_put_vara_long = nc_put_vara nc_put_vara_long'_ fromIntegral
foreign import ccall safe "Data/NetCDF/Raw.chs.h nc_put_vara_long"
  nc_put_vara_long'_ :: CInt -> CInt -> Ptr CULong -> Ptr CULong
                      -> Ptr CLong -> IO CInt

-- int nc_put_vara_float(int ncid, int varid, const size_t *startp,
--                       const size_t *countp, const float *op);
nc_put_vara_float :: Int -> Int -> [Int] -> [Int] -> SV.Vector Float -> IO Int
nc_put_vara_float = nc_put_vara nc_put_vara_float'_ realToFrac
foreign import ccall safe "Data/NetCDF/Raw.chs.h nc_put_vara_float"
  nc_put_vara_float'_ :: CInt -> CInt -> Ptr CULong -> Ptr CULong
                      -> Ptr CFloat -> IO CInt

-- int nc_put_vara_double(int ncid, int varid, const size_t *startp,
--                        const size_t *countp, const double *op);
nc_put_vara_double :: Int -> Int -> [Int] -> [Int] -> SV.Vector Double -> IO Int
nc_put_vara_double = nc_put_vara nc_put_vara_double'_ realToFrac
foreign import ccall safe "Data/NetCDF/Raw.chs.h nc_put_vara_double"
  nc_put_vara_double'_ :: CInt -> CInt -> Ptr CULong -> Ptr CULong
                      -> Ptr CDouble -> IO CInt

-- int nc_put_vara_ushort(int ncid, int varid, const size_t *startp,
--                        const size_t *countp, const unsigned short *op);
nc_put_vara_ushort :: Int -> Int -> [Int] -> [Int] -> SV.Vector Int -> IO Int
nc_put_vara_ushort = nc_put_vara nc_put_vara_ushort'_ fromIntegral
foreign import ccall safe "Data/NetCDF/Raw.chs.h nc_put_vara_ushort"
  nc_put_vara_ushort'_ :: CInt -> CInt -> Ptr CULong -> Ptr CULong
                      -> Ptr CUShort -> IO CInt

-- int nc_put_vara_uint(int ncid, int varid, const size_t *startp,
--                      const size_t *countp, const unsigned int *op);
nc_put_vara_uint :: Int -> Int -> [Int] -> [Int] -> SV.Vector Int -> IO Int
nc_put_vara_uint = nc_put_vara nc_put_vara_uint'_ fromIntegral
foreign import ccall safe "Data/NetCDF/Raw.chs.h nc_put_vara_uint"
  nc_put_vara_uint'_ :: CInt -> CInt -> Ptr CULong -> Ptr CULong
                      -> Ptr CUInt -> IO CInt

-- int nc_put_vara_longlong(int ncid, int varid, const size_t *startp,
--                          const size_t *countp, const long long *op);
nc_put_vara_longlong :: Int -> Int -> [Int] -> [Int] -> SV.Vector Int -> IO Int
nc_put_vara_longlong = nc_put_vara nc_put_vara_longlong'_ fromIntegral
foreign import ccall safe "Data/NetCDF/Raw.chs.h nc_put_vara_longlong"
  nc_put_vara_longlong'_ :: CInt -> CInt -> Ptr CULong -> Ptr CULong
                      -> Ptr CLLong -> IO CInt

-- int nc_put_vara_ulonglong(int ncid, int varid, const size_t *startp,
--                           const size_t *countp,
--                           const unsigned long long *op);
nc_put_vara_ulonglong :: Int -> Int -> [Int] -> [Int] -> SV.Vector Int -> IO Int
nc_put_vara_ulonglong = nc_put_vara nc_put_vara_ulonglong'_ fromIntegral
foreign import ccall safe "Data/NetCDF/Raw.chs.h nc_put_vara_ulonglong"
  nc_put_vara_ulonglong'_ :: CInt -> CInt -> Ptr CULong -> Ptr CULong
                      -> Ptr CULLong -> IO CInt


nc_get_vara :: (Storable a, Storable b) =>
               (CInt -> CInt -> Ptr CULong -> Ptr CULong -> Ptr b -> IO CInt)
            -> (b -> a)
            -> Int -> Int -> [Int] -> [Int] -> IO (Int, SV.Vector a)
nc_get_vara cfn conv nc var start count = do
  let ncid = fromIntegral nc
      varid = fromIntegral var
      sz = product count
  is <- mallocForeignPtrArray sz
  withForeignPtr is $ \isp -> do
    withSizeArray start $ \startp ->
      withSizeArray count $ \countp -> do
        res <- cfn ncid varid startp countp isp
        return (fromIntegral res,
                SV.map conv $ SV.unsafeFromForeignPtr0 is sz)

-- int nc_get_vara_text(int ncid, int varid, const size_t *startp,
--                      const size_t *countp, char *ip);
nc_get_vara_text :: Int -> Int -> [Int] -> [Int] -> IO (Int, SV.Vector Word8)
nc_get_vara_text = nc_get_vara nc_get_vara_text'_ fromIntegral
foreign import ccall safe "Data/NetCDF/Raw.chs.h nc_get_vara_text"
  nc_get_vara_text'_ :: CInt -> CInt -> Ptr CULong -> Ptr CULong
                     -> Ptr CChar -> IO CInt

-- int nc_get_vara_uchar(int ncid, int varid, const size_t *startp,
--                       const size_t *countp, unsigned char *ip);
nc_get_vara_uchar :: Int -> Int -> [Int] -> [Int] -> IO (Int, SV.Vector Word8)
nc_get_vara_uchar = nc_get_vara nc_get_vara_uchar'_ fromIntegral
foreign import ccall safe "Data/NetCDF/Raw.chs.h nc_get_vara_uchar"
  nc_get_vara_uchar'_ :: CInt -> CInt -> Ptr CULong -> Ptr CULong
                     -> Ptr CUChar -> IO CInt

-- int nc_get_vara_schar(int ncid, int varid, const size_t *startp,
--                       const size_t *countp, signed char *ip);
nc_get_vara_schar :: Int -> Int -> [Int] -> [Int] -> IO (Int, SV.Vector Word8)
nc_get_vara_schar = nc_get_vara nc_get_vara_schar'_ fromIntegral
foreign import ccall safe "Data/NetCDF/Raw.chs.h nc_get_vara_schar"
  nc_get_vara_schar'_ :: CInt -> CInt -> Ptr CULong -> Ptr CULong
                     -> Ptr CChar -> IO CInt

-- int nc_get_vara_short(int ncid, int varid, const size_t *startp,
--                       const size_t *countp, short *ip);
nc_get_vara_short :: Int -> Int -> [Int] -> [Int] -> IO (Int, SV.Vector Int)
nc_get_vara_short = nc_get_vara nc_get_vara_short'_ fromIntegral
foreign import ccall safe "Data/NetCDF/Raw.chs.h nc_get_vara_short"
  nc_get_vara_short'_ :: CInt -> CInt -> Ptr CULong -> Ptr CULong
                     -> Ptr CShort -> IO CInt

-- int nc_get_vara_int(int ncid, int varid, const size_t *startp,
--                     const size_t *countp, int *ip);
nc_get_vara_int :: Int -> Int -> [Int] -> [Int] -> IO (Int, SV.Vector Int)
nc_get_vara_int = nc_get_vara nc_get_vara_int'_ fromIntegral
foreign import ccall safe "Data/NetCDF/Raw.chs.h nc_get_vara_int"
  nc_get_vara_int'_ :: CInt -> CInt -> Ptr CULong -> Ptr CULong
                     -> Ptr CInt -> IO CInt

-- int nc_get_vara_long(int ncid, int varid,
--                      const size_t *startp, const size_t *countp, long *ip);
nc_get_vara_long :: Int -> Int -> [Int] -> [Int] -> IO (Int, SV.Vector Int)
nc_get_vara_long = nc_get_vara nc_get_vara_long'_ fromIntegral
foreign import ccall safe "Data/NetCDF/Raw.chs.h nc_get_vara_long"
  nc_get_vara_long'_ :: CInt -> CInt -> Ptr CULong -> Ptr CULong
                     -> Ptr CLong -> IO CInt

-- int nc_get_vara_float(int ncid, int varid,
--                       const size_t *startp, const size_t *countp, float *ip);
nc_get_vara_float :: Int -> Int -> [Int] -> [Int] -> IO (Int, SV.Vector Float)
nc_get_vara_float = nc_get_vara nc_get_vara_float'_ realToFrac
foreign import ccall safe "Data/NetCDF/Raw.chs.h nc_get_vara_float"
  nc_get_vara_float'_ :: CInt -> CInt -> Ptr CULong -> Ptr CULong
                     -> Ptr CFloat -> IO CInt

-- int nc_get_vara_double(int ncid, int varid, const size_t *startp,
--                        const size_t *countp, double *ip);
nc_get_vara_double :: Int -> Int -> [Int] -> [Int] -> IO (Int, SV.Vector Double)
nc_get_vara_double = nc_get_vara nc_get_vara_double'_ realToFrac
foreign import ccall safe "Data/NetCDF/Raw.chs.h nc_get_vara_double"
  nc_get_vara_double'_ :: CInt -> CInt -> Ptr CULong -> Ptr CULong
                     -> Ptr CDouble -> IO CInt

-- int nc_get_vara_ushort(int ncid, int varid, const size_t *startp,
--                        const size_t *countp, unsigned short *ip);
nc_get_vara_ushort :: Int -> Int -> [Int] -> [Int] -> IO (Int, SV.Vector Int)
nc_get_vara_ushort = nc_get_vara nc_get_vara_ushort'_ fromIntegral
foreign import ccall safe "Data/NetCDF/Raw.chs.h nc_get_vara_ushort"
  nc_get_vara_ushort'_ :: CInt -> CInt -> Ptr CULong -> Ptr CULong
                     -> Ptr CUShort -> IO CInt

-- int nc_get_vara_uint(int ncid, int varid, const size_t *startp,
--                      const size_t *countp, unsigned int *ip);
nc_get_vara_uint :: Int -> Int -> [Int] -> [Int] -> IO (Int, SV.Vector Int)
nc_get_vara_uint = nc_get_vara nc_get_vara_uint'_ fromIntegral
foreign import ccall safe "Data/NetCDF/Raw.chs.h nc_get_vara_uint"
  nc_get_vara_uint'_ :: CInt -> CInt -> Ptr CULong -> Ptr CULong
                     -> Ptr CUInt -> IO CInt

-- int nc_get_vara_longlong(int ncid, int varid, const size_t *startp,
--                          const size_t *countp, long long *ip);
nc_get_vara_longlong :: Int -> Int -> [Int] -> [Int] -> IO (Int, SV.Vector Int)
nc_get_vara_longlong = nc_get_vara nc_get_vara_longlong'_ fromIntegral
foreign import ccall safe "Data/NetCDF/Raw.chs.h nc_get_vara_longlong"
  nc_get_vara_longlong'_ :: CInt -> CInt -> Ptr CULong -> Ptr CULong
                     -> Ptr CLLong -> IO CInt

-- int nc_get_vara_ulonglong(int ncid, int varid, const size_t *startp,
--                           const size_t *countp, unsigned long long *ip);
nc_get_vara_ulonglong :: Int -> Int -> [Int] -> [Int] -> IO (Int, SV.Vector Int)
nc_get_vara_ulonglong = nc_get_vara nc_get_vara_ulonglong'_ fromIntegral
foreign import ccall safe "Data/NetCDF/Raw.chs.h nc_get_vara_ulonglong"
  nc_get_vara_ulonglong'_ :: CInt -> CInt -> Ptr CULong -> Ptr CULong
                     -> Ptr CULLong -> IO CInt
