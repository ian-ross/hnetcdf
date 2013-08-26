-- WRITING AND READING A SLICED ARRAY

{-# LANGUAGE ForeignFunctionInterface #-}

module Data.NetCDF.Raw.PutGetVarS where

import Data.Word
import qualified Data.Vector.Storable as SV

import Data.NetCDF.Raw.Utils

#include <netcdf.h>

nc_put_vars :: (Storable a, Storable b) =>
               (CInt -> CInt -> Ptr CULong -> Ptr CULong
                     -> Ptr CULong -> Ptr b -> IO CInt)
            -> (a -> b) -> Int -> Int -> [Int] -> [Int] -> [Int]
            -> SV.Vector a -> IO Int
nc_put_vars cfn conv nc var start count stride v = do
  let ncid = fromIntegral nc
      varid = fromIntegral var
      (is, _) = SV.unsafeToForeignPtr0 (SV.map conv v)
  withForeignPtr is $ \isp ->
    withSizeArray start $ \startp ->
      withSizeArray count $ \countp -> do
        withSizeArray stride $ \stridep -> do
        res <- cfn ncid varid startp countp stridep isp
        return $ fromIntegral res

-- int nc_put_vars_text(int ncid, int varid, const size_t *startp,
--                      const size_t *countp, const ptrdiff_t *stridep,
--                      const char *op);
nc_put_vars_text :: Int -> Int -> [Int] -> [Int] -> [Int]
                 -> SV.Vector Word8 -> IO Int
nc_put_vars_text = nc_put_vars nc_put_vars_text'_ fromIntegral
foreign import ccall safe "Data/NetCDF/Raw.chs.h nc_put_vars_text"
  nc_put_vars_text'_ :: CInt -> CInt -> Ptr CULong -> Ptr CULong -> Ptr CULong
                     -> Ptr CChar -> IO CInt

-- int nc_put_vars_uchar(int ncid, int varid, const size_t *startp,
--                       const size_t *countp, const ptrdiff_t *stridep,
--                       const unsigned char *op);
nc_put_vars_uchar :: Int -> Int -> [Int] -> [Int] -> [Int]
                 -> SV.Vector Word8 -> IO Int
nc_put_vars_uchar = nc_put_vars nc_put_vars_uchar'_ fromIntegral
foreign import ccall safe "Data/NetCDF/Raw.chs.h nc_put_vars_uchar"
  nc_put_vars_uchar'_ :: CInt -> CInt -> Ptr CULong -> Ptr CULong -> Ptr CULong
                     -> Ptr CUChar -> IO CInt

-- int nc_put_vars_schar(int ncid, int varid, const size_t *startp,
--                       const size_t *countp, const ptrdiff_t *stridep,
--                       const signed char *op);
nc_put_vars_schar :: Int -> Int -> [Int] -> [Int] -> [Int]
                 -> SV.Vector Word8 -> IO Int
nc_put_vars_schar = nc_put_vars nc_put_vars_schar'_ fromIntegral
foreign import ccall safe "Data/NetCDF/Raw.chs.h nc_put_vars_schar"
  nc_put_vars_schar'_ :: CInt -> CInt -> Ptr CULong -> Ptr CULong -> Ptr CULong
                     -> Ptr CChar -> IO CInt

-- int nc_put_vars_short(int ncid, int varid, const size_t *startp,
--                       const size_t *countp, const ptrdiff_t *stridep,
--                       const short *op);
nc_put_vars_short :: Int -> Int -> [Int] -> [Int] -> [Int]
                 -> SV.Vector Int -> IO Int
nc_put_vars_short = nc_put_vars nc_put_vars_short'_ fromIntegral
foreign import ccall safe "Data/NetCDF/Raw.chs.h nc_put_vars_short"
  nc_put_vars_short'_ :: CInt -> CInt -> Ptr CULong -> Ptr CULong -> Ptr CULong
                     -> Ptr CShort -> IO CInt

-- int nc_put_vars_int(int ncid, int varid, const size_t *startp,
--                     const size_t *countp, const ptrdiff_t *stridep,
--                     const int *op);
nc_put_vars_int :: Int -> Int -> [Int] -> [Int] -> [Int]
                 -> SV.Vector Int -> IO Int
nc_put_vars_int = nc_put_vars nc_put_vars_int'_ fromIntegral
foreign import ccall safe "Data/NetCDF/Raw.chs.h nc_put_vars_int"
  nc_put_vars_int'_ :: CInt -> CInt -> Ptr CULong -> Ptr CULong -> Ptr CULong
                     -> Ptr CInt -> IO CInt

-- int nc_put_vars_long(int ncid, int varid, const size_t *startp,
--                      const size_t *countp, const ptrdiff_t *stridep,
--                      const long *op);
nc_put_vars_long :: Int -> Int -> [Int] -> [Int] -> [Int]
                 -> SV.Vector Int -> IO Int
nc_put_vars_long = nc_put_vars nc_put_vars_long'_ fromIntegral
foreign import ccall safe "Data/NetCDF/Raw.chs.h nc_put_vars_long"
  nc_put_vars_long'_ :: CInt -> CInt -> Ptr CULong -> Ptr CULong -> Ptr CULong
                     -> Ptr CLong -> IO CInt

-- int nc_put_vars_float(int ncid, int varid, const size_t *startp,
--                       const size_t *countp, const ptrdiff_t *stridep,
--                       const float *op);
nc_put_vars_float :: Int -> Int -> [Int] -> [Int] -> [Int]
                 -> SV.Vector Float -> IO Int
nc_put_vars_float = nc_put_vars nc_put_vars_float'_ realToFrac
foreign import ccall safe "Data/NetCDF/Raw.chs.h nc_put_vars_float"
  nc_put_vars_float'_ :: CInt -> CInt -> Ptr CULong -> Ptr CULong -> Ptr CULong
                     -> Ptr CFloat -> IO CInt

-- int nc_put_vars_double(int ncid, int varid, const size_t *startp,
--                        const size_t *countp, const ptrdiff_t *stridep,
--                        const double *op);
nc_put_vars_double :: Int -> Int -> [Int] -> [Int] -> [Int]
                 -> SV.Vector Double -> IO Int
nc_put_vars_double = nc_put_vars nc_put_vars_double'_ realToFrac
foreign import ccall safe "Data/NetCDF/Raw.chs.h nc_put_vars_double"
  nc_put_vars_double'_ :: CInt -> CInt -> Ptr CULong -> Ptr CULong -> Ptr CULong
                     -> Ptr CDouble -> IO CInt

-- int nc_put_vars_ushort(int ncid, int varid, const size_t *startp,
--                        const size_t *countp, const ptrdiff_t *stridep,
--                        const unsigned short *op);
nc_put_vars_ushort :: Int -> Int -> [Int] -> [Int] -> [Int]
                 -> SV.Vector Int -> IO Int
nc_put_vars_ushort = nc_put_vars nc_put_vars_ushort'_ fromIntegral
foreign import ccall safe "Data/NetCDF/Raw.chs.h nc_put_vars_ushort"
  nc_put_vars_ushort'_ :: CInt -> CInt -> Ptr CULong -> Ptr CULong -> Ptr CULong
                     -> Ptr CUShort -> IO CInt

-- int nc_put_vars_uint(int ncid, int varid, const size_t *startp,
--                      const size_t *countp, const ptrdiff_t *stridep,
--                      const unsigned int *op);
nc_put_vars_uint :: Int -> Int -> [Int] -> [Int] -> [Int]
                 -> SV.Vector Int -> IO Int
nc_put_vars_uint = nc_put_vars nc_put_vars_uint'_ fromIntegral
foreign import ccall safe "Data/NetCDF/Raw.chs.h nc_put_vars_uint"
  nc_put_vars_uint'_ :: CInt -> CInt -> Ptr CULong -> Ptr CULong -> Ptr CULong
                     -> Ptr CUInt -> IO CInt

-- int nc_put_vars_longlong(int ncid, int varid, const size_t *startp,
--                          const size_t *countp, const ptrdiff_t *stridep,
--                          const long long *op);
nc_put_vars_longlong :: Int -> Int -> [Int] -> [Int] -> [Int]
                 -> SV.Vector Int -> IO Int
nc_put_vars_longlong = nc_put_vars nc_put_vars_longlong'_ fromIntegral
foreign import ccall safe "Data/NetCDF/Raw.chs.h nc_put_vars_longlong"
  nc_put_vars_longlong'_ :: CInt -> CInt -> Ptr CULong -> Ptr CULong
                         -> Ptr CULong -> Ptr CLLong -> IO CInt

-- int nc_put_vars_ulonglong(int ncid, int varid, const size_t *startp,
--                           const size_t *countp, const ptrdiff_t *stridep,
--                           const unsigned long long *op);
nc_put_vars_ulonglong :: Int -> Int -> [Int] -> [Int] -> [Int]
                 -> SV.Vector Int -> IO Int
nc_put_vars_ulonglong = nc_put_vars nc_put_vars_ulonglong'_ fromIntegral
foreign import ccall safe "Data/NetCDF/Raw.chs.h nc_put_vars_ulonglong"
  nc_put_vars_ulonglong'_ :: CInt -> CInt -> Ptr CULong -> Ptr CULong
                          -> Ptr CULong -> Ptr CULLong -> IO CInt


nc_get_vars :: (Storable a, Storable b) =>
               (CInt -> CInt -> Ptr CULong -> Ptr CULong
                     -> Ptr CULong -> Ptr b -> IO CInt)
            -> (b -> a)
            -> Int -> Int -> [Int] -> [Int] -> [Int] -> IO (Int, SV.Vector a)
nc_get_vars cfn conv nc var start count stride = do
  let ncid = fromIntegral nc
      varid = fromIntegral var
      sz = product count
  is <- mallocForeignPtrArray sz
  withForeignPtr is $ \isp -> do
    withSizeArray start $ \startp ->
      withSizeArray count $ \countp -> do
        withSizeArray stride $ \stridep -> do
        res <- cfn ncid varid startp countp stridep isp
        return (fromIntegral res,
                SV.map conv $ SV.unsafeFromForeignPtr0 is sz)

-- int nc_get_vars_text(int ncid, int varid, const size_t *startp,
--                      const size_t *countp, const ptrdiff_t *stridep,
--                      char *ip);
nc_get_vars_text :: Int -> Int -> [Int] -> [Int] -> [Int]
                 -> IO (Int, SV.Vector Word8)
nc_get_vars_text = nc_get_vars nc_get_vars_text'_ fromIntegral
foreign import ccall safe "Data/NetCDF/Raw.chs.h nc_get_vars_text"
  nc_get_vars_text'_ :: CInt -> CInt -> Ptr CULong -> Ptr CULong -> Ptr CULong
                     -> Ptr CChar -> IO CInt

-- int nc_get_vars_uchar(int ncid, int varid, const size_t *startp,
--                       const size_t *countp, const ptrdiff_t *stridep,
--                       unsigned char *ip);
nc_get_vars_uchar :: Int -> Int -> [Int] -> [Int] -> [Int]
                 -> IO (Int, SV.Vector Word8)
nc_get_vars_uchar = nc_get_vars nc_get_vars_uchar'_ fromIntegral
foreign import ccall safe "Data/NetCDF/Raw.chs.h nc_get_vars_uchar"
  nc_get_vars_uchar'_ :: CInt -> CInt -> Ptr CULong -> Ptr CULong -> Ptr CULong
                     -> Ptr CUChar -> IO CInt

-- int nc_get_vars_schar(int ncid, int varid, const size_t *startp,
--                       const size_t *countp, const ptrdiff_t *stridep,
--                       signed char *ip);
nc_get_vars_schar :: Int -> Int -> [Int] -> [Int] -> [Int]
                 -> IO (Int, SV.Vector Word8)
nc_get_vars_schar = nc_get_vars nc_get_vars_schar'_ fromIntegral
foreign import ccall safe "Data/NetCDF/Raw.chs.h nc_get_vars_schar"
  nc_get_vars_schar'_ :: CInt -> CInt -> Ptr CULong -> Ptr CULong -> Ptr CULong
                     -> Ptr CChar -> IO CInt

-- int nc_get_vars_short(int ncid, int varid, const size_t *startp,
--                       const size_t *countp, const ptrdiff_t *stridep,
--                       short *ip);
nc_get_vars_short :: Int -> Int -> [Int] -> [Int] -> [Int]
                  -> IO (Int, SV.Vector Int)
nc_get_vars_short = nc_get_vars nc_get_vars_short'_ fromIntegral
foreign import ccall safe "Data/NetCDF/Raw.chs.h nc_get_vars_short"
  nc_get_vars_short'_ :: CInt -> CInt -> Ptr CULong -> Ptr CULong -> Ptr CULong
                     -> Ptr CShort -> IO CInt

-- int nc_get_vars_int(int ncid, int varid, const size_t *startp,
--                     const size_t *countp, const ptrdiff_t *stridep,
--                     int *ip);
nc_get_vars_int :: Int -> Int -> [Int] -> [Int] -> [Int]
                  -> IO (Int, SV.Vector Int)
nc_get_vars_int = nc_get_vars nc_get_vars_int'_ fromIntegral
foreign import ccall safe "Data/NetCDF/Raw.chs.h nc_get_vars_int"
  nc_get_vars_int'_ :: CInt -> CInt -> Ptr CULong -> Ptr CULong -> Ptr CULong
                     -> Ptr CInt -> IO CInt

-- int nc_get_vars_long(int ncid, int varid, const size_t *startp,
--                      const size_t *countp, const ptrdiff_t *stridep,
--                      long *ip);
nc_get_vars_long :: Int -> Int -> [Int] -> [Int] -> [Int]
                  -> IO (Int, SV.Vector Int)
nc_get_vars_long = nc_get_vars nc_get_vars_long'_ fromIntegral
foreign import ccall safe "Data/NetCDF/Raw.chs.h nc_get_vars_long"
  nc_get_vars_long'_ :: CInt -> CInt -> Ptr CULong -> Ptr CULong -> Ptr CULong
                     -> Ptr CLong -> IO CInt

-- int nc_get_vars_float(int ncid, int varid, const size_t *startp,
--                       const size_t *countp, const ptrdiff_t *stridep,
--                       float *ip);
nc_get_vars_float :: Int -> Int -> [Int] -> [Int] -> [Int]
                  -> IO (Int, SV.Vector Float)
nc_get_vars_float = nc_get_vars nc_get_vars_float'_ realToFrac
foreign import ccall safe "Data/NetCDF/Raw.chs.h nc_get_vars_float"
  nc_get_vars_float'_ :: CInt -> CInt -> Ptr CULong -> Ptr CULong -> Ptr CULong
                     -> Ptr CFloat -> IO CInt

-- int nc_get_vars_double(int ncid, int varid, const size_t *startp,
--                        const size_t *countp, const ptrdiff_t *stridep,
--                        double *ip);
nc_get_vars_double :: Int -> Int -> [Int] -> [Int] -> [Int]
                  -> IO (Int, SV.Vector Double)
nc_get_vars_double = nc_get_vars nc_get_vars_double'_ realToFrac
foreign import ccall safe "Data/NetCDF/Raw.chs.h nc_get_vars_double"
  nc_get_vars_double'_ :: CInt -> CInt -> Ptr CULong -> Ptr CULong -> Ptr CULong
                     -> Ptr CDouble -> IO CInt

-- int nc_get_vars_ushort(int ncid, int varid, const size_t *startp,
--                        const size_t *countp, const ptrdiff_t *stridep,
--                        unsigned short *ip);
nc_get_vars_ushort :: Int -> Int -> [Int] -> [Int] -> [Int]
                  -> IO (Int, SV.Vector Int)
nc_get_vars_ushort = nc_get_vars nc_get_vars_ushort'_ fromIntegral
foreign import ccall safe "Data/NetCDF/Raw.chs.h nc_get_vars_ushort"
  nc_get_vars_ushort'_ :: CInt -> CInt -> Ptr CULong -> Ptr CULong -> Ptr CULong
                     -> Ptr CUShort -> IO CInt

-- int nc_get_vars_uint(int ncid, int varid, const size_t *startp,
--                      const size_t *countp, const ptrdiff_t *stridep,
--                      unsigned int *ip);
nc_get_vars_uint :: Int -> Int -> [Int] -> [Int] -> [Int]
                  -> IO (Int, SV.Vector Int)
nc_get_vars_uint = nc_get_vars nc_get_vars_uint'_ fromIntegral
foreign import ccall safe "Data/NetCDF/Raw.chs.h nc_get_vars_uint"
  nc_get_vars_uint'_ :: CInt -> CInt -> Ptr CULong -> Ptr CULong -> Ptr CULong
                     -> Ptr CUInt -> IO CInt

-- int nc_get_vars_longlong(int ncid, int varid, const size_t *startp,
--                          const size_t *countp, const ptrdiff_t *stridep,
--                          long long *ip);
nc_get_vars_longlong :: Int -> Int -> [Int] -> [Int] -> [Int]
                  -> IO (Int, SV.Vector Int)
nc_get_vars_longlong = nc_get_vars nc_get_vars_longlong'_ fromIntegral
foreign import ccall safe "Data/NetCDF/Raw.chs.h nc_get_vars_longlong"
  nc_get_vars_longlong'_ :: CInt -> CInt -> Ptr CULong -> Ptr CULong
                         -> Ptr CULong -> Ptr CLLong -> IO CInt

-- int nc_get_vars_ulonglong(int ncid, int varid, const size_t *startp,
--                           const size_t *countp, const ptrdiff_t *stridep,
--                           unsigned long long *ip);
nc_get_vars_ulonglong :: Int -> Int -> [Int] -> [Int] -> [Int]
                  -> IO (Int, SV.Vector Int)
nc_get_vars_ulonglong = nc_get_vars nc_get_vars_ulonglong'_ fromIntegral
foreign import ccall safe "Data/NetCDF/Raw.chs.h nc_get_vars_ulonglong"
  nc_get_vars_ulonglong'_ :: CInt -> CInt -> Ptr CULong -> Ptr CULong
                          -> Ptr CULong -> Ptr CULLong -> IO CInt

