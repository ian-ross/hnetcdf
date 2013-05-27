{-# LANGUAGE ForeignFunctionInterface #-}

module Data.NetCDF.Raw.Utils where

import Control.Monad (liftM)
import C2HS

ncMaxName, ncMaxDims, ncMaxVars, ncMaxAttrs, ncMaxVarDims :: Int
ncMaxName = 256
ncMaxDims = 1024
ncMaxVars = 8192
ncMaxAttrs = 8192
ncMaxVarDims = 1024


-- Utilities.

withIntArray :: (Storable a, Integral a) => [a] -> (Ptr CInt -> IO b) -> IO b
withIntArray = withArray . liftM fromIntegral
withSizeArray :: (Storable a, Integral a) => [a] -> (Ptr CULong -> IO b) -> IO b
withSizeArray = withArray . liftM fromIntegral

withIntPtrConv :: (Storable a, Storable b, Integral a, Integral b)
                  => a -> (Ptr b -> IO c) -> IO c
withIntPtrConv val f =
  alloca $ \ptr -> do
    poke ptr (fromIntegral val)
    f ptr

withFloatPtrConv :: (Storable a, Storable b, RealFloat a, RealFloat b)
                    => a -> (Ptr b -> IO c) -> IO c
withFloatPtrConv val f =
  alloca $ \ptr -> do
    poke ptr (realToFrac val)
    f ptr

withCStringPtr :: String -> (Ptr CString -> IO a) -> IO a
withCStringPtr val f =
  withCString val $ \inner -> do
    alloca $ \outer -> do
      poke outer inner
      f outer

allocaName :: (Ptr a -> IO b) -> IO b
allocaName = allocaBytes ncMaxName

allocaVarDims :: (Ptr CInt -> IO b) -> IO b
allocaVarDims = allocaArray ncMaxVarDims
peekVarDims :: Ptr CInt -> IO [Int]
peekVarDims = liftM (map fromIntegral) . peekArray ncMaxVarDims
