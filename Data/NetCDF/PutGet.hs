{-# LANGUAGE ConstraintKinds #-}
-- | Mid-level interface for reading and writing NetCDF data.  The
-- functions in "Data.NetCDF" provide a more convenient interface.

module Data.NetCDF.PutGet
       ( put_var1 , put_var, put_vara, put_vars
       , get_var1 , get_var, get_vara, get_vars
       ) where

import Foreign.C
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc
import Control.Applicative ((<$>))
import Control.Monad (liftM)

import Data.NetCDF.Storable
import Data.NetCDF.Store

put_var1 :: NcStorable a => Int -> Int -> [Int] -> a -> IO Int
put_var1 nc var idxs v = do
  let ncid = fromIntegral nc
      varid = fromIntegral var
  alloca $ \iv -> do
    poke iv v
    withSizeArray idxs $ \idxsp -> do
      res <- ffi_put_var1 ncid varid idxsp iv
      return $ fromIntegral res

get_var1 :: NcStorable a => Int -> Int -> [Int] -> IO (Int, a)
get_var1 nc var idxs = do
  let ncid = fromIntegral nc
      varid = fromIntegral var
  alloca $ \iv -> do
    withSizeArray idxs $ \idxsp -> do
      res <- ffi_get_var1 ncid varid idxsp iv
      v <- peek iv
      return $ (fromIntegral res, v)

put_var :: (NcStorable a, NcStore s, NcStoreExtraCon s a) =>
           Int -> Int -> s a -> IO Int
put_var nc var v = do
  let ncid = fromIntegral nc
      varid = fromIntegral var
      is = toForeignPtr v
  withForeignPtr is $ \isp -> fromIntegral <$> ffi_put_var ncid varid isp

get_var :: (NcStorable a, NcStore s, NcStoreExtraCon s a) =>
           Int -> Int -> [Int] -> IO (Int, s a)
get_var nc var sz = do
  let ncid = fromIntegral nc
      varid = fromIntegral var
  is <- mallocForeignPtrArray (product sz)
  withForeignPtr is $ \isp -> do
    res <- ffi_get_var ncid varid isp
    return (fromIntegral res, fromForeignPtr is sz)

put_vara :: (NcStorable a, NcStore s, NcStoreExtraCon s a) =>
            Int -> Int -> [Int] -> [Int] -> s a -> IO Int
put_vara nc var start count v = do
  let ncid = fromIntegral nc
      varid = fromIntegral var
      is = toForeignPtr v
  withForeignPtr is $ \isp ->
    withSizeArray start $ \startp ->
      withSizeArray count $ \countp -> do
        res <- ffi_put_vara ncid varid startp countp isp
        return $ fromIntegral res

get_vara :: (NcStorable a, NcStore s, NcStoreExtraCon s a)
         => Int -> Int -> [Int] -> [Int] -> IO (Int, s a)
get_vara nc var start count = do
  let ncid = fromIntegral nc
      varid = fromIntegral var
  is <- mallocForeignPtrArray (product count)
  withForeignPtr is $ \isp ->
    withSizeArray start $ \s ->
      withSizeArray count $ \c -> do
        res <- ffi_get_vara ncid varid s c isp
        return (fromIntegral res, fromForeignPtr is (filter (>1) count))

put_vars :: (NcStorable a, NcStore s, NcStoreExtraCon s a) =>
            Int -> Int -> [Int] -> [Int] -> [Int] -> s a -> IO Int
put_vars nc var start count stride v = do
  let ncid = fromIntegral nc
      varid = fromIntegral var
      is = toForeignPtr v
  withForeignPtr is $ \isp ->
    withSizeArray start $ \startp ->
      withSizeArray count $ \countp -> do
        withSizeArray stride $ \stridep -> do
          res <- ffi_put_vars ncid varid startp countp stridep isp
          return $ fromIntegral res

get_vars :: (NcStorable a, NcStore s, NcStoreExtraCon s a)
         => Int -> Int -> [Int] -> [Int] -> [Int] -> IO (Int, s a)
get_vars nc var start count stride = do
  let ncid = fromIntegral nc
      varid = fromIntegral var
  is <- mallocForeignPtrArray (product count)
  withForeignPtr is $ \isp ->
    withSizeArray start $ \s ->
      withSizeArray count $ \c -> do
        withSizeArray stride $ \str -> do
          res <- ffi_get_vars ncid varid s c str isp
          return (fromIntegral res, fromForeignPtr is (filter (>1) count))

-- | Helper function for dealing with size (start, count, stride)
-- arrays.
withSizeArray :: (Integral a) => [a] -> (Ptr CULong -> IO b) -> IO b
withSizeArray = withArray . liftM fromIntegral
