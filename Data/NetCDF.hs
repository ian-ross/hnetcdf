{-# LANGUAGE TypeFamilies, FlexibleInstances #-}

module Data.NetCDF
       ( module Data.NetCDF.Raw
       , module Data.NetCDF.Types
       , module Data.NetCDF.Metadata
       , IOMode (..)
       , openFile, closeFile, withFile ) where

import Data.NetCDF.Raw
import Data.NetCDF.Storable
import Data.NetCDF.Types
import Data.NetCDF.Metadata

import Control.Exception (bracket, throw)
import Control.Monad (forM)
import System.FilePath
import System.IO (IOMode (..))

openFile :: FilePath -> IOMode -> IO NcInfo
openFile p m = do
  ncid <- chk "openFile" p $ nc_open p (ncIOMode m)
  ndims <- chk "openFile" p $ nc_inq_ndims ncid
  unlim <- chk "openFile" p $ nc_inq_unlimdim ncid
  dims <- forM [0..ndims-1] $ \dimid -> do
    (name, len) <- chk "openFile" p $ nc_inq_dim ncid dimid
    return $ NcDim name len (dimid == unlim)
  return $ NcInfo dims [] ncid

closeFile :: NcInfo -> IO ()
closeFile i = return ()

withFile :: FilePath -> IOMode -> (NcInfo -> IO r) -> IO r
withFile p m = bracket (openFile p m) closeFile


-- | Utility class to make dealing with status return from foreign
-- NetCDF functions a little easier.
--
class Checkable a where
  type OutType a :: *
  chk :: String -> FilePath -> IO a -> IO (OutType a)

instance Checkable (Int, a) where
  type OutType (Int, a) = a
  chk f p a = do
    (st, v) <- a
    checkStatus f st p
    return v

instance Checkable (Int, a, b) where
  type OutType (Int, a, b) = (a, b)
  chk f p a = do
    (st, v1, v2) <- a
    checkStatus f st p
    return (v1, v2)

instance Checkable (Int, a, b, c) where
  type OutType (Int, a, b, c) = (a, b, c)
  chk f p a = do
    (st, v1, v2, v3) <- a
    checkStatus f st p
    return (v1, v2, v3)

checkStatus :: String -> Int -> FilePath -> IO ()
checkStatus _ 0 _ = return ()
checkStatus fn st p = throw (NcError fn st (nc_strerror st) p)
