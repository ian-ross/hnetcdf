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
  (st1, ncid) <- nc_open p (ncIOMode m)
  checkStatus "openFile" st1 p
  (st2, ndims) <- nc_inq_ndims ncid
  checkStatus "openFile" st2 p
  (st3, unlim) <- nc_inq_unlimdim ncid
  checkStatus "openFile" st3 p
  dims <- forM [0..ndims-1] $ \dimid -> do
    (st4, name, len) <- nc_inq_dim ncid dimid
    checkStatus "openFile" st4 p
    return $ NcDim name len (dimid == unlim)
  return $ NcInfo dims [] ncid

closeFile :: NcInfo -> IO ()
closeFile i = return ()

withFile :: FilePath -> IOMode -> (NcInfo -> IO r) -> IO r
withFile p m = bracket (openFile p m) closeFile



checkStatus :: String -> Int -> FilePath -> IO ()
checkStatus _ 0 _ = return ()
checkStatus fn st p = throw (NcError fn st (nc_strerror st) p)
