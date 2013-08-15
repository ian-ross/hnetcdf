{-# LANGUAGE TypeFamilies, FlexibleInstances, ScopedTypeVariables #-}

module Data.NetCDF
       ( module Data.NetCDF.Raw
       , module Data.NetCDF.Types
       , module Data.NetCDF.Metadata
       , IOMode (..)
       , openFile, closeFile, withFile ) where

import Data.NetCDF.Raw
import Data.NetCDF.Types
import Data.NetCDF.Metadata
import Data.NetCDF.Utils

import Control.Applicative ((<$>))
import Control.Exception (bracket)
import Control.Monad (forM, void)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Either
import Foreign.C
import System.IO (IOMode (..))

-- | Open a NetCDF file and read all metadata.
openFile :: FilePath -> IOMode -> IO (Either NcError NcInfo)
openFile p m = runEitherT $ runReaderT go ("openFile", p) where
  go :: Access NcInfo
  go = do
    ncid <- chk $ nc_open p (ncIOMode m)
    (ndims, _, nattrs, unlim) <- chk $ nc_inq ncid
    dims <- forM [0..ndims-1] $ \dimid -> do
      (name, len) <- chk $ nc_inq_dim ncid dimid
      return $ NcDim name len (dimid == unlim)
    attrs <- forM [0..nattrs-1] $ \attid -> do
      n <- chk $ nc_inq_attname ncid ncGlobal attid
      (itype, len) <- chk $ nc_inq_att ncid ncGlobal n
      a <- readAttr ncid ncGlobal n (toEnum itype) len
      return a
    return $ NcInfo dims [] attrs ncid

-- | Close a NetCDF file.
closeFile :: NcInfo -> IO ()
closeFile (NcInfo _ _ _ ncid) = void $ nc_close ncid

-- | Bracket file use: a little different from the standard 'bracket'
-- function because of error handling.
withFile :: FilePath -> IOMode
         -> (NcInfo -> IO r) -> (NcError -> IO r) -> IO r
withFile p m ok err = bracket
                      (openFile p m)
                      (\lr -> case lr of
                          Left _ -> return ()
                          Right i -> closeFile i)
                      (\lr -> case lr of
                          Left e -> err e
                          Right i -> ok i)

-- | Read an attribute from a NetCDF variable with error handling.
readAttr :: Int -> Int -> String -> NcType -> Int -> Access NcAttr
readAttr nc var n NcChar l = readAttr' nc var n l nc_get_att_text
readAttr nc var n NcShort l = readAttr' nc var n l nc_get_att_short
readAttr nc var n NcInt l = readAttr' nc var n l nc_get_att_int
readAttr nc var n NcFloat l = readAttr' nc var n l nc_get_att_float
readAttr nc var n NcDouble l = readAttr' nc var n l nc_get_att_double
readAttr nc var n NcUInt l = readAttr' nc var n l nc_get_att_uint
readAttr _ _ n _ _ = return $ NcAttr n ([0] :: [CInt])

-- | Helper function for attribute reading.
readAttr' :: Show a => Int -> Int -> String -> Int
          -> (Int -> Int -> String -> Int -> IO (Int, [a])) -> Access NcAttr
readAttr' nc var n l rf = NcAttr n <$> (chk $ rf nc var n l)
