{-# LANGUAGE DeriveDataTypeable #-}
-- | Basic utility type definitions for NetCDF bindings.

module Data.NetCDF.Types where

import Control.Exception
import Data.Typeable
import System.IO (IOMode (..))

-- | Representation for NetCDF external data types.
--
data NcType = NcByte    -- ^ Signed 1 byte integer
            | NcChar    -- ^ ISO/ASCII character
            | NcShort   -- ^ Signed 2 byte integer
            | NcInt     -- ^ Signed 4 byte integer
            | NcFloat   -- ^ Single precision floating point
            | NcDouble  -- ^ Double precision floating point
            | NcString  -- ^ Strings
            deriving (Eq, Show)

instance Enum NcType where
  fromEnum NcByte = 1
  fromEnum NcChar = 2
  fromEnum NcShort = 3
  fromEnum NcInt = 4
  fromEnum NcFloat = 5
  fromEnum NcDouble = 6
  fromEnum NcString = 12
  toEnum n = case n of
    1  -> NcByte
    2  -> NcChar
    3  -> NcShort
    4  -> NcInt
    5  -> NcFloat
    6  -> NcDouble
    12  -> NcString
    _ -> throw (NcInvalidType n)

-- | Internal representation of NetCDF IDs.
type NcId = Int

-- | NetCDF error types.
data NcError = NcError String Int String FilePath
             | NcInvalidArgs String
             | NcInvalidType Int
             deriving (Show, Typeable)

instance Exception NcError

-- | Convenience function to convert `IOMode` values to integer values
-- for calls to NetCDF functions.
ncIOMode :: IOMode -> Int
ncIOMode ReadMode = 0
ncIOMode WriteMode = 1
ncIOMode _ = throw (NcInvalidArgs "IO mode")

-- | Fake variable identifier for global attributes.
ncGlobal :: Int
ncGlobal = -1

-- | Fake length value for defining unlimited dimensions.
ncUnlimitedLength :: Int
ncUnlimitedLength = 0

-- | Fake file identifier for unopened files.
ncInvalidId :: NcId
ncInvalidId = -1

-- | 'mode' flags for nc_create (see netcdf.h)
ncClobber, ncNoClobber, nc64bitOffset, ncNetCDF4, ncClassicModel :: Int
ncClobber      = 0
ncNoClobber    = 0x0004
nc64bitOffset  = 0x0200
ncNetCDF4      = 0x1000
ncClassicModel = 0x0100

