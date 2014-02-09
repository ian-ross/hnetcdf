{-# LANGUAGE DeriveDataTypeable #-}
-- | Basic utility type definitions for NetCDF bindings.

module Data.NetCDF.Types where

import Control.Exception
import Data.Typeable
import System.FilePath
import System.IO (IOMode (..))

-- | Representation for NetCDF external data types.
--
data NcType = NcByte    -- ^ Signed 1 byte integer
            | NcChar    -- ^ ISO/ASCII character
            | NcShort   -- ^ Signed 2 byte integer
            | NcInt     -- ^ Signed 4 byte integer
            | NcFloat   -- ^ Single precision floating point
            | NcDouble  -- ^ Double precision floating point
            deriving (Eq, Show)

instance Enum NcType where
  fromEnum NcByte = 1
  fromEnum NcChar = 2
  fromEnum NcShort = 3
  fromEnum NcInt = 4
  fromEnum NcFloat = 5
  fromEnum NcDouble = 6
  toEnum n = case n of
    1  -> NcByte
    2  -> NcChar
    3  -> NcShort
    4  -> NcInt
    5  -> NcFloat
    6  -> NcDouble
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
