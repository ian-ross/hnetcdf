{-# LANGUAGE DeriveDataTypeable #-}

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
            | NcUByte   -- ^ Unsigned 1 byte int
            | NcUShort  -- ^ Unsigned 2-byte int
            | NcUInt    -- ^ Unsigned 4-byte int
            | NcInt64   -- ^ Signed 8-byte int
            | NcUInt64  -- ^ Unsigned 8-byte int
            | NcString  -- ^ String
            deriving (Eq, Show)

instance Enum NcType where
  fromEnum NcByte = 1
  fromEnum NcChar = 2
  fromEnum NcShort = 3
  fromEnum NcInt = 4
  fromEnum NcFloat = 5
  fromEnum NcDouble = 6
  fromEnum NcUByte = 7
  fromEnum NcUShort = 8
  fromEnum NcUInt = 9
  fromEnum NcInt64 = 10
  fromEnum NcUInt64 = 11
  fromEnum NcString = 12
  toEnum n = case n of
    1  -> NcByte
    2  -> NcChar
    3  -> NcShort
    4  -> NcInt
    5  -> NcFloat
    6  -> NcDouble
    7  -> NcUByte
    8  -> NcUShort
    9  -> NcUInt
    10 -> NcInt64
    11 -> NcUInt64
    12 -> NcString
    _ -> throw (NcInvalidType n)

type NcId = Int

data NcError = NcError String Int String FilePath
             | NcInvalidArgs String
             | NcInvalidType Int
             deriving (Show, Typeable)

instance Exception NcError


ncIOMode :: IOMode -> Int
ncIOMode ReadMode = 0
ncIOMode WriteMode = 1
ncIOMode _ = throw (NcInvalidArgs "IO mode")

-- | Fake variable identifier for global attributes.
ncGlobal :: Int
ncGlobal = -1
