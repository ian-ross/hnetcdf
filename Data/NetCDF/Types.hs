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

type NcId = Int

data NcError = NcError String Int String FilePath
             | NcInvalidArgs String
             deriving (Show, Typeable)

instance Exception NcError


ncIOMode :: IOMode -> Int
ncIOMode ReadMode = 0
ncIOMode WriteMode = 1
ncIOMode _ = throw (NcInvalidArgs "IO mode")
