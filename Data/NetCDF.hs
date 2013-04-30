module Data.NetCDF
       ( module Data.NetCDF.Raw ) where

import Data.NetCDF.Raw

-- | The NetCDF external data types.
--
data NcType = NcNAT        -- ^ Not a type
            | NcByte       -- ^ Signed 1 byte integer
            | NcChar       -- ^ ISO/ASCII character
            | NcShort      -- ^ Signed 2 byte integer
            | NcInt        -- ^ Signed 4 byte integer
            | NcFloat      -- ^ Single precision floating point number
            | NcDouble     -- ^ Double precision floating point number
            | NcUByte      -- ^ Unsigned 1 byte int
            | NcUShort     -- ^ Unsigned 2-byte int
            | NcUInt       -- ^ Unsigned 4-byte int
            | NcInt64      -- ^ Signed 8-byte int
            | NcUInt64     -- ^ Unsigned 8-byte int
            | NcString     -- ^ String
            | NcVlen       -- ^ Vlen types
            | NcOpaque     -- ^ Opaque types
            | NcEnum       -- ^ Enum types
            | NcCompound   -- ^ Compound types
            deriving (Eq, Show, Enum)

newtype NcId = NcId Int
newtype NcError = NcError Int
