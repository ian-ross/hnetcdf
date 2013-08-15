{-# LANGUAGE ExistentialQuantification, StandaloneDeriving #-}

module Data.NetCDF.Metadata
       ( NcDim (..)
       , NcAttr (..)
       , NcVar (..)
       , NcInfo (..) ) where

import Data.NetCDF.Types


-- | Information about a dimension: name, number of entries and
-- whether unlimited.
--
data NcDim = NcDim String Int Bool
           deriving Show

data NcAttr = forall a. Show a => NcAttr String [a]

deriving instance Show NcAttr

-- | Information about a variable: name, type, dimensions and
-- attributes.
--
data NcVar = NcVar String NcType [NcDim] [NcAttr]
           deriving Show

-- | Metadata information for a whole NetCDF file.
--
data NcInfo = NcInfo { ncDims :: [NcDim]
                       -- ^ Dimensions defined in file.
                     , ncVars :: [NcVar]
                       -- ^ Variables defined in file.
                     , ncAttrs :: [NcAttr]
                       -- ^ Global attributes defined in file.
                     , ncId :: NcId
                       -- ^ Low-level file access ID.
                     } deriving Show
