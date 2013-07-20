{-# LANGUAGE ExistentialQuantification, StandaloneDeriving #-}

module Data.NetCDF.Metadata
       ( NcDim (..)
       , NcAttr (..)
       , NcVar (..)
       , NcInfo (..) ) where

import Data.NetCDF.Types
import Data.NetCDF.Storable


-- | Information about a dimension: name, number of entries and
-- whether unlimited.
--
data NcDim = NcDim String Int Bool
           deriving Show

data NcAttr = forall a. NcStorable a => NcAttr String a


-- | Information about a variable: name, type, dimensions and
-- attributes.
--
data NcVar = NcVar String NcType [NcDim]
           deriving Show

-- | Metadata information for a whole NetCDF file.
--
data NcInfo = NcInfo { ncDims :: [NcDim]
                       -- ^ Dimensions defined in file.
                     , ncVars :: [NcVar]
                       -- ^ Variables defined in file.
--                     , ncAttrs :: [NcAttr]
                       -- ^ Global attributes defined in file.
                     , ncId :: NcId
                       -- ^ Low-level file access ID.
                     } deriving Show
