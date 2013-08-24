{-# LANGUAGE ExistentialQuantification, StandaloneDeriving #-}

module Data.NetCDF.Metadata
       ( NcDim (..)
       , NcAttr (..)
       , NcVar (..)
       , NcInfo (..)
       , ncDim, ncAttr, ncVar, ncVarAttr
       ) where

import Data.NetCDF.Types
import qualified Data.Map as M


-- | Information about a dimension: name, number of entries and
-- whether unlimited.
data NcDim = NcDim { ncDimName      :: String
                   , ncDimLength    :: Int
                   , ncDimUnlimited :: Bool
                   } deriving Show

-- | Attribute value.
data NcAttr = forall a. Show a => NcAttr { ncAttrName :: String
                                         , ncAttrVals :: [a] }

deriving instance Show NcAttr

-- | Information about a variable: name, type, dimensions and
-- attributes.
data NcVar = NcVar { ncVarName  :: String
                   , ncVarType  :: NcType
                   , ncVarDims  :: [NcDim]
                   , ncVarAttrs :: M.Map String NcAttr
                   } deriving Show

-- | Metadata information for a whole NetCDF file.
data NcInfo = NcInfo { ncName :: FilePath
                       -- ^ File name.
                     ,ncDims :: M.Map String NcDim
                       -- ^ Dimensions defined in file.
                     , ncVars :: M.Map String NcVar
                       -- ^ Variables defined in file.
                     , ncAttrs :: M.Map String NcAttr
                       -- ^ Global attributes defined in file.
                     , ncId :: NcId
                       -- ^ Low-level file access ID.
                     , ncVarIds :: M.Map String NcId
                       -- ^ Low-level IDs for variables.
                     } deriving Show


ncDim :: NcInfo -> String -> Maybe NcDim
ncDim nc n = M.lookup n $ ncDims nc

ncAttr :: NcInfo -> String -> Maybe NcAttr
ncAttr nc n = M.lookup n $ ncAttrs nc

ncVar :: NcInfo -> String -> Maybe NcVar
ncVar nc n = M.lookup n $ ncVars nc

ncVarAttr :: NcVar -> String -> Maybe NcAttr
ncVarAttr v n = M.lookup n $ ncVarAttrs v
