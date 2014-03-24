{-# LANGUAGE FlexibleInstances, UndecidableInstances, EmptyDataDecls #-}
-- | NetCDF file metadata handling: when a NetCDF file is opened,
-- metadata defining the dimensions, variables and attributes in the
-- file are read all at once to create a value of type `NcInfo`.

module Data.NetCDF.Metadata
       ( Name
       , NcDim (..)
       , NcAttr (..), ToNcAttr (..), FromNcAttr (..)
       , NcVar (..)
       , NcInfo (..), NcRead, NcWrite
       , ncDim, ncAttr, ncVar, ncVarAttr
       , emptyNcInfo
       , addNcDim, addNcVar, addNcAttr, addNcVarAttr, (#)
       ) where

import Data.NetCDF.Types
import qualified Data.Map as M
import Data.Word
import Foreign.C

-- | Names for dimensions, variables, attributes.
type Name = String

-- | Information about a dimension: name, number of entries and
-- whether unlimited.
data NcDim = NcDim { ncDimName      :: Name
                   , ncDimLength    :: Int
                   , ncDimUnlimited :: Bool
                   } deriving Show

-- | Attribute value.
data NcAttr = NcAttrByte [Word8]
            | NcAttrChar [Char]
            | NcAttrShort [CShort]
            | NcAttrInt [CInt]
            | NcAttrFloat [CFloat]
            | NcAttrDouble [CDouble]
            deriving (Eq, Show)

-- | Conversion from Haskell types to attribute values.
class ToNcAttrHelp a where
  toAttrHelp :: [a] -> NcAttr

instance ToNcAttrHelp Word8 where toAttrHelp = NcAttrByte
instance ToNcAttrHelp Char where toAttrHelp = NcAttrChar
instance ToNcAttrHelp CShort where toAttrHelp = NcAttrShort
instance ToNcAttrHelp CInt where toAttrHelp = NcAttrInt
instance ToNcAttrHelp CFloat where toAttrHelp = NcAttrFloat
instance ToNcAttrHelp CDouble where toAttrHelp = NcAttrDouble

class ToNcAttr a where
  toAttr :: a -> NcAttr

instance ToNcAttrHelp a => ToNcAttr a where
  toAttr x = toAttrHelp [x]
instance ToNcAttrHelp a => ToNcAttr [a] where
  toAttr xs = toAttrHelp xs

-- | Conversion from attribute values to Haskell types.
class FromNcAttr a where
  fromAttr :: NcAttr -> Maybe a

instance FromNcAttr Word8 where
  fromAttr (NcAttrByte [x]) = Just x
  fromAttr _ = Nothing
instance FromNcAttr [Word8] where
  fromAttr (NcAttrByte xs) = Just xs
  fromAttr _ = Nothing
instance FromNcAttr Char where
  fromAttr (NcAttrChar [x]) = Just x
  fromAttr _ = Nothing
instance FromNcAttr [Char] where
  fromAttr (NcAttrChar xs) = Just xs
  fromAttr _ = Nothing
instance FromNcAttr CShort where
  fromAttr (NcAttrShort [x]) = Just x
  fromAttr _ = Nothing
instance FromNcAttr [CShort] where
  fromAttr (NcAttrShort xs) = Just xs
  fromAttr _ = Nothing
instance FromNcAttr CInt where
  fromAttr (NcAttrInt [x]) = Just x
  fromAttr _ = Nothing
instance FromNcAttr [CInt] where
  fromAttr (NcAttrInt xs) = Just xs
  fromAttr _ = Nothing
instance FromNcAttr CFloat where
  fromAttr (NcAttrFloat [x]) = Just x
  fromAttr _ = Nothing
instance FromNcAttr [CFloat] where
  fromAttr (NcAttrFloat xs) = Just xs
  fromAttr _ = Nothing
instance FromNcAttr CDouble where
  fromAttr (NcAttrDouble [x]) = Just x
  fromAttr _ = Nothing
instance FromNcAttr [CDouble] where
  fromAttr (NcAttrDouble xs) = Just xs
  fromAttr _ = Nothing


-- | Information about a variable: name, type, dimensions and
-- attributes.
data NcVar = NcVar { ncVarName  :: Name
                   , ncVarType  :: NcType
                   , ncVarDims  :: [NcDim]
                   , ncVarAttrs :: M.Map Name NcAttr
                   } deriving Show

-- | Type tags for NcInfo values.
data NcRead
data NcWrite

-- | Metadata information for a whole NetCDF file.
data NcInfo a = NcInfo { ncName :: FilePath
                         -- ^ File name.
                       , ncDims :: M.Map Name NcDim
                         -- ^ Dimensions defined in file.
                       , ncVars :: M.Map Name NcVar
                         -- ^ Variables defined in file.
                       , ncAttrs :: M.Map Name NcAttr
                         -- ^ Global attributes defined in file.
                       , ncId :: NcId
                         -- ^ Low-level file access ID.
                       , ncVarIds :: M.Map Name NcId
                         -- ^ Low-level IDs for variables.
                       } deriving Show


-- | Extract dimension metadata by name.
ncDim :: NcInfo a -> Name -> Maybe NcDim
ncDim nc n = M.lookup n $ ncDims nc

-- | Extract a global attribute by name.
ncAttr :: NcInfo a -> Name -> Maybe NcAttr
ncAttr nc n = M.lookup n $ ncAttrs nc

-- | Extract variable metadata by name.
ncVar :: NcInfo a -> Name -> Maybe NcVar
ncVar nc n = M.lookup n $ ncVars nc

-- | Extract an attribute for a given variable by name.
ncVarAttr :: NcVar -> Name -> Maybe NcAttr
ncVarAttr v n = M.lookup n $ ncVarAttrs v


-- | Empty NcInfo value to build on.
emptyNcInfo :: FilePath -> NcInfo NcWrite
emptyNcInfo n = NcInfo n M.empty M.empty M.empty ncInvalidId M.empty

-- | Add a new dimension to an NcInfo value.
addNcDim :: NcDim -> NcInfo NcWrite -> NcInfo NcWrite
addNcDim dim@(NcDim name _ _) (NcInfo n ds vs as fid vids) =
  NcInfo n (M.insert name chkdim ds) vs as fid vids
  where chkdim = dim { ncDimUnlimited =
                          ncDimUnlimited dim &&
                          (not . or . (map ncDimUnlimited) . M.elems $ ds) }

-- | Add a new variable to an NcInfo value.
addNcVar :: NcVar -> NcInfo NcWrite -> NcInfo NcWrite
addNcVar var@(NcVar name _ _ _) (NcInfo n ds vs as fid vids) =
  NcInfo n ds (M.insert name var vs) as fid vids

-- | Add a new global attribute to an NcInfo value.
addNcAttr :: Name -> NcAttr -> NcInfo NcWrite -> NcInfo NcWrite
addNcAttr name att (NcInfo n ds vs as fid vids) =
  NcInfo n ds vs (M.insert name att as) fid vids

-- | Add a new attribute to an NcVar value.
addNcVarAttr :: Name -> NcAttr -> NcVar -> NcVar
addNcVarAttr name att (NcVar n t ds as) = NcVar n t ds (M.insert name att as)

infixl 8 #

-- | Handy postfix function application.
(#) :: a -> (a -> b) -> b
(#) = flip ($)
