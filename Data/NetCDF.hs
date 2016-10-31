{-# LANGUAGE ScopedTypeVariables, ConstraintKinds #-}
-- | Bindings to the Unidata NetCDF data access library.
--
--   As well as conventional low-level FFI bindings to the functions
--   in the NetCDF library (in the "Data.NetCDF.Raw" modules),
--   @hnetcdf@ provides a higher-level Haskell interface (currently
--   only for reading data).  This higher-level interface aims to
--   provide a "container polymorphic" view of NetCDF data allowing
--   NetCDF variables to be read into @Storable@ @Vectors@ and Repa
--   arrays easily.
--
--   For example:
--
-- > import Data.NetCDF
-- > import Foreign.C
-- > import qualified Data.Vector.Storable as SV
-- > ...
-- > type SVRet = IO (Either NcError (SV.Vector a))
-- > ...
-- >   enc <- openFile "tst.nc"
-- >   case enc of
-- >     Right nc -> do
-- >       eval <- get nc "varname" :: SVRet CDouble
-- >       ...
--
--   gets the full contents of a NetCDF variable as a @Storable@
--   @Vector@, while the following code reads the same variable
--   (assumed to be three-dimensional) into a Repa array:
--
-- > import Data.NetCDF
-- > import Foreign.C
-- > import qualified Data.Array.Repa as R
-- > import qualified Data.Array.Repa.Eval as RE
-- > import Data.Array.Repa.Repr.ForeignPtr (F)
-- > ...
-- > type RepaRet3 a = IO (Either NcError (R.Array F R.DIM3 a))
-- > ...
-- >   enc <- openFile "tst.nc"
-- >   case enc of
-- >     Right nc -> do
-- >       eval <- get nc "varname" :: RepaRet3 CDouble
-- >       ...

module Data.NetCDF
       ( module Data.NetCDF.Types
       , module Data.NetCDF.Metadata
       , NcStorable (..)
       , IOMode (..)
       , openFile, createFile, syncFile, closeFile
       , withReadFile, withCreateFile
       , get1, get, getA, getS
       , put1, put, putA, putS
       , coardsScale ) where

import Data.NetCDF.Raw
import Data.NetCDF.Types
import Data.NetCDF.Metadata
import Data.NetCDF.PutGet
import Data.NetCDF.Storable
import Data.NetCDF.Store
import Data.NetCDF.Utils

import Control.Exception (bracket)
import Control.Monad (forM, forM_, void)
import Control.Error
import qualified Data.Map as M
import Foreign.C
import System.IO (IOMode (..))

-- | Open an existing NetCDF file for read-only access and read all
-- metadata: the returned 'NcInfo' value contains all the information
-- about dimensions, variables and attributes in the file.
openFile :: FilePath -> NcIO (NcInfo NcRead)
openFile p = runAccess "openFile" p $ do
  ncid <- chk $ nc_open p (ncIOMode ReadMode)
  (ndims, nvars, nattrs, unlim) <- chk $ nc_inq ncid
  dims <- forM [0..ndims-1] (read1Dim ncid unlim)
  attrs <- forM [0..nattrs-1] (read1Attr ncid ncGlobal)
  vars <- forM [0..nvars-1] (read1Var ncid dims)
  let mkMap nf = foldl (\m v -> M.insert (nf v) v m) M.empty
      dimmap = mkMap ncDimName dims
      attmap = M.fromList attrs
      varmap = mkMap ncVarName vars
      varidmap = M.fromList $ zip (map ncVarName vars) [0..]
  return $ NcInfo p dimmap varmap attmap ncid varidmap

-- | Create a new NetCDF file, ready for write-only access.  The
-- 'NcInfo' parameter contains all the information about dimensions,
-- variables and attributes in the file.
createFile :: NcInfo NcWrite -> NcIO (NcInfo NcWrite)
createFile (NcInfo n ds vs as _ _) = runAccess "createFile" n $ do
  ncid <- chk $ nc_create n ncClobber
  newds <- forM (M.toList ds) (write1Dim ncid . snd)
  let dimids = M.fromList $ zip (M.keys ds) newds
  forM_ (M.toList as) (write1Attr ncid ncGlobal)
  newvs <- forM (M.toList vs) (write1Var ncid dimids . snd)
  let varids = M.fromList $ zip (M.keys vs) newvs
  chk $ nc_enddef ncid
  return $ NcInfo n ds vs as ncid varids

-- | Sync a NetCDF file.
syncFile :: NcInfo NcWrite -> IO ()
syncFile (NcInfo _ _ _ _ ncid _) = void $ nc_sync ncid

-- | Close a NetCDF file.
closeFile :: NcInfo a -> IO ()
closeFile (NcInfo _ _ _ _ ncid _) = void $ nc_close ncid

-- | Bracket read-only file use: a little different from the standard
-- 'withFile' function because of error handling.
withReadFile :: FilePath
             -> (NcInfo NcRead -> IO r) -> (NcError -> IO r) -> IO r
withReadFile p ok e = bracket
                      (openFile p)
                      (either (const $ return ()) closeFile)
                      (either e ok)

-- | Bracket write-only file use: a little different from the standard
-- 'withFile' function because of error handling.
withCreateFile :: NcInfo NcWrite
               -> (NcInfo NcWrite -> IO r) -> (NcError -> IO r) -> IO r
withCreateFile nc ok e = bracket
                         (createFile nc)
                         (either (const $ return ()) closeFile)
                         (either e ok)

-- | Read a single value from an open NetCDF file.
get1 :: NcStorable a => NcInfo NcRead -> NcVar -> [Int] -> NcIO a
get1 nc var idxs = runAccess "get1" (ncName nc) $
  chk $ get_var1 (ncId nc) ((ncVarIds nc) M.! (ncVarName var)) idxs

-- | Read a whole variable from an open NetCDF file.
get :: (NcStorable a, NcStore s, NcStoreExtraCon s a) =>
       NcInfo NcRead -> NcVar -> NcIO (s a)
get nc var = runAccess "get" (ncName nc) $ do
  let ncid = ncId nc
      varid = (ncVarIds nc) M.! (ncVarName var)
      sz = map ncDimLength $ ncVarDims var
  chk $ get_var ncid varid sz

-- | Read a slice of a variable from an open NetCDF file.
getA :: (NcStorable a, NcStore s, NcStoreExtraCon s a)
     => NcInfo NcRead -> NcVar -> [Int] -> [Int] -> NcIO (s a)
getA nc var start count = runAccess "getA" (ncName nc) $ do
  let ncid = ncId nc
      varid = (ncVarIds nc) M.! (ncVarName var)
  chk $ get_vara ncid varid start count

-- | Read a strided slice of a variable from an open NetCDF file.
getS :: (NcStorable a, NcStore s, NcStoreExtraCon s a)
     => NcInfo NcRead -> NcVar -> [Int] -> [Int] -> [Int] -> NcIO (s a)
getS nc var start count stride = runAccess "getS" (ncName nc) $ do
  let ncid = ncId nc
      varid = (ncVarIds nc) M.! (ncVarName var)
  chk $ get_vars ncid varid start count stride


-- | Write a single value to an open NetCDF file.
put1 :: NcStorable a => NcInfo NcWrite -> NcVar -> [Int] -> a -> NcIO ()
put1 nc var idxs val = runAccess "put1" (ncName nc) $
  chk $ put_var1 (ncId nc) ((ncVarIds nc) M.! (ncVarName var)) idxs val

-- | Write a whole variable to an open NetCDF file.
put :: (NcStorable a, NcStore s, NcStoreExtraCon s a) =>
       NcInfo NcWrite -> NcVar -> s a -> NcIO ()
put nc var val = runAccess "put" (ncName nc) $ do
  let ncid = ncId nc
      varid = (ncVarIds nc) M.! (ncVarName var)
  chk $ put_var ncid varid val

-- | Write a slice of a variable to an open NetCDF file.
putA :: (NcStorable a, NcStore s, NcStoreExtraCon s a)
     => NcInfo NcWrite -> NcVar -> [Int] -> [Int] -> s a -> NcIO ()
putA nc var start count val = runAccess "putA" (ncName nc) $ do
  let ncid = ncId nc
      varid = (ncVarIds nc) M.! (ncVarName var)
  chk $ put_vara ncid varid start count val

-- | Write a strided slice of a variable to an open NetCDF file.
putS :: (NcStorable a, NcStore s, NcStoreExtraCon s a)
     => NcInfo NcWrite -> NcVar -> [Int] -> [Int] -> [Int] -> s a -> NcIO ()
putS nc var start count stride val = runAccess "putS" (ncName nc) $ do
  let ncid = ncId nc
      varid = (ncVarIds nc) M.! (ncVarName var)
  chk $ put_vars ncid varid start count stride val


-- | Helper function to read a single NC dimension.
read1Dim :: Int -> Int -> Int -> Access NcDim
read1Dim ncid unlim dimid = do
  (name, len) <- chk $ nc_inq_dim ncid dimid
  return $ NcDim name len (dimid == unlim)

-- | Helper function to write a single NC dimension.
write1Dim :: Int -> NcDim -> Access Int
write1Dim ncid (NcDim name len unlim) = do
  chk $ nc_def_dim ncid name (if unlim then ncUnlimitedLength else len)

-- | Helper function to read a single NC attribute.
read1Attr :: Int -> Int -> Int -> Access (Name, NcAttr)
read1Attr ncid varid attid = do
  n <- chk $ nc_inq_attname ncid varid attid
  (itype, len) <- chk $ nc_inq_att ncid varid n
  a <- readAttr ncid varid n (toEnum itype) len
  return (n, a)

-- | Helper function to write a single NC attribute.
write1Attr :: Int -> Int -> (Name, NcAttr) -> Access ()
write1Attr ncid varid (n, a) = writeAttr ncid varid n a

-- | Helper function to read metadata for a single NC variable.
read1Var :: Int -> [NcDim] -> Int -> Access NcVar
read1Var ncid dims varid = do
  (n, itype, nvdims, vdimids, nvatts) <- chk $ nc_inq_var ncid varid
  let vdims = map (dims !!) $ take nvdims vdimids
  vattrs <- forM [0..nvatts-1] (read1Attr ncid varid)
  let vattmap = foldl (\m (nm, a) -> M.insert nm a m) M.empty vattrs
  return $ NcVar n (toEnum itype) vdims vattmap

-- | Helper function to write metadata for a single NC variable.
write1Var :: Int -> M.Map Name Int -> NcVar -> Access Int
write1Var ncid dimidmap (NcVar n t dims as) = do
  let dimids = map ((dimidmap M.!) . ncDimName) dims
  varid <- chk $ nc_def_var ncid n (fromEnum t) (length dims) dimids
  forM_ (M.toList as) $ write1Attr ncid varid
  return varid

-- | Read an attribute from a NetCDF variable with error handling.
readAttr :: Int -> Int -> String -> NcType -> Int -> Access NcAttr
readAttr nc var n NcByte l =
  readAttr' nc var n l (NcAttrByte . map fromIntegral) nc_get_att_uchar
readAttr nc var n NcChar l = readAttr' nc var n l NcAttrChar nc_get_att_text
readAttr nc var n NcShort l = readAttr' nc var n l NcAttrShort nc_get_att_short
readAttr nc var n NcInt l = readAttr' nc var n l NcAttrInt nc_get_att_int
readAttr nc var n NcFloat l = readAttr' nc var n l NcAttrFloat nc_get_att_float
readAttr nc var n NcDouble l =
  readAttr' nc var n l NcAttrDouble nc_get_att_double

-- | Helper function for attribute reading.
readAttr' :: Int -> Int -> String -> Int -> ([a] -> NcAttr)
          -> (Int -> Int -> String -> Int -> IO (Int, [a])) -> Access NcAttr
readAttr' nc var n l w rf = chk $ do
  tmp <- rf nc var n l
  return $ (fst tmp, w $ snd tmp)

-- | Write an attribute to a NetCDF variable with error handling.
writeAttr :: Int -> Int -> String -> NcAttr -> Access ()
writeAttr nc var n (NcAttrByte v) =
  writeAttr' nc var n NcByte id v nc_put_att_uchar
writeAttr nc var n (NcAttrChar v) =
  writeAttr' nc var n NcChar id v nc_put_att_text
writeAttr nc var n (NcAttrShort v) =
  writeAttr' nc var n NcShort fromIntegral v nc_put_att_short
writeAttr nc var n (NcAttrInt v) =
  writeAttr' nc var n NcInt fromIntegral v nc_put_att_int
writeAttr nc var n (NcAttrFloat v) =
  writeAttr' nc var n NcFloat realToFrac v nc_put_att_float
writeAttr nc var n (NcAttrDouble v) =
  writeAttr' nc var n NcDouble realToFrac v nc_put_att_double

-- | Helper function for attribute writeing.
writeAttr' :: Int -> Int -> String -> NcType -> (a -> b) -> [a]
          -> (Int -> Int -> String -> Int -> [b] -> IO Int) -> Access ()
writeAttr' nc var n t conv vs wf = chk $ wf nc var n (fromEnum t) (map conv vs)


-- | Apply COARDS value scaling.
coardsScale :: forall a b s. (NcStorable a, NcStorable b, FromNcAttr a,
                              NcStore s, Real a, Fractional b,
                              NcStoreExtraCon s a, NcStoreExtraCon s b)
             => NcVar -> s a -> s b
coardsScale v din = smap xform din
  where offset = fromMaybe 0.0 $
                 ncVarAttr v "add_offset" >>= fromAttr :: CDouble
        scale = fromMaybe 1.0 $
                ncVarAttr v "scale_factor" >>= fromAttr :: CDouble
        fill = ncVarAttr v "_FillValue" >>= fromAttr :: Maybe a
        xform x = case fill of
          Nothing -> realToFrac $ realToFrac x * scale + offset
          Just f -> if x == f
                    then realToFrac f
                    else realToFrac $ realToFrac x * scale + offset
