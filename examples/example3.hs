{-# LANGUAGE FlexibleContexts #-}
module Main where

import Prelude hiding (length, sum)
import Control.Applicative ((<$>))
import Data.NetCDF
import Data.NetCDF.HMatrix
import Data.NetCDF.Store (smap)
import qualified Data.Map as M
import Foreign.C
import Foreign.Storable
import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Devel (foldVector)

type VRet a = IO (Either NcError (HVector a))
type MRet a = IO (Either NcError (HMatrix a))

main :: IO ()
main = do
  -- Open file and access some basic metadata.
  Right nc <- openFile "z500-short.nc"
  putStrLn $ "Name: " ++ ncName nc
  putStrLn $ "Dims: " ++ show (M.keys $ ncDims nc)
  putStr $ unlines $ map (\(n, s) -> "  " ++ n ++ ": " ++ s) $
    M.toList $ flip M.map (ncDims nc) $
    \d -> show (ncDimLength d) ++ if ncDimUnlimited d then " (UNLIM)" else ""
  putStrLn $ "Vars: " ++ show (M.keys $ ncVars nc)
  putStrLn $ "Global attributes: " ++ show (M.keys $ ncAttrs nc)

  -- Get dimension sizes.
  let Just ntime = ncDimLength <$> ncDim nc "time"
      Just nlat = ncDimLength <$> ncDim nc "latitude"
      Just nlon = ncDimLength <$> ncDim nc "longitude"

  -- Get longitude variable: metadata, then all values, then a slice
  -- of values.
  let (Just lonvar) = ncVar nc "longitude"
  Right lon' <- get nc lonvar :: VRet CFloat
  let HVector lon = smap realToFrac lon' :: HVector Float
      mlon = mean lon
  putStrLn $ "longitude: " ++ show lon ++ " -> " ++ show mlon
  Right lon2' <- getS nc lonvar [0] [72] [2] :: VRet CFloat
  let HVector lon2  = smap realToFrac lon2' :: HVector Float
      mlon2 = mean lon2
  putStrLn $ "longitude (every 2): " ++ show lon2 ++ " -> " ++ show mlon2

  -- Get latitude variable: metadata, then all values, then a slice of
  -- values.
  let (Just latvar) = ncVar nc "latitude"
  Right lat' <- get nc latvar :: VRet CFloat
  let HVector lat  = smap realToFrac lat' :: HVector Float
      mlat = mean lat
  putStrLn $ "latitude: " ++ show lat ++ " -> " ++ show mlat
  Right lat2' <- getS nc latvar [0] [37] [2] :: VRet CFloat
  let (HVector lat2)  = smap realToFrac lat2' :: HVector Float
      mlat2 = mean lat2
  putStrLn $ "latitude (every 2): " ++ show lat2 ++ " -> " ++ show mlat2

  -- Get Z500 variable: get a slice, do COARDS short -> double
  -- scaling, convert units.
  let (Just zvar) = ncVar nc "z500"
  putStrLn $ "z500 dims: " ++ show (map ncDimName $ ncVarDims zvar)
  Right slice1tmp <-
    getA nc zvar [0, 0, 0] [1, nlat, nlon] :: MRet CShort
  let (HMatrix slice1tmp2) = coardsScale zvar slice1tmp :: HMatrix CDouble
      slice1 = cmap ((/ 9.8) . realToFrac) slice1tmp2 :: Matrix Double
  putStrLn $ "size slice1 = " ++
    show (rows slice1) ++ " x " ++ show (cols slice1)
  putStrLn $ "lon(i=25) = " ++ show (lon `atIndex` (25 - 1))
  putStrLn $ "lat(j=40) = " ++ show (lat `atIndex` (nlat - 40))
  let (@!!) :: Matrix Double -> (Int, Int) -> Double
      v @!! (i, j) = v `atIndex` (nlat - i, j - 1)
  putStrLn $ "slice1(i=25,j=40) = " ++ show (slice1 @!! (25, 40))

mean :: (Storable a, Fractional a, Container Vector a) => Vector a -> a -- FlexibleContexts
mean xs = (foldVector (+) 0 xs) / fromIntegral (size xs)
