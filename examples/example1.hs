module Main where

import Prelude hiding (length, maximum, minimum, sum)
import Control.Applicative ((<$>))
import Data.NetCDF
import Data.NetCDF.Vector
import qualified Data.Map as M
import Foreign.C
import Data.Vector.Generic hiding ((++), map)
import qualified Data.Vector.Storable as SV

type SVRet a = IO (Either NcError (SV.Vector a))

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
  Right lon <- get nc lonvar :: SVRet CFloat
  let mlon = mean lon
  putStrLn $ "longitude: " ++ show lon ++ " -> " ++ show mlon
  Right lon2 <- getS nc lonvar [0] [72] [2] :: SVRet CFloat
  let mlon2 = mean lon2
  putStrLn $ "longitude (every 2): " ++ show lon2 ++ " -> " ++ show mlon2

  -- Get latitude variable: metadata, then all values, then a slice of
  -- values.
  let (Just latvar) = ncVar nc "latitude"
  Right lat <- get nc latvar :: SVRet CFloat
  let mlat = mean lat
  putStrLn $ "latitude: " ++ show lat ++ " -> " ++ show mlat
  Right lat2 <- getS nc latvar [0] [37] [2] :: SVRet CFloat
  let mlat2 = mean lat2
  putStrLn $ "latitude (every 2): " ++ show lat2 ++ " -> " ++ show mlat2

  -- Get Z500 variable: get a slice, do COARDS short -> double
  -- scaling, convert units.
  let (Just zvar) = ncVar nc "z500"
  putStrLn $ "z500 dims: " ++ show (map ncDimName $ ncVarDims zvar)
  Right slice1tmp <- getA nc zvar [0, 0, 0] [1, nlat, nlon] :: SVRet CShort
  let slice1 = SV.map (/ 9.8) $ coardsScale zvar slice1tmp :: SV.Vector CDouble
  putStrLn $ "length slice1 = " ++ show (length slice1)
  let idx i j = (nlat - j) * nlon + (i - 1)
  putStrLn $ "lon(i=25) = " ++ show (lon SV.! (25 - 1))
  putStrLn $ "lat(j=40) = " ++ show (lat SV.! (nlat - 40))
  putStrLn $ "slice1(i=25,j=40) = " ++ show (slice1 SV.! idx 25 40)

mean :: (Fractional a, Vector v a) => v a -> a
mean xs = sum xs / fromIntegral (length xs)
