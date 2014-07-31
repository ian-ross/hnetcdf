module Main where

import Prelude hiding (length, maximum, minimum, sum)
import Control.Applicative ((<$>))
import qualified Data.Map as M
import Foreign.C
import Data.Vector.Generic hiding ((++), map)
import qualified Data.Array.Repa as Repa
import Data.Array.Repa.Repr.ForeignPtr (F)
import Data.Array.Repa.Repr.Unboxed (U)
import qualified Data.Vector.Storable as SV

import Data.NetCDF
import qualified Data.NetCDF.Vector as V
import qualified Data.NetCDF.Repa as R

type SVRet a = IO (Either NcError (SV.Vector a))
type FArray2 a = Repa.Array F Repa.DIM2 a
type Array2 a = Repa.Array U Repa.DIM2 a
type RepaRet2 a = IO (Either NcError (FArray2 a))
type RepaRet1 a = IO (Either NcError (Repa.Array F Repa.DIM1 a))

main :: IO ()
main = do
  Right nc <- openFile "z500-short.nc"

  let Just ntime = ncDimLength <$> ncDim nc "time"
      Just nlat = ncDimLength <$> ncDim nc "latitude"
      Just nlon = ncDimLength <$> ncDim nc "longitude"

  let (Just timevar) = ncVar nc "time"
  Right time <- get nc timevar :: SVRet CInt
  let (Just lonvar) = ncVar nc "longitude"
  Right lon <- get nc lonvar :: SVRet CFloat
  let (Just latvar) = ncVar nc "latitude"
  Right lat <- get nc latvar :: SVRet CFloat
  let (Just zvar) = ncVar nc "z500"

  let late = vectorIndex LT FromEnd lat 40.0
      lats = vectorIndex GT FromStart lat 60.0
      lons = vectorIndex LT FromStart lon 10.0
      lone = vectorIndex GT FromEnd lon 30.0
      start = [0, lats, lons]
      count = [1, late - lats + 1, lone - lons + 1]
  Right slice1tmp <- getA nc zvar start count :: RepaRet2 CShort
  let slice1tmp2 = coardsScale zvar slice1tmp :: FArray2 CDouble
  let slice1 = (Repa.computeS $
                Repa.map (realToFrac . (/ 9.8)) slice1tmp2) :: Array2 Double
  putStrLn $ "size slice1 = " ++ show (Repa.extent slice1)
  putStrLn $ "maximum slice1 = " ++ show (Repa.foldAllS max (-1.0E9) slice1)
  putStrLn $ "minimum slice1 = " ++ show (Repa.foldAllS min (1.0E9) slice1)

mean :: (Fractional a, Vector v a) => v a -> a
mean xs = sum xs / fromIntegral (length xs)

data IndexStart = FromStart | FromEnd

vectorIndex :: (SV.Storable a, Ord a)
            => Ordering -> IndexStart -> SV.Vector a -> a -> Int
vectorIndex o s v val = case (go o, s) of
  (Nothing, _) -> (-1)
  (Just i, FromStart) -> i
  (Just i, FromEnd) -> SV.length v - 1 - i
  where go LT = SV.findIndex (>= val) vord
        go GT = SV.findIndex (<= val) vord
        vord = case s of
          FromStart -> v
          FromEnd -> SV.reverse v
