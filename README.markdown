hnetcdf
=======

[![Build Status](https://travis-ci.org/ian-ross/hnetcdf.svg?branch=master)](https://travis-ci.org/ian-ross/hnetcdf)

Haskell NetCDF library: as well as conventional low-level FFI bindings
to the functions in the NetCDF library (in the `Data.NetCDF.Raw`
modules), `hnetcdf` provides a higher-level Haskell interface
(currently only for reading data).  This higher-level interface aims
to provide a "container polymorphic" view of NetCDF data allowing
NetCDF variables to be read into `Storable` `Vectors` and Repa arrays
easily.

For example:

``` haskell
import Data.NetCDF
import Foreign.C
import qualified Data.Vector.Storable as SV
...
type SVRet = IO (Either NcError (SV.Vector a))
...
  enc <- openFile "tst.nc" ReadMode
  case enc of
    Right nc -> do
      eval <- get nc "varname" :: SVRet CDouble
      ...
```

gets the full contents of a NetCDF variable as a `Storable` `Vector`,
while the following code reads the same variable (assumed to be
three-dimensional) into a Repa array:

``` haskell
import Data.NetCDF
import Foreign.C
import qualified Data.Array.Repa as R
import qualified Data.Array.Repa.Eval as RE
import Data.Array.Repa.Repr.ForeignPtr (F)
...
type RepaRet3 a = IO (Either NcError (R.Array F R.DIM3 a))
...
  enc <- openFile "tst.nc" ReadMode
  case enc of
    Right nc -> do
      eval <- get nc "varname" :: RepaRet3 CDouble
      ...
```
