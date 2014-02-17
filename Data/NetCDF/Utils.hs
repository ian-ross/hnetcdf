{-# LANGUAGE TypeFamilies, FlexibleInstances #-}
module Data.NetCDF.Utils where

import Data.NetCDF.Raw
import Data.NetCDF.Types

import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Either

-- | Simple synonym to tidy up signatures.
type NcIO a = IO (Either NcError a)

-- | Monad stack to help with handling errors from FFI functions.
type Access a = ReaderT (String, FilePath) (EitherT NcError IO) a

-- | Utility function to run access monad stack.
runAccess :: String -> String -> Access a -> NcIO a
runAccess f p = runEitherT . flip runReaderT (f, p)

-- | Utility class to make dealing with status return from foreign
-- NetCDF functions a little easier.
class Checkable a where
  type OutType a :: *
  status :: a -> Int
  proj :: a -> OutType a

instance Checkable Int where
  type OutType Int = ()
  status s = s
  proj _ = ()

instance Checkable (Int, a) where
  type OutType (Int, a) = a
  status (s, _) = s
  proj (_, a) = a

instance Checkable (Int, a, b) where
  type OutType (Int, a, b) = (a, b)
  status (s, _, _) = s
  proj (_, a, b) = (a, b)

instance Checkable (Int, a, b, c) where
  type OutType (Int, a, b, c) = (a, b, c)
  status (s, _, _, _) = s
  proj (_, a, b, c) = (a, b, c)

instance Checkable (Int, a, b, c, d) where
  type OutType (Int, a, b, c, d) = (a, b, c, d)
  status (s, _, _, _, _) = s
  proj (_, a, b, c, d) = (a, b, c, d)

instance Checkable (Int, a, b, c, d, e) where
  type OutType (Int, a, b, c, d, e) = (a, b, c, d, e)
  status (s, _, _, _, _, _) = s
  proj (_, a, b, c, d, e) = (a, b, c, d, e)

-- | Perform an IO action that returns a tuple with an integer status
-- and some return values, processing errors.
chk :: Checkable a => IO a -> Access (OutType a)
chk act = do
  res <- lift $ liftIO $ act
  let st = status res
      val = proj res
  (f, p) <- ask
  lift $ if st == 0
    then right val
    else left $ NcError f st (nc_strerror st) p
