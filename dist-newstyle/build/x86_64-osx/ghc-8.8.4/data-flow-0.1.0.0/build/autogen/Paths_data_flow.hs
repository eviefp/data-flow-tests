{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_data_flow (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/lf33ge/.cabal/bin"
libdir     = "/Users/lf33ge/.cabal/lib/x86_64-osx-ghc-8.8.4/data-flow-0.1.0.0-inplace"
dynlibdir  = "/Users/lf33ge/.cabal/lib/x86_64-osx-ghc-8.8.4"
datadir    = "/Users/lf33ge/.cabal/share/x86_64-osx-ghc-8.8.4/data-flow-0.1.0.0"
libexecdir = "/Users/lf33ge/.cabal/libexec/x86_64-osx-ghc-8.8.4/data-flow-0.1.0.0"
sysconfdir = "/Users/lf33ge/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "data_flow_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "data_flow_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "data_flow_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "data_flow_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "data_flow_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "data_flow_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
