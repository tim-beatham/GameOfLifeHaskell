{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_GameOfLifeGUI (
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

bindir     = "/home/tim/.cabal/bin"
libdir     = "/home/tim/.cabal/lib/x86_64-linux-ghc-8.8.3/GameOfLifeGUI-0.1.0.0-inplace-gameoflife"
dynlibdir  = "/home/tim/.cabal/lib/x86_64-linux-ghc-8.8.3"
datadir    = "/home/tim/.cabal/share/x86_64-linux-ghc-8.8.3/GameOfLifeGUI-0.1.0.0"
libexecdir = "/home/tim/.cabal/libexec/x86_64-linux-ghc-8.8.3/GameOfLifeGUI-0.1.0.0"
sysconfdir = "/home/tim/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "GameOfLifeGUI_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "GameOfLifeGUI_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "GameOfLifeGUI_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "GameOfLifeGUI_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "GameOfLifeGUI_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "GameOfLifeGUI_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
