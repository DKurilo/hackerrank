{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-safe-haskell-mode #-}
module Paths_the_resistance (
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

bindir     = "/Users/dkurilo/personal/hackerrank/codingame/the-resistance/.stack-work/install/x86_64-osx/ebeaece99f8bd61668cbdde49ef281dbf88211ac1fbf1546567bad1a4d765353/9.0.2/bin"
libdir     = "/Users/dkurilo/personal/hackerrank/codingame/the-resistance/.stack-work/install/x86_64-osx/ebeaece99f8bd61668cbdde49ef281dbf88211ac1fbf1546567bad1a4d765353/9.0.2/lib/x86_64-osx-ghc-9.0.2/the-resistance-0.1.0.0-G4lkw92tesD1P190OdSaxc-the-resistance"
dynlibdir  = "/Users/dkurilo/personal/hackerrank/codingame/the-resistance/.stack-work/install/x86_64-osx/ebeaece99f8bd61668cbdde49ef281dbf88211ac1fbf1546567bad1a4d765353/9.0.2/lib/x86_64-osx-ghc-9.0.2"
datadir    = "/Users/dkurilo/personal/hackerrank/codingame/the-resistance/.stack-work/install/x86_64-osx/ebeaece99f8bd61668cbdde49ef281dbf88211ac1fbf1546567bad1a4d765353/9.0.2/share/x86_64-osx-ghc-9.0.2/the-resistance-0.1.0.0"
libexecdir = "/Users/dkurilo/personal/hackerrank/codingame/the-resistance/.stack-work/install/x86_64-osx/ebeaece99f8bd61668cbdde49ef281dbf88211ac1fbf1546567bad1a4d765353/9.0.2/libexec/x86_64-osx-ghc-9.0.2/the-resistance-0.1.0.0"
sysconfdir = "/Users/dkurilo/personal/hackerrank/codingame/the-resistance/.stack-work/install/x86_64-osx/ebeaece99f8bd61668cbdde49ef281dbf88211ac1fbf1546567bad1a4d765353/9.0.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "the_resistance_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "the_resistance_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "the_resistance_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "the_resistance_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "the_resistance_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "the_resistance_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
