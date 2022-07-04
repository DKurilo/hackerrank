{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_vox_codei_episode_two (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/Users/dkurilo/personal/hackerrank/codingame/vox-codei-episode-two/.stack-work/install/x86_64-osx/b5a8ecd786d5a95b779a0a01a4416980599daced89a96aa0219dbdefd042a628/9.2.2/bin"
libdir     = "/Users/dkurilo/personal/hackerrank/codingame/vox-codei-episode-two/.stack-work/install/x86_64-osx/b5a8ecd786d5a95b779a0a01a4416980599daced89a96aa0219dbdefd042a628/9.2.2/lib/x86_64-osx-ghc-9.2.2/vox-codei-episode-two-0.1.0.0-EAOJdHzIF7o96yoUFAC2K1-vox-codei-episode-two"
dynlibdir  = "/Users/dkurilo/personal/hackerrank/codingame/vox-codei-episode-two/.stack-work/install/x86_64-osx/b5a8ecd786d5a95b779a0a01a4416980599daced89a96aa0219dbdefd042a628/9.2.2/lib/x86_64-osx-ghc-9.2.2"
datadir    = "/Users/dkurilo/personal/hackerrank/codingame/vox-codei-episode-two/.stack-work/install/x86_64-osx/b5a8ecd786d5a95b779a0a01a4416980599daced89a96aa0219dbdefd042a628/9.2.2/share/x86_64-osx-ghc-9.2.2/vox-codei-episode-two-0.1.0.0"
libexecdir = "/Users/dkurilo/personal/hackerrank/codingame/vox-codei-episode-two/.stack-work/install/x86_64-osx/b5a8ecd786d5a95b779a0a01a4416980599daced89a96aa0219dbdefd042a628/9.2.2/libexec/x86_64-osx-ghc-9.2.2/vox-codei-episode-two-0.1.0.0"
sysconfdir = "/Users/dkurilo/personal/hackerrank/codingame/vox-codei-episode-two/.stack-work/install/x86_64-osx/b5a8ecd786d5a95b779a0a01a4416980599daced89a96aa0219dbdefd042a628/9.2.2/etc"

getBinDir     = catchIO (getEnv "vox_codei_episode_two_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "vox_codei_episode_two_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "vox_codei_episode_two_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "vox_codei_episode_two_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "vox_codei_episode_two_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "vox_codei_episode_two_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
