module Paths_conference_management_system (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/jeanyang/.cabal/bin"
libdir     = "/home/jeanyang/.cabal/lib/x86_64-linux-ghc-7.10.3/conference-management-system-0.0.0-9ert6BqWkZk1PeIVWZvoBZ"
datadir    = "/home/jeanyang/.cabal/share/x86_64-linux-ghc-7.10.3/conference-management-system-0.0.0"
libexecdir = "/home/jeanyang/.cabal/libexec"
sysconfdir = "/home/jeanyang/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "conference_management_system_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "conference_management_system_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "conference_management_system_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "conference_management_system_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "conference_management_system_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
