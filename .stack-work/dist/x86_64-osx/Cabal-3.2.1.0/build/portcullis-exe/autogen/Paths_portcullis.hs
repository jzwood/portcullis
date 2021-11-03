{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_portcullis (
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

bindir     = "/Users/jakew/Documents/work/haskell/langs/portcullis/.stack-work/install/x86_64-osx/4b975a77dfaea358b5958deadf4b5949d061855ae1167da5adbee8daa02be7cc/8.10.7/bin"
libdir     = "/Users/jakew/Documents/work/haskell/langs/portcullis/.stack-work/install/x86_64-osx/4b975a77dfaea358b5958deadf4b5949d061855ae1167da5adbee8daa02be7cc/8.10.7/lib/x86_64-osx-ghc-8.10.7/portcullis-0.1.0.0-CjETfMuppxLFfR6V6pe41E-portcullis-exe"
dynlibdir  = "/Users/jakew/Documents/work/haskell/langs/portcullis/.stack-work/install/x86_64-osx/4b975a77dfaea358b5958deadf4b5949d061855ae1167da5adbee8daa02be7cc/8.10.7/lib/x86_64-osx-ghc-8.10.7"
datadir    = "/Users/jakew/Documents/work/haskell/langs/portcullis/.stack-work/install/x86_64-osx/4b975a77dfaea358b5958deadf4b5949d061855ae1167da5adbee8daa02be7cc/8.10.7/share/x86_64-osx-ghc-8.10.7/portcullis-0.1.0.0"
libexecdir = "/Users/jakew/Documents/work/haskell/langs/portcullis/.stack-work/install/x86_64-osx/4b975a77dfaea358b5958deadf4b5949d061855ae1167da5adbee8daa02be7cc/8.10.7/libexec/x86_64-osx-ghc-8.10.7/portcullis-0.1.0.0"
sysconfdir = "/Users/jakew/Documents/work/haskell/langs/portcullis/.stack-work/install/x86_64-osx/4b975a77dfaea358b5958deadf4b5949d061855ae1167da5adbee8daa02be7cc/8.10.7/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "portcullis_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "portcullis_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "portcullis_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "portcullis_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "portcullis_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "portcullis_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
