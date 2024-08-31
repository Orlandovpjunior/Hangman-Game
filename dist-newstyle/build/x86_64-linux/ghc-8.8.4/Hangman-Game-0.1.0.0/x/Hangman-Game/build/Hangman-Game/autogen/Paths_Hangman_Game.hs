{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_Hangman_Game (
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

bindir     = "/home/gabrielcabral/.cabal/bin"
libdir     = "/home/gabrielcabral/.cabal/lib/x86_64-linux-ghc-8.8.4/Hangman-Game-0.1.0.0-inplace-Hangman-Game"
dynlibdir  = "/home/gabrielcabral/.cabal/lib/x86_64-linux-ghc-8.8.4"
datadir    = "/home/gabrielcabral/.cabal/share/x86_64-linux-ghc-8.8.4/Hangman-Game-0.1.0.0"
libexecdir = "/home/gabrielcabral/.cabal/libexec/x86_64-linux-ghc-8.8.4/Hangman-Game-0.1.0.0"
sysconfdir = "/home/gabrielcabral/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Hangman_Game_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Hangman_Game_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "Hangman_Game_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "Hangman_Game_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Hangman_Game_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Hangman_Game_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
