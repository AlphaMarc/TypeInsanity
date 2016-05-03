module Paths_TypeInsanity (
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
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\mallaire\\AppData\\Roaming\\cabal\\bin"
libdir     = "C:\\Users\\mallaire\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-7.10.3\\TypeInsanity-0.1.0.0-4i5F3hJQCTp6gcCLyUF4Xi"
datadir    = "C:\\Users\\mallaire\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-7.10.3\\TypeInsanity-0.1.0.0"
libexecdir = "C:\\Users\\mallaire\\AppData\\Roaming\\cabal\\TypeInsanity-0.1.0.0-4i5F3hJQCTp6gcCLyUF4Xi"
sysconfdir = "C:\\Users\\mallaire\\AppData\\Roaming\\cabal\\etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "TypeInsanity_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "TypeInsanity_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "TypeInsanity_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "TypeInsanity_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "TypeInsanity_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
