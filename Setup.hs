{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE CPP #-}
module Main (main) where

import Distribution.Simple

#if defined(ghcjs_HOST_OS)
#else
import Distribution.PackageDescription
import Distribution.Simple.Setup
import Distribution.Simple.Program.Types
import Distribution.Simple.Program.Db
import Distribution.Simple.LocalBuildInfo

import Control.Monad (liftM)
import Data.Maybe (fromMaybe)

import System.Directory
#endif

main :: IO ()
#if defined(ghcjs_HOST_OS)
main = defaultMain
#else
main = defaultMainWithHooks simpleUserHooks
         { confHook = customConfHook (confHook simpleUserHooks)
         , regHook  = customRegHook  (regHook  simpleUserHooks)
         }


customConfHook :: ((GenericPackageDescription, HookedBuildInfo) -> ConfigFlags -> IO LocalBuildInfo)
               -> (GenericPackageDescription, HookedBuildInfo) -> ConfigFlags -> IO LocalBuildInfo
customConfHook cf params flags = do
    lbi <- cf params flags
    fpaths <- getAdditionalFiles (buildDir lbi)
    return . addPrimOpFilesFlags fpaths $ lbi

customRegHook :: (PackageDescription -> LocalBuildInfo -> UserHooks -> RegisterFlags -> IO ())
              ->  PackageDescription -> LocalBuildInfo -> UserHooks -> RegisterFlags -> IO ()
customRegHook rhF pdisc = rhF pdisc . removeAddedGHCflags


-- | Add special glafs to GHC and linkers
addPrimOpFilesFlags :: ([String], [String]) -> LocalBuildInfo -> LocalBuildInfo
addPrimOpFilesFlags (llvmFiles, objFiles) lbi = lbi {withPrograms = f $ withPrograms lbi}
    where f = userSpecifyArgss
            [ ("ar", objFiles)
            , ("runlib", objFiles)
            , ("ld", objFiles)
            , ("ghc", "-mavx2":llvmFiles)
            ]

-- | Remove all user-specified flags from GHC
removeAddedGHCflags :: LocalBuildInfo -> LocalBuildInfo
removeAddedGHCflags lbi = lbi {withPrograms = f $ withPrograms lbi}
    where f progDB = fromMaybe progDB
                   . liftM (flip updateProgram progDB . suppressOverrideArgs)
                   $ lookupKnownProgram "ghc" progDB >>= flip lookupProgram progDB

-- | List PrimOp files (LLVM and Objects)
getAdditionalFiles :: String -> IO ([String], [String])
getAdditionalFiles distDir = do
    -- find all LLVM files for compilation
    primObjs <- liftM (filter ((=="ll.") . take 3 . reverse)) (getDirectoryContents primsDir)
    -- list all paths to LLVM files and to compiled object files
    let llvmFiles = map (primsDir ++) primObjs
        objFiles = map ((distDir ++) . ('/':) . reverse . ('o':) . drop 2 . reverse) llvmFiles
    return (llvmFiles, objFiles)
    where primsDir = "prims/"

#endif
