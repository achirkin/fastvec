module Main (main) where

import Distribution.Simple
import Distribution.PackageDescription
import Distribution.Simple.Setup
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program
import Distribution.Simple.Program.Run
import Distribution.System
import Distribution.Verbosity


import Control.Arrow ((***))
import Data.Maybe (fromMaybe, maybe)
import Data.List (isPrefixOf)
import Control.Monad (forM, unless)
import System.Directory
import System.FilePath
import qualified Data.Map as Map (lookup)
import Debug.Trace

main :: IO ()
main = do
  defaultMainWithHooks simpleUserHooks
         { buildHook = customBuildHook (buildHook simpleUserHooks)
         , copyHook  = customCopyHook (copyHook simpleUserHooks)
         , instHook  = customInstHook (instHook simpleUserHooks)
         , sDistHook = customSDistHook (sDistHook simpleUserHooks)
         , regHook   = customRegHook (regHook simpleUserHooks)
         }


customBuildHook :: (PackageDescription -> LocalBuildInfo -> UserHooks -> BuildFlags -> IO ())
                -> PackageDescription -> LocalBuildInfo -> UserHooks -> BuildFlags -> IO ()
customBuildHook bf packageDesc localBuildInfo userHooks buildFlags = do

    -- If building for the first time, generate platform-dependent LLVM headers
    testOnceCompiled <- doesFileExist ("codegen" </> "platformtest.o")
    platformhsExists <- doesFileExist ("codegen" </> "Platform.hs")
    case (testOnceCompiled && platformhsExists, mcprogram) of
      (True, _) -> return ()
      (_, Nothing) -> return ()
      (_, Just ghc) -> do
        let runghc = programInvocation ghc ["-ddump-llvm", "-c", "codegen" </> "platformtest.cmm"]
        putStrLn "Invoking test GHC run to compile .cmm into .ll to figure out proper target datalayout and triple..."
        platformTest <- lines <$> getProgramInvocationOutput deafening runghc
        let (tDatalayout, tTriple) = (fromMaybe "" *** fromMaybe "") $ parseLlvmOutput platformTest
        putStrLn tDatalayout
        putStrLn tTriple
        writeFile ("codegen" </> "Platform.hs") . unlines $
          [ "module Platform where"
          , ""
          , "targetDataLayout :: String"
          , "targetDataLayout = " ++ show tDatalayout
          , ""
          , "targetTriple :: String"
          , "targetTriple = " ++ show tTriple
          ]

    -- Compile only codegen to generate sources for library
    bf (pdOnlyCodegen packageDesc)
       (lbiOnlyCodegen localBuildInfo)
       userHooks
       (bfOnlyCodegen buildFlags)

    -- Add all generated LLVM files as GHC arguments:
    --   This way all generated LLVM IR functions are visible to all haskell files;
    --   however, all functions must have globally unique names
    let lbi = lbiNoCodegen localBuildInfo
    lbf' <- case library $ localPkgDescr lbi of
      Nothing -> return lbi
      Just lib -> do
        addedFiles <- allDirsRec $ hsSourceDirs $ libBuildInfo lib
        return lbi
          { localPkgDescr = (localPkgDescr lbi)
              { library = Just lib
                 { libBuildInfo = (libBuildInfo lib)
                    { options = map (updateOpts addedFiles) $ options (libBuildInfo lib)
                    }
                 }
              }
          }

    -- compile everything except codegen
    bf (pdNoCodegen packageDesc)
       lbf'
       userHooks
       (bfNoCodegen buildFlags)
  where
    ps = withPrograms localBuildInfo
    mcprogram = (\p ->
                  let xs = preserveLlvmArgs (programDefaultArgs p)
                      ys0 = preserveLlvmArgs (programOverrideArgs p)
                      ys1 = if "-pgmlc" `elem` xs || "-pgmlc" `elem` ys0
                            then ys0
                            else "-pgmlc":cllc:ys0
                      ys2 = if "-pgmlo" `elem` xs || "-pgmlo" `elem` ys1
                            then ys1
                            else "-pgmlo":copt:ys1
                   in p { programDefaultArgs  = xs
                        , programOverrideArgs = ys2
                        }
                ) <$> lookupProgram (simpleProgram "ghc") ps
    updateOpts addedFiles (GHC, opts) = (GHC, opts ++ addedFiles)
    updateOpts _ xs = xs
    allDirsRec [] = return []
    allDirsRec dirs = do
       paths <- concatMap (\(d,ps) -> map (d </>) $ filter (\p -> p /= "." && p /= "..") ps)
                       . zip dirs <$> mapM getDirectoryContents dirs
       rezs <- fmap (map snd . filter fst) . forM paths $ \p -> do
          exists <- doesFileExist p
          return (exists && isPrefixOf "codegen_" (takeBaseName p)
                         && ".ll" == takeExtensions p
                 , p)
       dirs' <- fmap (map snd . filter fst) . forM paths $ \p -> do
          exists <- doesDirectoryExist p
          return (exists, p)
       moref <- allDirsRec dirs'
       return $ rezs ++ moref
    parseLlvmOutput (x:xs)
        | "target datalayout" `isPrefixOf` x = (Just x, snd (parseLlvmOutput xs))
        | "target triple"     `isPrefixOf` x = (fst (parseLlvmOutput xs), Just x)
        | otherwise                          = parseLlvmOutput xs
    parseLlvmOutput [] = (Nothing,Nothing)

    -- if we are in an unfriendly environment of ambiguous LLVM versions
    -- hope that user specified a proper version in --ghc-options
    preserveLlvmArgs ("-pgmlc":p:xs) = "-pgmlc":p:preserveLlvmArgs xs
    preserveLlvmArgs ("-pgmlo":p:xs) = "-pgmlo":p:preserveLlvmArgs xs
    preserveLlvmArgs (x:xs) = preserveLlvmArgs xs
    preserveLlvmArgs [] = []


    -- last resort: llc and opt might be specified in compiler properties
    compilerProps = compilerProperties $ compiler localBuildInfo
    cllc = fromMaybe "llc" $ Map.lookup "LLVM llc command" compilerProps
    copt = fromMaybe "llc" $ Map.lookup "LLVM opt command" compilerProps




----------------------------------------------------------------------------------------------------
-- All of these is needed to build codegen before anything else, and disable its registration
----------------------------------------------------------------------------------------------------

customCopyHook :: (PackageDescription -> LocalBuildInfo -> UserHooks -> CopyFlags -> IO ())
               -> PackageDescription -> LocalBuildInfo -> UserHooks -> CopyFlags -> IO ()
customCopyHook copyh pd lbi = copyh (pdNoCodegen pd) (lbiNoCodegen lbi)

customInstHook :: (PackageDescription -> LocalBuildInfo -> UserHooks -> InstallFlags -> IO ())
               -> PackageDescription -> LocalBuildInfo -> UserHooks -> InstallFlags -> IO ()
customInstHook insth pd lbi = insth (pdNoCodegen pd) (lbiNoCodegen lbi)


customSDistHook :: (PackageDescription -> Maybe LocalBuildInfo -> UserHooks -> SDistFlags -> IO ())
               -> PackageDescription -> Maybe LocalBuildInfo -> UserHooks -> SDistFlags -> IO ()
customSDistHook sdisth pd lbi = sdisth (pdNoCodegen pd) (lbiNoCodegen <$> lbi)


customRegHook :: (PackageDescription -> LocalBuildInfo -> UserHooks -> RegisterFlags -> IO ())
               -> PackageDescription -> LocalBuildInfo -> UserHooks -> RegisterFlags -> IO ()
customRegHook regh pd lbi = regh (pdNoCodegen pd) (lbiNoCodegen lbi)

--confSeparate :: IORef (Maybe LocalBuildInfo)
--             -> ((GenericPackageDescription, HookedBuildInfo) -> ConfigFlags -> IO LocalBuildInfo)
--             -> (GenericPackageDescription, HookedBuildInfo) -> ConfigFlags -> IO LocalBuildInfo
--confSeparate lbiRef copyh pd cf = do
--  lbi <- copyh pd cf
--  writeIORef lbiRef (Just $ lbiOnlyCodegen lbi)
--  return (lbiNoCodegen lbi)


pdOnlyCodegen :: PackageDescription -> PackageDescription
pdOnlyCodegen pd@PackageDescription { executables = execs }
    = pd { library = Nothing
         , executables = filter fcodegen execs
         }
  where
    fcodegen Executable{exeName = "codegen"} = True
    fcodegen _ = False

lbiOnlyCodegen :: LocalBuildInfo -> LocalBuildInfo
lbiOnlyCodegen lbi@LocalBuildInfo { localPkgDescr = lpd }
    = lbi { localPkgDescr = pdOnlyCodegen lpd}

bfOnlyCodegen :: BuildFlags -> BuildFlags
bfOnlyCodegen bf@BuildFlags{ buildArgs = xs} = bf { buildArgs = filter ("exe:codegen" ==) xs}

pdNoCodegen :: PackageDescription -> PackageDescription
pdNoCodegen pd@PackageDescription { executables = execs }
    = pd { executables = filter fnocodegen execs }
  where
    fnocodegen Executable{exeName = "codegen"} = False
    fnocodegen _ = True

lbiNoCodegen :: LocalBuildInfo -> LocalBuildInfo
lbiNoCodegen lbi@LocalBuildInfo { localPkgDescr = lpd }
    = lbi { localPkgDescr = pdNoCodegen lpd}


bfNoCodegen :: BuildFlags -> BuildFlags
bfNoCodegen bf@BuildFlags{ buildArgs = xs} = bf { buildArgs = filter ("exe:codegen" /=) xs}


