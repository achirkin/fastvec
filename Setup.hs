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

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
         { buildHook = customBuildHook (buildHook simpleUserHooks)
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
        let (tDatalayout, tTriple) = (fromMaybe "target datalayout = \"\"" *** fromMaybe "target triple = \"\"") $ parseLlvmOutput platformTest
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

    -- Add all generated LLVM files as GHC arguments:
    --   This way all generated LLVM IR functions are visible to all haskell files;
    --   however, all functions must have globally unique names
    lbf' <- case library $ localPkgDescr localBuildInfo of
      Nothing -> return localBuildInfo
      Just lib -> do
        addedFiles <- allDirsRec $ hsSourceDirs $ libBuildInfo lib
        return localBuildInfo
          { localPkgDescr = (localPkgDescr localBuildInfo)
              { library = Just lib
                 { libBuildInfo = (libBuildInfo lib)
                    { options = map (updateOpts addedFiles) $ options (libBuildInfo lib)
                    }
                 }
              }
          }
    bf packageDesc lbf' userHooks buildFlags
  where
    ps = withPrograms localBuildInfo
    mcprogram = (\p -> p { programDefaultArgs = [], programOverrideArgs = [] } ) <$> lookupProgram (simpleProgram "ghc") ps
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

