{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Llvm
-- Copyright   :  Copyright (C) 2015 Artem M. Chirkin <chirkin@arch.ethz.ch>
-- License     :  BSD3
--
-- Maintainer  :  Artem M. Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  Experimental
--
--
-----------------------------------------------------------------------------
module Llvm
  ( module Llvm.BaseClass
  , module Llvm.ProgramMonad
  , module Llvm.Numbers
  , module Llvm.Expression
  , module Llvm.Function
  , genDefs
  , writeAllFilesFun
  ) where

import Data.List (intercalate)

import Llvm.BaseClass
import Llvm.ProgramMonad
import Llvm.Numbers
import Llvm.Arch.CodeAMD64 ()
import Llvm.Expression
import Llvm.Function

import Control.Arrow (second, (***))
import Language.Haskell.TH
import System.FilePath
import System.Directory
import Platform


-- | Generate all code in all files
writeAllFilesFun :: Q [Dec]
writeAllFilesFun = do
  ClassI _ instances <- reify ''LlvmModule
  let calls = DoE $ instances >>= map NoBindS . genWriteCall
  runIO $ print instances
  [d| writeAllFiles :: IO ()
      writeAllFiles = $(pure calls)
   |]


genWriteCall :: Dec -> [Exp]
genWriteCall
  ( InstanceD
#if MIN_VERSION_template_haskell(2,11,0)
  _
#endif
  _ (AppT (ConT _) t@(LitT (StrTyLit _))) _ ) =
      [ VarE 'moduleCodegen `AppE` fName
      , VarE 'moduleDefgen `AppE` fName]
  where
    fName = SigE (ConE 'FName) (AppT (ConT ''FName) t)
genWriteCall _ = []

-- | Generate a special class instance
--
--   @
--    class LlvmModule (name::Symbol) where
--      moduleCodegen :: FName name -> IO ()
--      moduleDefgen  :: FName name -> IO ()
--   @
--
--   This instance aggregates all definitions in this module and makes two functions
--   to write the definitions in corresponding .ll and .hs files.
genDefs :: Q [Dec]
genDefs = do
  myloc <- location
  let hsFilename = modGenPrim . modSrc $ loc_filename myloc
      llFilename = modName hsFilename
      hsModulename = modGenModule $ loc_module myloc
  -- make sure directories exist
  runIO . createDirectoryIfMissing True . fst $ splitFileName llFilename
  ClassI _ instances <- reify ''DefineFunction
  let calls = (ListE *** ListE) . unzip . filterJust $ map genFunCalls instances
      writeModule = genWriteModule hsFilename hsModulename $ snd calls
      writeLlvm   = genWriteLlvm llFilename $ fst calls
      decInstance = genLlvmModuleInstance hsModulename writeModule writeLlvm
  return [decInstance]
  where
    modName = uncurry (</>) .  second ((-<.> ".ll") . ("codegen_" ++)) . splitFileName
    replaceFirst pat val (x:xs) | x == pat = val:xs
                                | otherwise = x : replaceFirst pat val xs
    replaceFirst _ _ [] = []
    modSrc = joinPath . replaceFirst "codegen" "src" . splitDirectories
    modGenPrim = joinPath . replaceFirst "Gen" "Prim" . splitDirectories
    modGenModule = joinModule . replaceFirst "Gen" "Prim" . splitModule
    splitModule xs = case break ('.' ==) $ dropWhile ('.' ==) xs of
                      ("", _) -> []
                      (s, ss) -> s : splitModule ss
    joinModule = intercalate "."


-- | For each instance of DefineFunction I need to get Expression to run code generation and def generation.
--   Real output types are: (String, OrderedList String) , String
genFunCalls :: Dec -> Maybe (Exp, Exp)
genFunCalls (InstanceD
#if MIN_VERSION_template_haskell(2,11,0)
  _
#endif
  _ (AppT (AppT (AppT _ t@(LitT (StrTyLit s))) _) _) _) = Just
    ( -- genCode "myFunc" (code (FName :: FName "firstFun"))
      VarE 'genCode `AppE` LitE (StringL s) `AppE` co
      -- genImportPrim "myFunc" (code (FName :: FName "firstFun"))
    , VarE 'genImportPrim `AppE` LitE (StringL s) `AppE` co
    )
  where
    fName = SigE (ConE 'FName) (AppT (ConT ''FName) t)
    co = VarE 'code `AppE` fName
genFunCalls _ = Nothing

-- | Write a fimctopm to put into LlvmModule.moduleDefgen.
--   writeFile filename $ hsModuleDec modname fundefs
genWriteModule :: String -- ^ filename
               -> String -- ^ modulename
               -> Exp -- ^ fun defs
               -> Exp -- ^ IO ()
genWriteModule fname modname fundefs = VarE 'writeFile `AppE` LitE (StringL fname) `AppE`
    ( VarE 'hsModuleDec `AppE` LitE (StringL modname) `AppE` fundefs )

-- | Write a fimctopm to put into LlvmModule.moduleDefgen.
--   writeFile filename $ hsModuleDec modname fundefs
genWriteLlvm :: String -- ^ filename
             -> Exp -- ^ fun code ([(String, OrderedList String)])
             -> Exp -- ^ IO ()
genWriteLlvm fname funcodes = VarE 'writeFile `AppE` LitE (StringL fname) `AppE`
    ( VarE 'llvmFile `AppE` funcodes )


genLlvmModuleInstance :: String -- ^ modulename
                      -> Exp -- ^ IO () genWriteModule
                      -> Exp -- ^ IO () genWriteLlvm
                      -> Dec -- ^ instance LlvmModule (name::Symbol) where
genLlvmModuleInstance modname writeModule writeLlvm =
  InstanceD
#if MIN_VERSION_template_haskell(2,11,0)
    Nothing
#endif
    [] (ConT ''LlvmModule `AppT` LitT (StrTyLit modname))
    [ FunD 'moduleCodegen [Clause [WildP] (NormalB writeModule) []]
    , FunD 'moduleDefgen  [Clause [WildP] (NormalB writeLlvm) []]
    ]



filterJust :: [Maybe a] -> [a]
filterJust (Nothing:xs) = filterJust xs
filterJust (Just x :xs) = x : filterJust xs
filterJust [] = []

-- | code of prim file
hsModuleDec :: String -- ^ moduleName
            -> [String] -- ^ fun defs
            -> String -- Content of the file
hsModuleDec mname funs = unlines $
    [ "{-# LANGUAGE MagicHash, UnboxedTuples #-}"
    , "{-# LANGUAGE GHCForeignImportPrim #-}"
    , "{-# LANGUAGE UnliftedFFITypes #-}"
    , "{-# OPTIONS_HADDOCK hide #-}"
    , ""
    , "module " ++ mname ++ " where"
    , ""
    , "import GHC.Exts"
    , ""
    ] ++ funs

-- | code of llvm generated functions
llvmFile :: [(String, OrderedList String)] -> String
llvmFile xs = unlines
            ( targetDataLayout
            : targetTriple
            : ""
            : fbodies )
            ++ combineDeps reqdecs
  where
    (fbodies, reqdecs) = unzip xs

