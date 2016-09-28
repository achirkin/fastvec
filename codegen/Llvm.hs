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
  , LlvmFunDef (..), genDefs
--  , runTH, targetDataLayout, targetTriple
  ) where



--import Control.Monad
--import Control.Applicative
--import GHC.TypeLits

--import qualified Data.IntSet as Set
--import Data.Text as T
import Data.Monoid
--import Data.IORef

--import Data.Int
--import Data.Word
--import Unsafe.Coerce (unsafeCoerce)

import Llvm.BaseClass
import Llvm.ProgramMonad
import Llvm.Numbers
import Llvm.Arch.CodeAMD64 ()
import Llvm.Expression
import Llvm.Function

import Control.Arrow (second)
import Language.Haskell.TH
import System.FilePath
import Platform
--import System.Directory


data LlvmFunDef = forall inputs outputs . LlvmFunction inputs outputs => LFDef String (inputs -> outputs)

-- | Generate LLVM IR file with a list of functions
genDefs :: [LlvmFunDef] -> Q [Dec]
genDefs xs = do
  filename <- modName . loc_filename <$> location
  runIO . writeFile filename $
          unlines
            ( targetDataLayout
            : targetTriple
            : ""
            : fbodies )
          <> combineDeps reqdecs
  runIO $ putStrLn filename
  return []
  where
    (fbodies, reqdecs) = unzip $ fmap (\(LFDef name f) -> genCode name f) xs
    modName = uncurry (</>) .  second ((-<.> ".ll") . ("codegen_" ++)) . splitFileName

--runTH :: LlvmFunction inputs outputs => String -> (inputs -> E outputs) -> Q [Dec]
--runTH funname f = do
--  filename <- modName . loc_filename <$> location
--  let (fbody, reqdecs) = genCode funname f
--  runIO . writeFile filename $
--          unlines
--            [ targetDataLayout
--            , targetTriple
--            , ""
--            ]
--          <> fbody
--          <> combineDeps [reqdecs]
--  runIO $ putStrLn filename
--  return []
--  where
--    modName = uncurry (</>) .  second ((-<.> ".ll") . ("codegen_" ++)) . splitFileName

