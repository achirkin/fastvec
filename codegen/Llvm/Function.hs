{-# LANGUAGE TypeOperators, GADTs #-}
{-# LANGUAGE KindSignatures, DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Llvm.Function
-- Copyright   :  Copyright (C) 2015 Artem M. Chirkin <chirkin@arch.ethz.ch>
-- License     :  BSD3
--
-- Maintainer  :  Artem M. Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  Experimental
--
--
-----------------------------------------------------------------------------

module Llvm.Function
  ( FInfo (..), LlvmFunctionInputs (..), LlvmFunctionOutputs (..), LlvmFunction
  , genCode, ireg -- , genImportPrim
  , VarArg (..), popArgs, pushArgs
  ) where

import GHC.TypeLits
import Llvm.BaseClass
import Llvm.Numbers
import Llvm.Expression
import Llvm.ProgramMonad
import Unsafe.Coerce (unsafeCoerce)
import Control.Arrow (Arrow(..))

import Data.Monoid

--class LlvmFunction inputs outputs where
--  genCode :: String -> (inputs -> E outputs) -> String

data FInfo = FInfo
  { baseReg :: HsPointer
  , stackPointer :: HsPointer
  , heapPointer :: HsPointer
  , stackLimit  :: HsWord
  }

class LlvmFunctionInputs x where
  genInput :: String -> L (x, FInfo)
--  -- | Input signature, e.g. FloatX4# -> FloatX4# ->
--  --   Must not depend on actual value (i.e. must allow bottom)
--  hsInput :: x -> String

class LlvmFunctionOutputs x where
  genOutput :: FInfo -> x -> L ()
--  -- | Input signature, e.g. (# FloatX4#, FloatX4# #)
--  --   Must not depend on actual value (i.e. must allow bottom)
--  hsOutput :: x -> String

--genImportPrim :: LlvmFunction inputs outputs
--              => String -> (inputs -> outputs) -> String
--genImportPrim fname f = "foreign import prim \"" ++ fname ++ "\" "
--      ++ fname ++ "# :: " ++ hsInput x ++ hsOutput y
--  where
--    x = getArgType f
--    y = f x

--getArgType :: (a -> b) -> a
--getArgType _ = undefined

genCode :: LlvmFunction inputs outputs
        => String -> (inputs -> outputs) -> (String, OrderedList String)
genCode fname f = runL eprog $ do
  (args, finfo) <- genInput fname
  genOutput finfo (f args)

ireg :: HsInt
ireg = undef

type LlvmFunction inputs outputs = (LlvmFunctionInputs inputs, LlvmFunctionOutputs outputs, LlvmCode outputs)


data VarArg (n::Nat) x where
  NoArgs :: VarArg 0 x
  (:&)   :: x -> VarArg (n-1) x -> VarArg n x

instance Functor (VarArg n) where
  fmap _ NoArgs = NoArgs
  fmap f (x :& xs) = f x :& fmap f xs

instance Foldable (VarArg n) where
  foldMap _ NoArgs = mempty
  foldMap f (x :& xs) = f x <> foldMap f xs

instance Traversable (VarArg n) where
  traverse = traverse'

traverse' :: Applicative f => (a -> f b) -> VarArg n a -> f (VarArg n b)
traverse' _ NoArgs = pure NoArgs
traverse' f (x :& xs) = (:&) <$> f x <*> traverse' f xs


popArgs :: (LlvmCode a, KnownNat n) => Pointer HsWord -> L (VarArg n a, Pointer HsWord)
popArgs ptr = rez
  where
    rez = popArgs' n ptr
    n = fromInteger . natVal $ eFst rez

popArgs' :: LlvmCode a => Int -> Pointer HsWord -> L (VarArg n a, Pointer HsWord)
popArgs' 0 ptr = return (unsafeCoerce NoArgs, ptr)
popArgs' n ptr = popStack ptr >>= \(x, p) -> first (x :&) <$> popArgs' (n-1) p

eFst :: L (VarArg n a, b) -> Dummy n
eFst _ = Dummy

data Dummy (n::Nat) = Dummy


pushArgs :: LlvmCode a => Pointer HsWord -> VarArg n a -> L (Pointer HsWord)
pushArgs ptr (x :& xs) = pushStack ptr x >>= flip pushArgs xs
pushArgs ptr NoArgs = return ptr
