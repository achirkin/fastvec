{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
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
  , genCode, ireg, genImportPrim
  , VarArg (..), popArgs, pushArgs
  -- actual definitions
  , FName (..), funcName, DefineFunction (..)
  , LlvmModule (..)
  ) where

import GHC.TypeLits
import Llvm.BaseClass
import Llvm.Numbers
import Llvm.Expression
import Llvm.ProgramMonad
import Unsafe.Coerce (unsafeCoerce)
import Control.Arrow (Arrow(..))
import Data.List (intercalate)
import Data.Monoid

data FInfo = FInfo
  { baseReg :: HsPointer
  , stackPointer :: HsPointer
  , heapPointer :: HsPointer
  , stackLimit  :: HsWord
  }

class HsFunctionInputs x => LlvmFunctionInputs x where
  genInput :: String -> L (x, FInfo)

class HsFunctionOutputs x => LlvmFunctionOutputs x where
  genOutput :: FInfo -> x -> L ()


class HsFunctionInputs x where
  -- | Input signature, e.g. FloatX4# -> FloatX4# ->
  --   Must not depend on actual value (i.e. must allow bottom)
  hsInput :: x -> String

class HsFunctionOutputs x where
  -- | Input signature, e.g. (# FloatX4#, FloatX4# #)
  --   Must not depend on actual value (i.e. must allow bottom)
  hsOutput :: x -> String


-- | Generate "foreign import prim" declaration in Haskell
genImportPrim :: LlvmFunction inputs outputs
              => String -> (inputs -> outputs) -> String
genImportPrim fn f = "foreign import prim \"" ++ fn ++ "\" "
      ++ fn ++ "# :: " ++ hsInput x ++ hsOutput y
  where
    x = getArgType f
    y = f x

getArgType :: (a -> b) -> a
getArgType _ = undefined

-- | Generate function implementation in LLVM IR
genCode :: LlvmFunction inputs outputs
        => String -> (inputs -> outputs) -> (String, OrderedList String)
genCode fn f = runL eprog $ do
  (args, finfo) <- genInput fn
  genOutput finfo (f args)

ireg :: HsInt
ireg = undef

type LlvmFunction inputs outputs = (LlvmFunctionInputs inputs, LlvmFunctionOutputs outputs)


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

vproxy :: VarArg n a -> Dummy n
vproxy _ = Dummy

argNum :: KnownNat n => VarArg n a -> Int
argNum = fromInteger . natVal . vproxy

pushArgs :: LlvmCode a => Pointer HsWord -> VarArg n a -> L (Pointer HsWord)
pushArgs ptr (x :& xs) = pushStack ptr x >>= flip pushArgs xs
pushArgs ptr NoArgs = return ptr




data FName (name::Symbol) = FName

funcName :: KnownSymbol name => FName name -> String
funcName = symbolVal




class LlvmFunction inputs outputs => DefineFunction (name::Symbol) inputs outputs | name -> inputs, name -> outputs where
  -- | Use this function to type in EDSL to be compiled into LLVM IR.
  --   First parameter is used only for a type inference
  code :: FName name -> inputs -> outputs



-- | Helper typeclass -- instances generated in genDefs TH function.
--   Executing function defined in instances does actual codegen job.
class LlvmModule (name::Symbol) where
  moduleCodegen :: FName name -> IO ()
  moduleDefgen  :: FName name -> IO ()








enforceType :: a -> a -> a
enforceType _ = id


instance (KnownNat n, LlvmNum t b)
         => HsFunctionInputs ( E (Numbers t b n)) where
  hsInput args = t
    where
      x = broadcast (NConst 0)
      ex = OpConst x
      _cast = enforceType args (ex)
      t = hsNumberType x ++ " -> "

instance (KnownNat n, LlvmNum t b)
         => HsFunctionInputs ( E (Numbers t b n), E (Numbers t b n)) where
  hsInput args = concat (replicate 2 t)
    where
      x = broadcast (NConst 0)
      ex = OpConst x
      _cast = enforceType args (ex,ex)
      t = hsNumberType x ++ " -> "

instance (KnownNat n, LlvmNum t b)
         => HsFunctionInputs ( E (Numbers t b n), E (Numbers t b n), E (Numbers t b n)) where
  hsInput args = concat (replicate 3 t)
    where
      x = broadcast (NConst 0)
      ex = OpConst x
      _cast = enforceType args (ex,ex,ex)
      t = hsNumberType x ++ " -> "

instance (KnownNat n, LlvmNum t b)
         => HsFunctionInputs ( E (Numbers t b n), E (Numbers t b n), E (Numbers t b n), E (Numbers t b n)) where
  hsInput args = concat (replicate 4 t)
    where
      x = broadcast (NConst 0)
      ex = OpConst x
      _cast = enforceType args (ex,ex,ex,ex)
      t = hsNumberType x ++ " -> "

instance (KnownNat n, LlvmNum t b)
         => HsFunctionInputs ( E (Numbers t b n), E (Numbers t b n), E (Numbers t b n), E (Numbers t b n), E (Numbers t b n)) where
  hsInput args = concat (replicate 5 t)
    where
      x = broadcast (NConst 0)
      ex = OpConst x
      _cast = enforceType args (ex,ex,ex,ex,ex)
      t = hsNumberType x ++ " -> "

instance (KnownNat n, LlvmNum t b)
         => HsFunctionInputs ( E (Numbers t b n), E (Numbers t b n), E (Numbers t b n), E (Numbers t b n), E (Numbers t b n), E (Numbers t b n)) where
  hsInput args = concat (replicate 6 t)
    where
      x = broadcast (NConst 0)
      ex = OpConst x
      _cast = enforceType args (ex,ex,ex,ex,ex,ex)
      t = hsNumberType x ++ " -> "

instance (KnownNat n, KnownNat k, LlvmNum t b)
         => HsFunctionInputs ( E (Numbers t b n), E (Numbers t b n), E (Numbers t b n), E (Numbers t b n), E (Numbers t b n), E (Numbers t b n)
                             , E (Numbers t b k)) where
  hsInput args = concat (replicate (6 + k) t)
    where
      x = broadcast (NConst 0)
      ex = OpConst x
      y = broadcast (NConst 0)
      ey = OpConst y
      k = dim y `div` dim x
      _cast = enforceType args (ex,ex,ex,ex,ex,ex,ey)
      t = hsNumberType x ++ " -> "

instance (KnownNat n, KnownNat k, LlvmNum t b)
         => HsFunctionInputs ( E (Numbers t b n), E (Numbers t b n), E (Numbers t b n), E (Numbers t b n), E (Numbers t b n), E (Numbers t b n)
                             , VarArg k (E (Numbers t b n))) where
  hsInput args = concat (replicate (6 + k) t)
    where
      x = broadcast (NConst 0)
      ex = OpConst x
      k = argNum va
      (_,_,_,_,_,_,va) = enforceType args (ex,ex,ex,ex,ex,ex,undefined)
      t = hsNumberType x ++ " -> "



instance (KnownNat n, LlvmNum t b)
         => HsFunctionOutputs ( E (Numbers t b n)) where
  hsOutput args = t
    where
      x = broadcast (NConst 0)
      ex = OpConst x
      _cast = enforceType args (ex)
      t = hsNumberType x

instance (KnownNat n, LlvmNum t b)
         => HsFunctionOutputs ( E (Numbers t b n), E (Numbers t b n)) where
  hsOutput args = "(# " ++ (intercalate ", " $ replicate 2 t) ++ " #)"
    where
      x = broadcast (NConst 0)
      ex = OpConst x
      _cast = enforceType args (ex,ex)
      t = hsNumberType x

instance (KnownNat n, LlvmNum t b)
         => HsFunctionOutputs ( E (Numbers t b n), E (Numbers t b n), E (Numbers t b n)) where
  hsOutput args = "(# " ++ (intercalate ", " $ replicate 3 t) ++ " #)"
    where
      x = broadcast (NConst 0)
      ex = OpConst x
      _cast = enforceType args (ex,ex,ex)
      t = hsNumberType x

instance (KnownNat n, LlvmNum t b)
         => HsFunctionOutputs ( E (Numbers t b n), E (Numbers t b n), E (Numbers t b n), E (Numbers t b n)) where
  hsOutput args = "(# " ++ (intercalate ", " $ replicate 4 t) ++ " #)"
    where
      x = broadcast (NConst 0)
      ex = OpConst x
      _cast = enforceType args (ex,ex,ex,ex)
      t = hsNumberType x

instance (KnownNat n, LlvmNum t b)
         => HsFunctionOutputs ( E (Numbers t b n), E (Numbers t b n), E (Numbers t b n), E (Numbers t b n), E (Numbers t b n)) where
  hsOutput args = "(# " ++ (intercalate ", " $ replicate 5 t) ++ " #)"
    where
      x = broadcast (NConst 0)
      ex = OpConst x
      _cast = enforceType args (ex,ex,ex,ex,ex)
      t = hsNumberType x

instance (KnownNat n, LlvmNum t b)
         => HsFunctionOutputs ( E (Numbers t b n), E (Numbers t b n), E (Numbers t b n), E (Numbers t b n), E (Numbers t b n), E (Numbers t b n)) where
  hsOutput args = "(# " ++ (intercalate ", " $ replicate 6 t) ++ " #)"
    where
      x = broadcast (NConst 0)
      ex = OpConst x
      _cast = enforceType args (ex,ex,ex,ex,ex,ex)
      t = hsNumberType x

instance (KnownNat n, KnownNat k, LlvmNum t b)
         => HsFunctionOutputs ( E (Numbers t b n), E (Numbers t b n), E (Numbers t b n), E (Numbers t b n), E (Numbers t b n), E (Numbers t b n)
                              , E (Numbers t b k)) where
  hsOutput args = "(# " ++ (intercalate ", " $ replicate (6 + k) t) ++ " #)"
    where
      x = broadcast (NConst 0)
      ex = OpConst x
      y = broadcast (NConst 0)
      ey = OpConst y
      k = dim y `div` dim x
      _cast = enforceType args (ex,ex,ex,ex,ex,ex,ey)
      t = hsNumberType x

instance (KnownNat n, KnownNat k, LlvmNum t b)
         => HsFunctionOutputs ( E (Numbers t b n), E (Numbers t b n), E (Numbers t b n), E (Numbers t b n), E (Numbers t b n), E (Numbers t b n)
                              , VarArg k (E (Numbers t b n))) where
  hsOutput args = "(# " ++ (intercalate ", " $ replicate (6 + k) t) ++ " #)"
    where
      x = broadcast (NConst 0)
      ex = OpConst x
      k = argNum va
      (_,_,_,_,_,_,va) = enforceType args (ex,ex,ex,ex,ex,ex,undefined)
      t = hsNumberType x
