{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies, GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RecursiveDo #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Llvm.Expression
-- Copyright   :  Copyright (C) 2015 Artem M. Chirkin <chirkin@arch.ethz.ch>
-- License     :  BSD3
--
-- Maintainer  :  Artem M. Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  Experimental
--
--
-----------------------------------------------------------------------------

module Llvm.Expression where

import GHC.TypeLits
import Data.Bits

import Llvm.BaseClass
import Llvm.ProgramMonad
import Llvm.Numbers
import Data.Monoid ((<>))



data E t where
  OpConst    :: LlvmCode a => a -> E a
  OpFadd     :: (NumberCategory t ~ 'CatFP, LlvmCode (Numbers t bits n))  => E (Numbers t bits n) -> E (Numbers t bits n) -> E (Numbers t bits n)
  OpFsub     :: (NumberCategory t ~ 'CatFP, LlvmCode (Numbers t bits n))  => E (Numbers t bits n) -> E (Numbers t bits n) -> E (Numbers t bits n)
  OpFmul     :: (NumberCategory t ~ 'CatFP, LlvmCode (Numbers t bits n))  => E (Numbers t bits n) -> E (Numbers t bits n) -> E (Numbers t bits n)
  OpAdd      :: (NumberCategory t ~ 'CatInt, LlvmCode (Numbers t bits n)) => E (Numbers t bits n) -> E (Numbers t bits n) -> E (Numbers t bits n)
  OpSub      :: (NumberCategory t ~ 'CatInt, LlvmCode (Numbers t bits n)) => E (Numbers t bits n) -> E (Numbers t bits n) -> E (Numbers t bits n)
  OpMul      :: (NumberCategory t ~ 'CatInt, LlvmCode (Numbers t bits n)) => E (Numbers t bits n) -> E (Numbers t bits n) -> E (Numbers t bits n)

  OpLoad     :: LlvmCode a => E (Pointer a) -> E a
  OpStore    :: (LlvmCode a, LlvmType b) => E (Pointer b) -> E a -> E ()

  OpInttoptr :: (LlvmType a, NumberCategory t ~ 'CatInt, LlvmCode (Numbers t bits 1)) => E (Numbers t bits 1) -> E (Pointer a)

  OpLL       :: L a -> E a

  OpBitcast  :: (LlvmCode (Numbers t1 bits1 n1), (bits1*n1) ~ (bits2*n2)) => E (Numbers t1 bits1 n1) -> E (Numbers t2 bits2 n2)
  OpBitcastP :: (LlvmType a, LlvmType b) => E (Pointer a) -> E (Pointer b)

  OpAnd      :: (NumberCategory t ~ 'CatInt, LlvmCode (Numbers t bits n)) => E (Numbers t bits n) -> E (Numbers t bits n) -> E (Numbers t bits n)
  OpOr       :: (NumberCategory t ~ 'CatInt, LlvmCode (Numbers t bits n)) => E (Numbers t bits n) -> E (Numbers t bits n) -> E (Numbers t bits n)
  OpXor      :: (NumberCategory t ~ 'CatInt, LlvmCode (Numbers t bits n)) => E (Numbers t bits n) -> E (Numbers t bits n) -> E (Numbers t bits n)
  OpShl      :: (NumberCategory t ~ 'CatInt, LlvmCode (Numbers t bits n)) => E (Numbers t bits n) -> E (Numbers t bits n) -> E (Numbers t bits n)
  OpLshr     :: (NumberCategory t ~ 'CatInt, LlvmCode (Numbers t bits n)) => E (Numbers t bits n) -> E (Numbers t bits n) -> E (Numbers t bits n)
  OpAshr     :: (NumberCategory t ~ 'CatInt, LlvmCode (Numbers t bits n)) => E (Numbers t bits n) -> E (Numbers t bits n) -> E (Numbers t bits n)
  OpSext     :: ( NumberCategory t ~ 'CatInt
                , LlvmCode (Numbers t bits1 n1)
                , LlvmCode (Numbers t bits2 n2)
                , bits1 <= bits2
                , n1 <= n2
                ) => E (Numbers t bits1 n1) -> E (Numbers t bits2 n2)
  OpZext     :: ( NumberCategory t ~ 'CatInt
                , LlvmCode (Numbers t bits1 n1)
                , LlvmCode (Numbers t bits2 n2)
                , bits1 <= bits2
                , n1 <= n2
                ) => E (Numbers t bits1 n1) -> E (Numbers t bits2 n2)
  OpIcmp     :: (NumberCategory t ~ 'CatInt
                , LlvmCode (Numbers t bits n)
                ) => LlvmCmp -> E (Numbers t bits n) -> E (Numbers t bits n) -> E (INumbers (NumberIsSigned t) 1 n)

  OpGetelemptr :: LlvmType a => E (Pointer a) -> E HsInt -> E (Pointer a)



data LlvmCmp
  = LC_EQ
  | LC_NE
  | LC_UGT
  | LC_UGE
  | LC_ULT
  | LC_ULE
  | LC_SGT
  | LC_SGE
  | LC_SLT
  | LC_SLE

instance Show LlvmCmp where
  show LC_EQ  = "eq"
  show LC_NE  = "ne"
  show LC_UGT = "ugt"
  show LC_UGE = "uge"
  show LC_ULT = "ult"
  show LC_ULE = "ule"
  show LC_SGT = "sgt"
  show LC_SGE = "sge"
  show LC_SLT = "slt"
  show LC_SLE = "sle"

undefinedContent :: E t -> t
undefinedContent _ =  undefined


compileE :: LlvmCode a => E a -> L a

compileE (OpConst x) = pure x
compileE (OpFadd ex ey) = do
  x <- compileE ex
  y <- compileE ey
  op $ "fadd " <> lTypVal x <> ", " <> lcode y
compileE (OpFsub ex ey) = do
  x <- compileE ex
  y <- compileE ey
  op $ "fsub " <> lTypVal x <> ", " <> lcode y
compileE (OpFmul ex ey) = do
  x <- compileE ex
  y <- compileE ey
  op $ "fmul " <> lTypVal x <> ", " <> lcode y
compileE (OpAdd ex ey) = do
  x <- compileE ex
  y <- compileE ey
  op $ "add " <> lTypVal x <> ", " <> lcode y
compileE (OpSub ex ey) = do
  x <- compileE ex
  y <- compileE ey
  op $ "sub " <> lTypVal x <> ", " <> lcode y
compileE (OpMul ex ey) = do
  x <- compileE ex
  y <- compileE ey
  op $ "mul " <> lTypVal x <> ", " <> lcode y

#if (__GLASGOW_HASKELL_LLVM__ >= (3,7) && __GLASGOW_HASKELL_LLVM__ < (5,9)) || __GLASGOW_HASKELL_LLVM__ >= 37
compileE (OpLoad eptr) = mdo
  ptr <- compileE eptr
  rez <- op $ "load " <> ltype rez <> ", " <> lTypVal ptr <> ", align " <> show stackAlign
  return rez
#else
compileE (OpLoad eptr) = do
  ptr <- compileE eptr
  op $ "load " <> lTypVal ptr <> ", align " <> show stackAlign
#endif
compileE (OpStore eptr ev) = do
  ptr <- compileE eptr
  val <- compileE ev
  op $ "store " <> lTypVal val <> ", " <> lTypVal ptr <> ", align " <> show stackAlign

compileE e@(OpInttoptr ex) = do
  x <- compileE ex
  op $ "inttoptr " <> lTypVal x <> " to " <> ltype (undefinedContent e)



compileE (OpLL a) = a

compileE e@(OpBitcast ex) = do
  x <- compileE ex
  op $ "bitcast " <> lTypVal x <> " to " <> ltype (undefinedContent e)
compileE e@(OpBitcastP ex) = do
  x <- compileE ex
  op $ "bitcast " <> lTypVal x <> " to " <> ltype (undefinedContent e)

compileE (OpAnd ex ey) = do
  x <- compileE ex
  y <- compileE ey
  op $ "and " <> lTypVal x <> ", " <> lcode y
compileE (OpOr ex ey) = do
  x <- compileE ex
  y <- compileE ey
  op $ "or " <> lTypVal x <> ", " <> lcode y
compileE (OpXor ex ey) = do
  x <- compileE ex
  y <- compileE ey
  op $ "xor " <> lTypVal x <> ", " <> lcode y
compileE (OpShl ex ey) = do
  x <- compileE ex
  y <- compileE ey
  op $ "shl " <> lTypVal x <> ", " <> lcode y
compileE (OpLshr ex ey) = do
  x <- compileE ex
  y <- compileE ey
  op $ "lshr " <> lTypVal x <> ", " <> lcode y
compileE (OpAshr ex ey) = do
  x <- compileE ex
  y <- compileE ey
  op $ "ashr " <> lTypVal x <> ", " <> lcode y


compileE e@(OpSext ex) = do
  x <- compileE ex
  op $ "sext " <> lTypVal x <> " to " <> ltype (undefinedContent e)
compileE e@(OpZext ex) = do
  x <- compileE ex
  op $ "zext " <> lTypVal x <> " to " <> ltype (undefinedContent e)


compileE (OpIcmp cmpt ex ey) = do
  x <- compileE ex
  y <- compileE ey
  op $ "icmp " <> show cmpt <> " " <> lTypVal x <> ", " <> lcode y



compileE (OpGetelemptr ex ei) = do
  x <- compileE ex
  i <- compileE ei
#if (__GLASGOW_HASKELL_LLVM__ >= (3,7) && __GLASGOW_HASKELL_LLVM__ < (5,9)) || __GLASGOW_HASKELL_LLVM__ >= 37
  op $ "getelementptr inbounds " <> ltype i <> ", " <> lTypVal x <> ", " <> lTypVal i
#else
  op $ "getelementptr inbounds " <> lTypVal x <> ", " <> lTypVal i
#endif



getReturnPointer :: LlvmArgTypes a => Pointer HsWord -> L (Pointer (a -> ()))
#if (__GLASGOW_HASKELL_LLVM__ >= (3,7) && __GLASGOW_HASKELL_LLVM__ < (5,9)) || __GLASGOW_HASKELL_LLVM__ >= 37
getReturnPointer sp = do
  iptr   <- compileE $ OpBitcastP (OpConst sp)
  compileE $ OpLoad (OpConst iptr)
#else
getReturnPointer sp = do
  iptr   <- compileE $ OpLoad (OpConst sp)
  compileE $ OpInttoptr (OpConst iptr)
#endif

-- | Pop value from a stack and update a pointer position
popStack :: LlvmCode a => Pointer HsWord -> L (a , Pointer HsWord)
popStack sp = do
  iptr   <- compileE $ OpBitcastP esp
  val <- compileE $ OpLoad (OpConst iptr)
  sp' <- compileE . OpGetelemptr esp . opConst . fromIntegral . negate $ (negate $ lsize val) `div` stackAlign
  return (val, sp')
  where
    esp = OpConst sp

-- | Push value to a stack and update a pointer position
pushStack :: LlvmCode a => Pointer HsWord -> a -> L (Pointer HsWord)
pushStack sp x = do
  compileE $ OpStore esp (OpConst x)
  compileE . OpGetelemptr esp . opConst . fromIntegral $ (negate $ lsize x) `div` stackAlign
  where
    esp = OpConst sp

opConst :: ( LlvmNum t bits
           , KnownNat n
           , 1 <= n
           )
        => NVal t bits -> E (Numbers t bits n)
opConst = OpConst . broadcast . NConst

instance ( LlvmNum 'NT_FP bits
         , LlvmNum 'NT_INT bits
         , LlvmFloating bits
         , Num (E (Numbers 'NT_INT bits n))
         , KnownNat n
         , 1 <= n
         , 1 <= bits) => Num (E (Numbers 'NT_FP bits n)) where
  (+) = OpFadd
  (-) = OpFsub
  (*) = OpFmul
  abs ex = OpBitcast (abs iex)
    where
      iex = OpBitcast ex :: E (Numbers 'NT_INT bits n)
  signum x = OpBitcast (OpAnd iw iz)
    where
      ix = OpBitcast x :: E (Numbers 'NT_INT bits n)
      iy = OpAnd ix (opConst $ coerceToInt (-0)) -- get sign
      iz = OpOr  iy (opConst $ coerceToInt 1)   -- get unsigned 1
      iw = OpSub (OpZext (OpIcmp LC_EQ ix (opConst 0))) 1 -- get if x==0 then 1111.. else 0000..
  fromInteger = opConst . fromInteger

instance ( LlvmNum 'NT_INT bits
         , LlvmIntegral 'Signed bits
         , KnownNat n
         , 1 <= n
         , 1 <= bits) => Num (E (Numbers 'NT_INT bits n)) where
  (+) = OpAdd
  (-) = OpSub
  (*) = OpMul
  abs x = OpAnd x (opConst $ complement (rotateR 1 1))
  signum x = OpOr (OpSext neg) (OpZext pos)
    where
      neg = OpIcmp LC_SLT x (opConst $ 0)
      pos = OpIcmp LC_SGT x (opConst $ 0)
  fromInteger = opConst . fromInteger

instance ( LlvmNum 'NT_WORD bits
         , KnownNat n
         , 1 <= n
         , 1 <= bits) => Num (E (Numbers 'NT_WORD bits n)) where
  (+) = OpAdd
  (-) = OpSub
  (*) = OpMul
  abs = id
  signum x = OpZext notzero
    where
      notzero = OpIcmp LC_NE x (opConst 0)
  fromInteger = opConst . fromInteger



