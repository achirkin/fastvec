{-# LANGUAGE ConstraintKinds, TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Llvm.Arch.CodeAMD64
-- Copyright   :  Copyright (C) 2015 Artem M. Chirkin <chirkin@arch.ethz.ch>
-- License     :  BSD3
--
-- Maintainer  :  Artem M. Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  Experimental
--
--
-----------------------------------------------------------------------------

module Llvm.Arch.CodeAMD64 () where

import GHC.TypeLits

import Llvm.BaseClass
import Llvm.ProgramMonad
import Llvm.Numbers
import Llvm.Expression
import Llvm.Function


#ifdef USING_VECTORS_AVX512

----------------------------------------------------------------------------------------------------
-- 8 x 64
----------------------------------------------------------------------------------------------------

instance AvxConstraint t 8 => LlvmFunctionInputs (E (Numbers t 8 64)) where
  genInput fname = do
    (br,sp,hp,_r1::HsInt,_r2::HsInt,_r3::HsInt,_r4::HsInt,_r5::HsInt,_r6::HsInt,spLim,a) <- define fname
    return (OpConst a, FInfo br sp hp spLim)

instance AvxConstraint t 8 => LlvmFunctionInputs (E (Numbers t 8 64), E (Numbers t 8 64)) where
  genInput fname = do
    (br,sp,hp,_r1::HsInt,_r2::HsInt,_r3::HsInt,_r4::HsInt,_r5::HsInt,_r6::HsInt,spLim,a,b) <- define fname
    return ((OpConst a, OpConst b), FInfo br sp hp spLim)

instance AvxConstraint t 8 => LlvmFunctionInputs (E (Numbers t 8 64), E (Numbers t 8 64), E (Numbers t 8 64)) where
  genInput fname = do
    (br,sp,hp,_r1::HsInt,_r2::HsInt,_r3::HsInt,_r4::HsInt,_r5::HsInt,_r6::HsInt,spLim,a,b,c) <- define fname
    return ((OpConst a, OpConst b, OpConst c), FInfo br sp hp spLim)

instance AvxConstraint t 8 => LlvmFunctionInputs (E (Numbers t 8 64), E (Numbers t 8 64), E (Numbers t 8 64), E (Numbers t 8 64)) where
  genInput fname = do
    (br,sp,hp,_r1::HsInt,_r2::HsInt,_r3::HsInt,_r4::HsInt,_r5::HsInt,_r6::HsInt,spLim,a,b,c,d) <- define fname
    return ((OpConst a, OpConst b, OpConst c, OpConst d), FInfo br sp hp spLim)

instance AvxConstraint t 8 => LlvmFunctionInputs (E (Numbers t 8 64), E (Numbers t 8 64), E (Numbers t 8 64), E (Numbers t 8 64), E (Numbers t 8 64)) where
  genInput fname = do
    (br,sp,hp,_r1::HsInt,_r2::HsInt,_r3::HsInt,_r4::HsInt,_r5::HsInt,_r6::HsInt,spLim,a,b,c,d,e) <- define fname
    return ((OpConst a, OpConst b, OpConst c, OpConst d, OpConst e), FInfo br sp hp spLim)

instance AvxConstraint t 8 => LlvmFunctionInputs (E (Numbers t 8 64), E (Numbers t 8 64), E (Numbers t 8 64), E (Numbers t 8 64), E (Numbers t 8 64), E (Numbers t 8 64)) where
  genInput fname = do
    (br,sp,hp,_r1::HsInt,_r2::HsInt,_r3::HsInt,_r4::HsInt,_r5::HsInt,_r6::HsInt,spLim,a,b,c,d,e,f) <- define fname
    return ((OpConst a, OpConst b, OpConst c, OpConst d, OpConst e, OpConst f), FInfo br sp hp spLim)

instance (AvxConstraint t 8, KnownNat n)
         => LlvmFunctionInputs ( E (Numbers t 8 64), E (Numbers t 8 64), E (Numbers t 8 64), E (Numbers t 8 64), E (Numbers t 8 64), E (Numbers t 8 64)
                               , E (Numbers t 8 n)) where
  genInput fname = do
    (br,sp::HsPointer,hp,_r1::HsInt,_r2::HsInt,_r3::HsInt,_r4::HsInt,_r5::HsInt,_r6::HsInt,spLim,a,b,c,d,e,f) <- define fname
    (g,sp') <- popStack (unLabel sp)
    return ((OpConst a, OpConst b, OpConst c, OpConst d, OpConst e, OpConst f, OpConst g), FInfo br (Labeled sp') hp spLim)

instance (AvxConstraint t 8, KnownNat n)
         => LlvmFunctionInputs ( E (Numbers t 8 64), E (Numbers t 8 64), E (Numbers t 8 64), E (Numbers t 8 64), E (Numbers t 8 64), E (Numbers t 8 64)
                               , VarArg n (E (Numbers t 8 64))) where
  genInput fname = do
    (br,sp::HsPointer,hp,_r1::HsInt,_r2::HsInt,_r3::HsInt,_r4::HsInt,_r5::HsInt,_r6::HsInt,spLim,a,b,c,d,e,f) <- define fname
    (args,sp') <- popArgs (unLabel sp)
    return ((OpConst a, OpConst b, OpConst c, OpConst d, OpConst e, OpConst f, fmap OpConst args), FInfo br (Labeled sp') hp spLim)


instance AvxConstraint t 8
         => LlvmFunctionOutputs ( E (Numbers t 8 64)) where
  genOutput (FInfo br sp hp spLim) r' = do
    r <- compileE r'
    retptr   <- getReturnPointer (unLabel sp)
    tailcall retptr (br, sp, hp, ireg, ireg, ireg, ireg, ireg, ireg, spLim, r)

instance AvxConstraint t 8
         => LlvmFunctionOutputs ( E (Numbers t 8 64), E (Numbers t 8 64)) where
  genOutput (FInfo br sp hp spLim) (r1',r2') = do
    (r1,r2) <- (,) <$> compileE r1' <*> compileE r2'
    retptr   <- getReturnPointer (unLabel sp)
    tailcall retptr (br, sp, hp, ireg, ireg, ireg, ireg, ireg, ireg, spLim, r1, r2)

instance AvxConstraint t 8
         => LlvmFunctionOutputs ( E (Numbers t 8 64), E (Numbers t 8 64), E (Numbers t 8 64)) where
  genOutput (FInfo br sp hp spLim) (r1',r2',r3') = do
    (r1,r2,r3) <- (,,) <$> compileE r1' <*> compileE r2' <*> compileE r3'
    retptr   <- getReturnPointer (unLabel sp)
    tailcall retptr (br, sp, hp, ireg, ireg, ireg, ireg, ireg, ireg, spLim, r1, r2, r3)

instance AvxConstraint t 8
         => LlvmFunctionOutputs ( E (Numbers t 8 64), E (Numbers t 8 64), E (Numbers t 8 64), E (Numbers t 8 64)) where
  genOutput (FInfo br sp hp spLim) (r1',r2',r3',r4') = do
    (r1,r2,r3,r4) <- (,,,) <$> compileE r1' <*> compileE r2' <*> compileE r3' <*> compileE r4'
    retptr   <- getReturnPointer (unLabel sp)
    tailcall retptr (br, sp, hp, ireg, ireg, ireg, ireg, ireg, ireg, spLim,r1,r2,r3,r4)

instance AvxConstraint t 8
         => LlvmFunctionOutputs ( E (Numbers t 8 64), E (Numbers t 8 64), E (Numbers t 8 64), E (Numbers t 8 64), E (Numbers t 8 64)) where
  genOutput (FInfo br sp hp spLim) (r1',r2',r3',r4',r5') = do
    (r1,r2,r3,r4,r5) <- (,,,,) <$> compileE r1' <*> compileE r2' <*> compileE r3' <*> compileE r4' <*> compileE r5'
    retptr   <- getReturnPointer (unLabel sp)
    tailcall retptr (br, sp, hp, ireg, ireg, ireg, ireg, ireg, ireg, spLim,r1,r2,r3,r4,r5)

instance AvxConstraint t 8
         => LlvmFunctionOutputs ( E (Numbers t 8 64), E (Numbers t 8 64), E (Numbers t 8 64), E (Numbers t 8 64), E (Numbers t 8 64), E (Numbers t 8 64)) where
  genOutput (FInfo br sp hp spLim) (r1',r2',r3',r4',r5',r6') = do
    (r1,r2,r3,r4,r5,r6) <- (,,,,,) <$> compileE r1' <*> compileE r2' <*> compileE r3' <*> compileE r4' <*> compileE r5' <*> compileE r6'
    retptr   <- getReturnPointer (unLabel sp)
    tailcall retptr (br, sp, hp, ireg, ireg, ireg, ireg, ireg, ireg, spLim,r1,r2,r3,r4,r5,r6)

instance (AvxConstraint t 8, KnownNat n)
         => LlvmFunctionOutputs ( E (Numbers t 8 64), E (Numbers t 8 64), E (Numbers t 8 64), E (Numbers t 8 64), E (Numbers t 8 64), E (Numbers t 8 64)
                                , E (Numbers t 8 n)) where
  genOutput (FInfo br (Labeled sp) hp spLim) (r1',r2',r3',r4',r5',r6',rn') = do
    (r1,r2,r3,r4,r5,r6,rn) <- (,,,,,,) <$> compileE r1' <*> compileE r2' <*> compileE r3' <*> compileE r4' <*> compileE r5' <*> compileE r6' <*> compileE rn'
    sp' <- pushStack sp rn
    retptr <- getReturnPointer sp'
    tailcall retptr (br, sp', hp, ireg, ireg, ireg, ireg, ireg, ireg, spLim,r1,r2,r3,r4,r5,r6)

instance (AvxConstraint t 8, KnownNat n)
         => LlvmFunctionOutputs ( E (Numbers t 8 64), E (Numbers t 8 64), E (Numbers t 8 64), E (Numbers t 8 64), E (Numbers t 8 64), E (Numbers t 8 64)
                                , VarArg n (E (Numbers t 8 64))) where
  genOutput (FInfo br (Labeled sp) hp spLim) (r1',r2',r3',r4',r5',r6',rn') = do
    (r1,r2,r3,r4,r5,r6) <- (,,,,,) <$> compileE r1' <*> compileE r2' <*> compileE r3' <*> compileE r4' <*> compileE r5' <*> compileE r6'
    rn <- mapM compileE rn'
    sp' <- pushArgs sp rn
    retptr <- getReturnPointer sp'
    tailcall retptr (br, sp', hp, ireg, ireg, ireg, ireg, ireg, ireg, spLim,r1,r2,r3,r4,r5,r6)

----------------------------------------------------------------------------------------------------
-- 16 x 32
----------------------------------------------------------------------------------------------------

instance AvxConstraint t 16 => LlvmFunctionInputs (E (Numbers t 16 32)) where
  genInput fname = do
    (br,sp,hp,_r1::HsInt,_r2::HsInt,_r3::HsInt,_r4::HsInt,_r5::HsInt,_r6::HsInt,spLim,a) <- define fname
    return (OpConst a, FInfo br sp hp spLim)

instance AvxConstraint t 16 => LlvmFunctionInputs (E (Numbers t 16 32), E (Numbers t 16 32)) where
  genInput fname = do
    (br,sp,hp,_r1::HsInt,_r2::HsInt,_r3::HsInt,_r4::HsInt,_r5::HsInt,_r6::HsInt,spLim,a,b) <- define fname
    return ((OpConst a, OpConst b), FInfo br sp hp spLim)

instance AvxConstraint t 16 => LlvmFunctionInputs (E (Numbers t 16 32), E (Numbers t 16 32), E (Numbers t 16 32)) where
  genInput fname = do
    (br,sp,hp,_r1::HsInt,_r2::HsInt,_r3::HsInt,_r4::HsInt,_r5::HsInt,_r6::HsInt,spLim,a,b,c) <- define fname
    return ((OpConst a, OpConst b, OpConst c), FInfo br sp hp spLim)

instance AvxConstraint t 16 => LlvmFunctionInputs (E (Numbers t 16 32), E (Numbers t 16 32), E (Numbers t 16 32), E (Numbers t 16 32)) where
  genInput fname = do
    (br,sp,hp,_r1::HsInt,_r2::HsInt,_r3::HsInt,_r4::HsInt,_r5::HsInt,_r6::HsInt,spLim,a,b,c,d) <- define fname
    return ((OpConst a, OpConst b, OpConst c, OpConst d), FInfo br sp hp spLim)

instance AvxConstraint t 16 => LlvmFunctionInputs (E (Numbers t 16 32), E (Numbers t 16 32), E (Numbers t 16 32), E (Numbers t 16 32), E (Numbers t 16 32)) where
  genInput fname = do
    (br,sp,hp,_r1::HsInt,_r2::HsInt,_r3::HsInt,_r4::HsInt,_r5::HsInt,_r6::HsInt,spLim,a,b,c,d,e) <- define fname
    return ((OpConst a, OpConst b, OpConst c, OpConst d, OpConst e), FInfo br sp hp spLim)

instance AvxConstraint t 16 => LlvmFunctionInputs (E (Numbers t 16 32), E (Numbers t 16 32), E (Numbers t 16 32), E (Numbers t 16 32), E (Numbers t 16 32), E (Numbers t 16 32)) where
  genInput fname = do
    (br,sp,hp,_r1::HsInt,_r2::HsInt,_r3::HsInt,_r4::HsInt,_r5::HsInt,_r6::HsInt,spLim,a,b,c,d,e,f) <- define fname
    return ((OpConst a, OpConst b, OpConst c, OpConst d, OpConst e, OpConst f), FInfo br sp hp spLim)

instance (AvxConstraint t 16, KnownNat n)
         => LlvmFunctionInputs ( E (Numbers t 16 32), E (Numbers t 16 32), E (Numbers t 16 32), E (Numbers t 16 32), E (Numbers t 16 32), E (Numbers t 16 32)
                               , E (Numbers t 16 n)) where
  genInput fname = do
    (br,sp::HsPointer,hp,_r1::HsInt,_r2::HsInt,_r3::HsInt,_r4::HsInt,_r5::HsInt,_r6::HsInt,spLim,a,b,c,d,e,f) <- define fname
    (g,sp') <- popStack (unLabel sp)
    return ((OpConst a, OpConst b, OpConst c, OpConst d, OpConst e, OpConst f, OpConst g), FInfo br (Labeled sp') hp spLim)

instance (AvxConstraint t 16, KnownNat n)
         => LlvmFunctionInputs ( E (Numbers t 16 32), E (Numbers t 16 32), E (Numbers t 16 32), E (Numbers t 16 32), E (Numbers t 16 32), E (Numbers t 16 32)
                               , VarArg n (E (Numbers t 16 32))) where
  genInput fname = do
    (br,sp::HsPointer,hp,_r1::HsInt,_r2::HsInt,_r3::HsInt,_r4::HsInt,_r5::HsInt,_r6::HsInt,spLim,a,b,c,d,e,f) <- define fname
    (args,sp') <- popArgs (unLabel sp)
    return ((OpConst a, OpConst b, OpConst c, OpConst d, OpConst e, OpConst f, fmap OpConst args), FInfo br (Labeled sp') hp spLim)


instance AvxConstraint t 16
         => LlvmFunctionOutputs ( E (Numbers t 16 32)) where
  genOutput (FInfo br sp hp spLim) r' = do
    r <- compileE r'
    retptr   <- getReturnPointer (unLabel sp)
    tailcall retptr (br, sp, hp, ireg, ireg, ireg, ireg, ireg, ireg, spLim, r)

instance AvxConstraint t 16
         => LlvmFunctionOutputs ( E (Numbers t 16 32), E (Numbers t 16 32)) where
  genOutput (FInfo br sp hp spLim) (r1',r2') = do
    (r1,r2) <- (,) <$> compileE r1' <*> compileE r2'
    retptr   <- getReturnPointer (unLabel sp)
    tailcall retptr (br, sp, hp, ireg, ireg, ireg, ireg, ireg, ireg, spLim, r1, r2)

instance AvxConstraint t 16
         => LlvmFunctionOutputs ( E (Numbers t 16 32), E (Numbers t 16 32), E (Numbers t 16 32)) where
  genOutput (FInfo br sp hp spLim) (r1',r2',r3') = do
    (r1,r2,r3) <- (,,) <$> compileE r1' <*> compileE r2' <*> compileE r3'
    retptr   <- getReturnPointer (unLabel sp)
    tailcall retptr (br, sp, hp, ireg, ireg, ireg, ireg, ireg, ireg, spLim, r1, r2, r3)

instance AvxConstraint t 16
         => LlvmFunctionOutputs ( E (Numbers t 16 32), E (Numbers t 16 32), E (Numbers t 16 32), E (Numbers t 16 32)) where
  genOutput (FInfo br sp hp spLim) (r1',r2',r3',r4') = do
    (r1,r2,r3,r4) <- (,,,) <$> compileE r1' <*> compileE r2' <*> compileE r3' <*> compileE r4'
    retptr   <- getReturnPointer (unLabel sp)
    tailcall retptr (br, sp, hp, ireg, ireg, ireg, ireg, ireg, ireg, spLim,r1,r2,r3,r4)

instance AvxConstraint t 16
         => LlvmFunctionOutputs ( E (Numbers t 16 32), E (Numbers t 16 32), E (Numbers t 16 32), E (Numbers t 16 32), E (Numbers t 16 32)) where
  genOutput (FInfo br sp hp spLim) (r1',r2',r3',r4',r5') = do
    (r1,r2,r3,r4,r5) <- (,,,,) <$> compileE r1' <*> compileE r2' <*> compileE r3' <*> compileE r4' <*> compileE r5'
    retptr   <- getReturnPointer (unLabel sp)
    tailcall retptr (br, sp, hp, ireg, ireg, ireg, ireg, ireg, ireg, spLim,r1,r2,r3,r4,r5)

instance AvxConstraint t 16
         => LlvmFunctionOutputs ( E (Numbers t 16 32), E (Numbers t 16 32), E (Numbers t 16 32), E (Numbers t 16 32), E (Numbers t 16 32), E (Numbers t 16 32)) where
  genOutput (FInfo br sp hp spLim) (r1',r2',r3',r4',r5',r6') = do
    (r1,r2,r3,r4,r5,r6) <- (,,,,,) <$> compileE r1' <*> compileE r2' <*> compileE r3' <*> compileE r4' <*> compileE r5' <*> compileE r6'
    retptr   <- getReturnPointer (unLabel sp)
    tailcall retptr (br, sp, hp, ireg, ireg, ireg, ireg, ireg, ireg, spLim,r1,r2,r3,r4,r5,r6)

instance (AvxConstraint t 16, KnownNat n)
         => LlvmFunctionOutputs ( E (Numbers t 16 32), E (Numbers t 16 32), E (Numbers t 16 32), E (Numbers t 16 32), E (Numbers t 16 32), E (Numbers t 16 32)
                                , E (Numbers t 16 n)) where
  genOutput (FInfo br (Labeled sp) hp spLim) (r1',r2',r3',r4',r5',r6',rn') = do
    (r1,r2,r3,r4,r5,r6,rn) <- (,,,,,,) <$> compileE r1' <*> compileE r2' <*> compileE r3' <*> compileE r4' <*> compileE r5' <*> compileE r6' <*> compileE rn'
    sp' <- pushStack sp rn
    retptr <- getReturnPointer sp'
    tailcall retptr (br, sp', hp, ireg, ireg, ireg, ireg, ireg, ireg, spLim,r1,r2,r3,r4,r5,r6)

instance (AvxConstraint t 16, KnownNat n)
         => LlvmFunctionOutputs ( E (Numbers t 16 32), E (Numbers t 16 32), E (Numbers t 16 32), E (Numbers t 16 32), E (Numbers t 16 32), E (Numbers t 16 32)
                                , VarArg n (E (Numbers t 16 32))) where
  genOutput (FInfo br (Labeled sp) hp spLim) (r1',r2',r3',r4',r5',r6',rn') = do
    (r1,r2,r3,r4,r5,r6) <- (,,,,,) <$> compileE r1' <*> compileE r2' <*> compileE r3' <*> compileE r4' <*> compileE r5' <*> compileE r6'
    rn <- mapM compileE rn'
    sp' <- pushArgs sp rn
    retptr <- getReturnPointer sp'
    tailcall retptr (br, sp', hp, ireg, ireg, ireg, ireg, ireg, ireg, spLim,r1,r2,r3,r4,r5,r6)


----------------------------------------------------------------------------------------------------
-- 32 x 16
----------------------------------------------------------------------------------------------------

instance AvxConstraint t 32 => LlvmFunctionInputs (E (Numbers t 32 16)) where
  genInput fname = do
    (br,sp,hp,_r1::HsInt,_r2::HsInt,_r3::HsInt,_r4::HsInt,_r5::HsInt,_r6::HsInt,spLim,a) <- define fname
    return (OpConst a, FInfo br sp hp spLim)

instance AvxConstraint t 32 => LlvmFunctionInputs (E (Numbers t 32 16), E (Numbers t 32 16)) where
  genInput fname = do
    (br,sp,hp,_r1::HsInt,_r2::HsInt,_r3::HsInt,_r4::HsInt,_r5::HsInt,_r6::HsInt,spLim,a,b) <- define fname
    return ((OpConst a, OpConst b), FInfo br sp hp spLim)

instance AvxConstraint t 32 => LlvmFunctionInputs (E (Numbers t 32 16), E (Numbers t 32 16), E (Numbers t 32 16)) where
  genInput fname = do
    (br,sp,hp,_r1::HsInt,_r2::HsInt,_r3::HsInt,_r4::HsInt,_r5::HsInt,_r6::HsInt,spLim,a,b,c) <- define fname
    return ((OpConst a, OpConst b, OpConst c), FInfo br sp hp spLim)

instance AvxConstraint t 32 => LlvmFunctionInputs (E (Numbers t 32 16), E (Numbers t 32 16), E (Numbers t 32 16), E (Numbers t 32 16)) where
  genInput fname = do
    (br,sp,hp,_r1::HsInt,_r2::HsInt,_r3::HsInt,_r4::HsInt,_r5::HsInt,_r6::HsInt,spLim,a,b,c,d) <- define fname
    return ((OpConst a, OpConst b, OpConst c, OpConst d), FInfo br sp hp spLim)

instance AvxConstraint t 32 => LlvmFunctionInputs (E (Numbers t 32 16), E (Numbers t 32 16), E (Numbers t 32 16), E (Numbers t 32 16), E (Numbers t 32 16)) where
  genInput fname = do
    (br,sp,hp,_r1::HsInt,_r2::HsInt,_r3::HsInt,_r4::HsInt,_r5::HsInt,_r6::HsInt,spLim,a,b,c,d,e) <- define fname
    return ((OpConst a, OpConst b, OpConst c, OpConst d, OpConst e), FInfo br sp hp spLim)

instance AvxConstraint t 32 => LlvmFunctionInputs (E (Numbers t 32 16), E (Numbers t 32 16), E (Numbers t 32 16), E (Numbers t 32 16), E (Numbers t 32 16), E (Numbers t 32 16)) where
  genInput fname = do
    (br,sp,hp,_r1::HsInt,_r2::HsInt,_r3::HsInt,_r4::HsInt,_r5::HsInt,_r6::HsInt,spLim,a,b,c,d,e,f) <- define fname
    return ((OpConst a, OpConst b, OpConst c, OpConst d, OpConst e, OpConst f), FInfo br sp hp spLim)

instance (AvxConstraint t 32, KnownNat n)
         => LlvmFunctionInputs ( E (Numbers t 32 16), E (Numbers t 32 16), E (Numbers t 32 16), E (Numbers t 32 16), E (Numbers t 32 16), E (Numbers t 32 16)
                               , E (Numbers t 32 n)) where
  genInput fname = do
    (br,sp::HsPointer,hp,_r1::HsInt,_r2::HsInt,_r3::HsInt,_r4::HsInt,_r5::HsInt,_r6::HsInt,spLim,a,b,c,d,e,f) <- define fname
    (g,sp') <- popStack (unLabel sp)
    return ((OpConst a, OpConst b, OpConst c, OpConst d, OpConst e, OpConst f, OpConst g), FInfo br (Labeled sp') hp spLim)

instance (AvxConstraint t 32, KnownNat n)
         => LlvmFunctionInputs ( E (Numbers t 32 16), E (Numbers t 32 16), E (Numbers t 32 16), E (Numbers t 32 16), E (Numbers t 32 16), E (Numbers t 32 16)
                               , VarArg n (E (Numbers t 32 16))) where
  genInput fname = do
    (br,sp::HsPointer,hp,_r1::HsInt,_r2::HsInt,_r3::HsInt,_r4::HsInt,_r5::HsInt,_r6::HsInt,spLim,a,b,c,d,e,f) <- define fname
    (args,sp') <- popArgs (unLabel sp)
    return ((OpConst a, OpConst b, OpConst c, OpConst d, OpConst e, OpConst f, fmap OpConst args), FInfo br (Labeled sp') hp spLim)


instance AvxConstraint t 32
         => LlvmFunctionOutputs ( E (Numbers t 32 16)) where
  genOutput (FInfo br sp hp spLim) r' = do
    r <- compileE r'
    retptr   <- getReturnPointer (unLabel sp)
    tailcall retptr (br, sp, hp, ireg, ireg, ireg, ireg, ireg, ireg, spLim, r)

instance AvxConstraint t 32
         => LlvmFunctionOutputs ( E (Numbers t 32 16), E (Numbers t 32 16)) where
  genOutput (FInfo br sp hp spLim) (r1',r2') = do
    (r1,r2) <- (,) <$> compileE r1' <*> compileE r2'
    retptr   <- getReturnPointer (unLabel sp)
    tailcall retptr (br, sp, hp, ireg, ireg, ireg, ireg, ireg, ireg, spLim, r1, r2)

instance AvxConstraint t 32
         => LlvmFunctionOutputs ( E (Numbers t 32 16), E (Numbers t 32 16), E (Numbers t 32 16)) where
  genOutput (FInfo br sp hp spLim) (r1',r2',r3') = do
    (r1,r2,r3) <- (,,) <$> compileE r1' <*> compileE r2' <*> compileE r3'
    retptr   <- getReturnPointer (unLabel sp)
    tailcall retptr (br, sp, hp, ireg, ireg, ireg, ireg, ireg, ireg, spLim, r1, r2, r3)

instance AvxConstraint t 32
         => LlvmFunctionOutputs ( E (Numbers t 32 16), E (Numbers t 32 16), E (Numbers t 32 16), E (Numbers t 32 16)) where
  genOutput (FInfo br sp hp spLim) (r1',r2',r3',r4') = do
    (r1,r2,r3,r4) <- (,,,) <$> compileE r1' <*> compileE r2' <*> compileE r3' <*> compileE r4'
    retptr   <- getReturnPointer (unLabel sp)
    tailcall retptr (br, sp, hp, ireg, ireg, ireg, ireg, ireg, ireg, spLim,r1,r2,r3,r4)

instance AvxConstraint t 32
         => LlvmFunctionOutputs ( E (Numbers t 32 16), E (Numbers t 32 16), E (Numbers t 32 16), E (Numbers t 32 16), E (Numbers t 32 16)) where
  genOutput (FInfo br sp hp spLim) (r1',r2',r3',r4',r5') = do
    (r1,r2,r3,r4,r5) <- (,,,,) <$> compileE r1' <*> compileE r2' <*> compileE r3' <*> compileE r4' <*> compileE r5'
    retptr   <- getReturnPointer (unLabel sp)
    tailcall retptr (br, sp, hp, ireg, ireg, ireg, ireg, ireg, ireg, spLim,r1,r2,r3,r4,r5)

instance AvxConstraint t 32
         => LlvmFunctionOutputs ( E (Numbers t 32 16), E (Numbers t 32 16), E (Numbers t 32 16), E (Numbers t 32 16), E (Numbers t 32 16), E (Numbers t 32 16)) where
  genOutput (FInfo br sp hp spLim) (r1',r2',r3',r4',r5',r6') = do
    (r1,r2,r3,r4,r5,r6) <- (,,,,,) <$> compileE r1' <*> compileE r2' <*> compileE r3' <*> compileE r4' <*> compileE r5' <*> compileE r6'
    retptr   <- getReturnPointer (unLabel sp)
    tailcall retptr (br, sp, hp, ireg, ireg, ireg, ireg, ireg, ireg, spLim,r1,r2,r3,r4,r5,r6)

instance (AvxConstraint t 32, KnownNat n)
         => LlvmFunctionOutputs ( E (Numbers t 32 16), E (Numbers t 32 16), E (Numbers t 32 16), E (Numbers t 32 16), E (Numbers t 32 16), E (Numbers t 32 16)
                                , E (Numbers t 32 n)) where
  genOutput (FInfo br (Labeled sp) hp spLim) (r1',r2',r3',r4',r5',r6',rn') = do
    (r1,r2,r3,r4,r5,r6,rn) <- (,,,,,,) <$> compileE r1' <*> compileE r2' <*> compileE r3' <*> compileE r4' <*> compileE r5' <*> compileE r6' <*> compileE rn'
    sp' <- pushStack sp rn
    retptr <- getReturnPointer sp'
    tailcall retptr (br, sp', hp, ireg, ireg, ireg, ireg, ireg, ireg, spLim,r1,r2,r3,r4,r5,r6)

instance (AvxConstraint t 32, KnownNat n)
         => LlvmFunctionOutputs ( E (Numbers t 32 16), E (Numbers t 32 16), E (Numbers t 32 16), E (Numbers t 32 16), E (Numbers t 32 16), E (Numbers t 32 16)
                                , VarArg n (E (Numbers t 32 16))) where
  genOutput (FInfo br (Labeled sp) hp spLim) (r1',r2',r3',r4',r5',r6',rn') = do
    (r1,r2,r3,r4,r5,r6) <- (,,,,,) <$> compileE r1' <*> compileE r2' <*> compileE r3' <*> compileE r4' <*> compileE r5' <*> compileE r6'
    rn <- mapM compileE rn'
    sp' <- pushArgs sp rn
    retptr <- getReturnPointer sp'
    tailcall retptr (br, sp', hp, ireg, ireg, ireg, ireg, ireg, ireg, spLim,r1,r2,r3,r4,r5,r6)

----------------------------------------------------------------------------------------------------
-- 64 x 8
----------------------------------------------------------------------------------------------------

instance AvxConstraint t 64 => LlvmFunctionInputs (E (Numbers t 64 8)) where
  genInput fname = do
    (br,sp,hp,_r1::HsInt,_r2::HsInt,_r3::HsInt,_r4::HsInt,_r5::HsInt,_r6::HsInt,spLim,a) <- define fname
    return (OpConst a, FInfo br sp hp spLim)

instance AvxConstraint t 64 => LlvmFunctionInputs (E (Numbers t 64 8), E (Numbers t 64 8)) where
  genInput fname = do
    (br,sp,hp,_r1::HsInt,_r2::HsInt,_r3::HsInt,_r4::HsInt,_r5::HsInt,_r6::HsInt,spLim,a,b) <- define fname
    return ((OpConst a, OpConst b), FInfo br sp hp spLim)

instance AvxConstraint t 64 => LlvmFunctionInputs (E (Numbers t 64 8), E (Numbers t 64 8), E (Numbers t 64 8)) where
  genInput fname = do
    (br,sp,hp,_r1::HsInt,_r2::HsInt,_r3::HsInt,_r4::HsInt,_r5::HsInt,_r6::HsInt,spLim,a,b,c) <- define fname
    return ((OpConst a, OpConst b, OpConst c), FInfo br sp hp spLim)

instance AvxConstraint t 64 => LlvmFunctionInputs (E (Numbers t 64 8), E (Numbers t 64 8), E (Numbers t 64 8), E (Numbers t 64 8)) where
  genInput fname = do
    (br,sp,hp,_r1::HsInt,_r2::HsInt,_r3::HsInt,_r4::HsInt,_r5::HsInt,_r6::HsInt,spLim,a,b,c,d) <- define fname
    return ((OpConst a, OpConst b, OpConst c, OpConst d), FInfo br sp hp spLim)

instance AvxConstraint t 64 => LlvmFunctionInputs (E (Numbers t 64 8), E (Numbers t 64 8), E (Numbers t 64 8), E (Numbers t 64 8), E (Numbers t 64 8)) where
  genInput fname = do
    (br,sp,hp,_r1::HsInt,_r2::HsInt,_r3::HsInt,_r4::HsInt,_r5::HsInt,_r6::HsInt,spLim,a,b,c,d,e) <- define fname
    return ((OpConst a, OpConst b, OpConst c, OpConst d, OpConst e), FInfo br sp hp spLim)

instance AvxConstraint t 64 => LlvmFunctionInputs (E (Numbers t 64 8), E (Numbers t 64 8), E (Numbers t 64 8), E (Numbers t 64 8), E (Numbers t 64 8), E (Numbers t 64 8)) where
  genInput fname = do
    (br,sp,hp,_r1::HsInt,_r2::HsInt,_r3::HsInt,_r4::HsInt,_r5::HsInt,_r6::HsInt,spLim,a,b,c,d,e,f) <- define fname
    return ((OpConst a, OpConst b, OpConst c, OpConst d, OpConst e, OpConst f), FInfo br sp hp spLim)

instance (AvxConstraint t 64, KnownNat n)
         => LlvmFunctionInputs ( E (Numbers t 64 8), E (Numbers t 64 8), E (Numbers t 64 8), E (Numbers t 64 8), E (Numbers t 64 8), E (Numbers t 64 8)
                               , E (Numbers t 64 n)) where
  genInput fname = do
    (br,sp::HsPointer,hp,_r1::HsInt,_r2::HsInt,_r3::HsInt,_r4::HsInt,_r5::HsInt,_r6::HsInt,spLim,a,b,c,d,e,f) <- define fname
    (g,sp') <- popStack (unLabel sp)
    return ((OpConst a, OpConst b, OpConst c, OpConst d, OpConst e, OpConst f, OpConst g), FInfo br (Labeled sp') hp spLim)

instance (AvxConstraint t 64, KnownNat n)
         => LlvmFunctionInputs ( E (Numbers t 64 8), E (Numbers t 64 8), E (Numbers t 64 8), E (Numbers t 64 8), E (Numbers t 64 8), E (Numbers t 64 8)
                               , VarArg n (E (Numbers t 64 8))) where
  genInput fname = do
    (br,sp::HsPointer,hp,_r1::HsInt,_r2::HsInt,_r3::HsInt,_r4::HsInt,_r5::HsInt,_r6::HsInt,spLim,a,b,c,d,e,f) <- define fname
    (args,sp') <- popArgs (unLabel sp)
    return ((OpConst a, OpConst b, OpConst c, OpConst d, OpConst e, OpConst f, fmap OpConst args), FInfo br (Labeled sp') hp spLim)


instance AvxConstraint t 64
         => LlvmFunctionOutputs ( E (Numbers t 64 8)) where
  genOutput (FInfo br sp hp spLim) r' = do
    r <- compileE r'
    retptr   <- getReturnPointer (unLabel sp)
    tailcall retptr (br, sp, hp, ireg, ireg, ireg, ireg, ireg, ireg, spLim, r)

instance AvxConstraint t 64
         => LlvmFunctionOutputs ( E (Numbers t 64 8), E (Numbers t 64 8)) where
  genOutput (FInfo br sp hp spLim) (r1',r2') = do
    (r1,r2) <- (,) <$> compileE r1' <*> compileE r2'
    retptr   <- getReturnPointer (unLabel sp)
    tailcall retptr (br, sp, hp, ireg, ireg, ireg, ireg, ireg, ireg, spLim, r1, r2)

instance AvxConstraint t 64
         => LlvmFunctionOutputs ( E (Numbers t 64 8), E (Numbers t 64 8), E (Numbers t 64 8)) where
  genOutput (FInfo br sp hp spLim) (r1',r2',r3') = do
    (r1,r2,r3) <- (,,) <$> compileE r1' <*> compileE r2' <*> compileE r3'
    retptr   <- getReturnPointer (unLabel sp)
    tailcall retptr (br, sp, hp, ireg, ireg, ireg, ireg, ireg, ireg, spLim, r1, r2, r3)

instance AvxConstraint t 64
         => LlvmFunctionOutputs ( E (Numbers t 64 8), E (Numbers t 64 8), E (Numbers t 64 8), E (Numbers t 64 8)) where
  genOutput (FInfo br sp hp spLim) (r1',r2',r3',r4') = do
    (r1,r2,r3,r4) <- (,,,) <$> compileE r1' <*> compileE r2' <*> compileE r3' <*> compileE r4'
    retptr   <- getReturnPointer (unLabel sp)
    tailcall retptr (br, sp, hp, ireg, ireg, ireg, ireg, ireg, ireg, spLim,r1,r2,r3,r4)

instance AvxConstraint t 64
         => LlvmFunctionOutputs ( E (Numbers t 64 8), E (Numbers t 64 8), E (Numbers t 64 8), E (Numbers t 64 8), E (Numbers t 64 8)) where
  genOutput (FInfo br sp hp spLim) (r1',r2',r3',r4',r5') = do
    (r1,r2,r3,r4,r5) <- (,,,,) <$> compileE r1' <*> compileE r2' <*> compileE r3' <*> compileE r4' <*> compileE r5'
    retptr   <- getReturnPointer (unLabel sp)
    tailcall retptr (br, sp, hp, ireg, ireg, ireg, ireg, ireg, ireg, spLim,r1,r2,r3,r4,r5)

instance AvxConstraint t 64
         => LlvmFunctionOutputs ( E (Numbers t 64 8), E (Numbers t 64 8), E (Numbers t 64 8), E (Numbers t 64 8), E (Numbers t 64 8), E (Numbers t 64 8)) where
  genOutput (FInfo br sp hp spLim) (r1',r2',r3',r4',r5',r6') = do
    (r1,r2,r3,r4,r5,r6) <- (,,,,,) <$> compileE r1' <*> compileE r2' <*> compileE r3' <*> compileE r4' <*> compileE r5' <*> compileE r6'
    retptr   <- getReturnPointer (unLabel sp)
    tailcall retptr (br, sp, hp, ireg, ireg, ireg, ireg, ireg, ireg, spLim,r1,r2,r3,r4,r5,r6)

instance (AvxConstraint t 64, KnownNat n)
         => LlvmFunctionOutputs ( E (Numbers t 64 8), E (Numbers t 64 8), E (Numbers t 64 8), E (Numbers t 64 8), E (Numbers t 64 8), E (Numbers t 64 8)
                                , E (Numbers t 64 n)) where
  genOutput (FInfo br (Labeled sp) hp spLim) (r1',r2',r3',r4',r5',r6',rn') = do
    (r1,r2,r3,r4,r5,r6,rn) <- (,,,,,,) <$> compileE r1' <*> compileE r2' <*> compileE r3' <*> compileE r4' <*> compileE r5' <*> compileE r6' <*> compileE rn'
    sp' <- pushStack sp rn
    retptr <- getReturnPointer sp'
    tailcall retptr (br, sp', hp, ireg, ireg, ireg, ireg, ireg, ireg, spLim,r1,r2,r3,r4,r5,r6)

instance (AvxConstraint t 64, KnownNat n)
         => LlvmFunctionOutputs ( E (Numbers t 64 8), E (Numbers t 64 8), E (Numbers t 64 8), E (Numbers t 64 8), E (Numbers t 64 8), E (Numbers t 64 8)
                                , VarArg n (E (Numbers t 64 8))) where
  genOutput (FInfo br (Labeled sp) hp spLim) (r1',r2',r3',r4',r5',r6',rn') = do
    (r1,r2,r3,r4,r5,r6) <- (,,,,,) <$> compileE r1' <*> compileE r2' <*> compileE r3' <*> compileE r4' <*> compileE r5' <*> compileE r6'
    rn <- mapM compileE rn'
    sp' <- pushArgs sp rn
    retptr <- getReturnPointer sp'
    tailcall retptr (br, sp', hp, ireg, ireg, ireg, ireg, ireg, ireg, spLim,r1,r2,r3,r4,r5,r6)

#endif

#ifdef USING_VECTORS_AVX
#ifdef USING_VECTORS_AVX2
type AvxConstraint t bits = (LlvmNum t bits)
#else
type AvxConstraint t bits = (NumberCategory t ~ 'CatInt, LlvmNum t bits)
#endif

#ifdef USING_VECTORS_AVX2

----------------------------------------------------------------------------------------------------
-- 8 x 32
----------------------------------------------------------------------------------------------------

instance AvxConstraint t 8 => LlvmFunctionInputs (E (Numbers t 8 32)) where
  genInput fname = do
    (br,sp,hp,_r1::HsInt,_r2::HsInt,_r3::HsInt,_r4::HsInt,_r5::HsInt,_r6::HsInt,spLim,a) <- define fname
    return (OpConst a, FInfo br sp hp spLim)

instance AvxConstraint t 8 => LlvmFunctionInputs (E (Numbers t 8 32), E (Numbers t 8 32)) where
  genInput fname = do
    (br,sp,hp,_r1::HsInt,_r2::HsInt,_r3::HsInt,_r4::HsInt,_r5::HsInt,_r6::HsInt,spLim,a,b) <- define fname
    return ((OpConst a, OpConst b), FInfo br sp hp spLim)

instance AvxConstraint t 8 => LlvmFunctionInputs (E (Numbers t 8 32), E (Numbers t 8 32), E (Numbers t 8 32)) where
  genInput fname = do
    (br,sp,hp,_r1::HsInt,_r2::HsInt,_r3::HsInt,_r4::HsInt,_r5::HsInt,_r6::HsInt,spLim,a,b,c) <- define fname
    return ((OpConst a, OpConst b, OpConst c), FInfo br sp hp spLim)

instance AvxConstraint t 8 => LlvmFunctionInputs (E (Numbers t 8 32), E (Numbers t 8 32), E (Numbers t 8 32), E (Numbers t 8 32)) where
  genInput fname = do
    (br,sp,hp,_r1::HsInt,_r2::HsInt,_r3::HsInt,_r4::HsInt,_r5::HsInt,_r6::HsInt,spLim,a,b,c,d) <- define fname
    return ((OpConst a, OpConst b, OpConst c, OpConst d), FInfo br sp hp spLim)

instance AvxConstraint t 8 => LlvmFunctionInputs (E (Numbers t 8 32), E (Numbers t 8 32), E (Numbers t 8 32), E (Numbers t 8 32), E (Numbers t 8 32)) where
  genInput fname = do
    (br,sp,hp,_r1::HsInt,_r2::HsInt,_r3::HsInt,_r4::HsInt,_r5::HsInt,_r6::HsInt,spLim,a,b,c,d,e) <- define fname
    return ((OpConst a, OpConst b, OpConst c, OpConst d, OpConst e), FInfo br sp hp spLim)

instance AvxConstraint t 8 => LlvmFunctionInputs (E (Numbers t 8 32), E (Numbers t 8 32), E (Numbers t 8 32), E (Numbers t 8 32), E (Numbers t 8 32), E (Numbers t 8 32)) where
  genInput fname = do
    (br,sp,hp,_r1::HsInt,_r2::HsInt,_r3::HsInt,_r4::HsInt,_r5::HsInt,_r6::HsInt,spLim,a,b,c,d,e,f) <- define fname
    return ((OpConst a, OpConst b, OpConst c, OpConst d, OpConst e, OpConst f), FInfo br sp hp spLim)

instance (AvxConstraint t 8, KnownNat n)
         => LlvmFunctionInputs ( E (Numbers t 8 32), E (Numbers t 8 32), E (Numbers t 8 32), E (Numbers t 8 32), E (Numbers t 8 32), E (Numbers t 8 32)
                               , E (Numbers t 8 n)) where
  genInput fname = do
    (br,sp::HsPointer,hp,_r1::HsInt,_r2::HsInt,_r3::HsInt,_r4::HsInt,_r5::HsInt,_r6::HsInt,spLim,a,b,c,d,e,f) <- define fname
    (g,sp') <- popStack (unLabel sp)
    return ((OpConst a, OpConst b, OpConst c, OpConst d, OpConst e, OpConst f, OpConst g), FInfo br (Labeled sp') hp spLim)

instance (AvxConstraint t 8, KnownNat n)
         => LlvmFunctionInputs ( E (Numbers t 8 32), E (Numbers t 8 32), E (Numbers t 8 32), E (Numbers t 8 32), E (Numbers t 8 32), E (Numbers t 8 32)
                               , VarArg n (E (Numbers t 8 32))) where
  genInput fname = do
    (br,sp::HsPointer,hp,_r1::HsInt,_r2::HsInt,_r3::HsInt,_r4::HsInt,_r5::HsInt,_r6::HsInt,spLim,a,b,c,d,e,f) <- define fname
    (args,sp') <- popArgs (unLabel sp)
    return ((OpConst a, OpConst b, OpConst c, OpConst d, OpConst e, OpConst f, fmap OpConst args), FInfo br (Labeled sp') hp spLim)


instance AvxConstraint t 8
         => LlvmFunctionOutputs ( E (Numbers t 8 32)) where
  genOutput (FInfo br sp hp spLim) r' = do
    r <- compileE r'
    retptr   <- getReturnPointer (unLabel sp)
    tailcall retptr (br, sp, hp, ireg, ireg, ireg, ireg, ireg, ireg, spLim, r)

instance AvxConstraint t 8
         => LlvmFunctionOutputs ( E (Numbers t 8 32), E (Numbers t 8 32)) where
  genOutput (FInfo br sp hp spLim) (r1',r2') = do
    (r1,r2) <- (,) <$> compileE r1' <*> compileE r2'
    retptr   <- getReturnPointer (unLabel sp)
    tailcall retptr (br, sp, hp, ireg, ireg, ireg, ireg, ireg, ireg, spLim, r1, r2)

instance AvxConstraint t 8
         => LlvmFunctionOutputs ( E (Numbers t 8 32), E (Numbers t 8 32), E (Numbers t 8 32)) where
  genOutput (FInfo br sp hp spLim) (r1',r2',r3') = do
    (r1,r2,r3) <- (,,) <$> compileE r1' <*> compileE r2' <*> compileE r3'
    retptr   <- getReturnPointer (unLabel sp)
    tailcall retptr (br, sp, hp, ireg, ireg, ireg, ireg, ireg, ireg, spLim, r1, r2, r3)

instance AvxConstraint t 8
         => LlvmFunctionOutputs ( E (Numbers t 8 32), E (Numbers t 8 32), E (Numbers t 8 32), E (Numbers t 8 32)) where
  genOutput (FInfo br sp hp spLim) (r1',r2',r3',r4') = do
    (r1,r2,r3,r4) <- (,,,) <$> compileE r1' <*> compileE r2' <*> compileE r3' <*> compileE r4'
    retptr   <- getReturnPointer (unLabel sp)
    tailcall retptr (br, sp, hp, ireg, ireg, ireg, ireg, ireg, ireg, spLim,r1,r2,r3,r4)

instance AvxConstraint t 8
         => LlvmFunctionOutputs ( E (Numbers t 8 32), E (Numbers t 8 32), E (Numbers t 8 32), E (Numbers t 8 32), E (Numbers t 8 32)) where
  genOutput (FInfo br sp hp spLim) (r1',r2',r3',r4',r5') = do
    (r1,r2,r3,r4,r5) <- (,,,,) <$> compileE r1' <*> compileE r2' <*> compileE r3' <*> compileE r4' <*> compileE r5'
    retptr   <- getReturnPointer (unLabel sp)
    tailcall retptr (br, sp, hp, ireg, ireg, ireg, ireg, ireg, ireg, spLim,r1,r2,r3,r4,r5)

instance AvxConstraint t 8
         => LlvmFunctionOutputs ( E (Numbers t 8 32), E (Numbers t 8 32), E (Numbers t 8 32), E (Numbers t 8 32), E (Numbers t 8 32), E (Numbers t 8 32)) where
  genOutput (FInfo br sp hp spLim) (r1',r2',r3',r4',r5',r6') = do
    (r1,r2,r3,r4,r5,r6) <- (,,,,,) <$> compileE r1' <*> compileE r2' <*> compileE r3' <*> compileE r4' <*> compileE r5' <*> compileE r6'
    retptr   <- getReturnPointer (unLabel sp)
    tailcall retptr (br, sp, hp, ireg, ireg, ireg, ireg, ireg, ireg, spLim,r1,r2,r3,r4,r5,r6)

instance (AvxConstraint t 8, KnownNat n)
         => LlvmFunctionOutputs ( E (Numbers t 8 32), E (Numbers t 8 32), E (Numbers t 8 32), E (Numbers t 8 32), E (Numbers t 8 32), E (Numbers t 8 32)
                                , E (Numbers t 8 n)) where
  genOutput (FInfo br (Labeled sp) hp spLim) (r1',r2',r3',r4',r5',r6',rn') = do
    (r1,r2,r3,r4,r5,r6,rn) <- (,,,,,,) <$> compileE r1' <*> compileE r2' <*> compileE r3' <*> compileE r4' <*> compileE r5' <*> compileE r6' <*> compileE rn'
    sp' <- pushStack sp rn
    retptr <- getReturnPointer sp'
    tailcall retptr (br, sp', hp, ireg, ireg, ireg, ireg, ireg, ireg, spLim,r1,r2,r3,r4,r5,r6)

instance (AvxConstraint t 8, KnownNat n)
         => LlvmFunctionOutputs ( E (Numbers t 8 32), E (Numbers t 8 32), E (Numbers t 8 32), E (Numbers t 8 32), E (Numbers t 8 32), E (Numbers t 8 32)
                                , VarArg n (E (Numbers t 8 32))) where
  genOutput (FInfo br (Labeled sp) hp spLim) (r1',r2',r3',r4',r5',r6',rn') = do
    (r1,r2,r3,r4,r5,r6) <- (,,,,,) <$> compileE r1' <*> compileE r2' <*> compileE r3' <*> compileE r4' <*> compileE r5' <*> compileE r6'
    rn <- mapM compileE rn'
    sp' <- pushArgs sp rn
    retptr <- getReturnPointer sp'
    tailcall retptr (br, sp', hp, ireg, ireg, ireg, ireg, ireg, ireg, spLim,r1,r2,r3,r4,r5,r6)

----------------------------------------------------------------------------------------------------
-- 16 x 16
----------------------------------------------------------------------------------------------------

instance AvxConstraint t 16 => LlvmFunctionInputs (E (Numbers t 16 16)) where
  genInput fname = do
    (br,sp,hp,_r1::HsInt,_r2::HsInt,_r3::HsInt,_r4::HsInt,_r5::HsInt,_r6::HsInt,spLim,a) <- define fname
    return (OpConst a, FInfo br sp hp spLim)

instance AvxConstraint t 16 => LlvmFunctionInputs (E (Numbers t 16 16), E (Numbers t 16 16)) where
  genInput fname = do
    (br,sp,hp,_r1::HsInt,_r2::HsInt,_r3::HsInt,_r4::HsInt,_r5::HsInt,_r6::HsInt,spLim,a,b) <- define fname
    return ((OpConst a, OpConst b), FInfo br sp hp spLim)

instance AvxConstraint t 16 => LlvmFunctionInputs (E (Numbers t 16 16), E (Numbers t 16 16), E (Numbers t 16 16)) where
  genInput fname = do
    (br,sp,hp,_r1::HsInt,_r2::HsInt,_r3::HsInt,_r4::HsInt,_r5::HsInt,_r6::HsInt,spLim,a,b,c) <- define fname
    return ((OpConst a, OpConst b, OpConst c), FInfo br sp hp spLim)

instance AvxConstraint t 16 => LlvmFunctionInputs (E (Numbers t 16 16), E (Numbers t 16 16), E (Numbers t 16 16), E (Numbers t 16 16)) where
  genInput fname = do
    (br,sp,hp,_r1::HsInt,_r2::HsInt,_r3::HsInt,_r4::HsInt,_r5::HsInt,_r6::HsInt,spLim,a,b,c,d) <- define fname
    return ((OpConst a, OpConst b, OpConst c, OpConst d), FInfo br sp hp spLim)

instance AvxConstraint t 16 => LlvmFunctionInputs (E (Numbers t 16 16), E (Numbers t 16 16), E (Numbers t 16 16), E (Numbers t 16 16), E (Numbers t 16 16)) where
  genInput fname = do
    (br,sp,hp,_r1::HsInt,_r2::HsInt,_r3::HsInt,_r4::HsInt,_r5::HsInt,_r6::HsInt,spLim,a,b,c,d,e) <- define fname
    return ((OpConst a, OpConst b, OpConst c, OpConst d, OpConst e), FInfo br sp hp spLim)

instance AvxConstraint t 16 => LlvmFunctionInputs (E (Numbers t 16 16), E (Numbers t 16 16), E (Numbers t 16 16), E (Numbers t 16 16), E (Numbers t 16 16), E (Numbers t 16 16)) where
  genInput fname = do
    (br,sp,hp,_r1::HsInt,_r2::HsInt,_r3::HsInt,_r4::HsInt,_r5::HsInt,_r6::HsInt,spLim,a,b,c,d,e,f) <- define fname
    return ((OpConst a, OpConst b, OpConst c, OpConst d, OpConst e, OpConst f), FInfo br sp hp spLim)

instance (AvxConstraint t 16, KnownNat n)
         => LlvmFunctionInputs ( E (Numbers t 16 16), E (Numbers t 16 16), E (Numbers t 16 16), E (Numbers t 16 16), E (Numbers t 16 16), E (Numbers t 16 16)
                               , E (Numbers t 16 n)) where
  genInput fname = do
    (br,sp::HsPointer,hp,_r1::HsInt,_r2::HsInt,_r3::HsInt,_r4::HsInt,_r5::HsInt,_r6::HsInt,spLim,a,b,c,d,e,f) <- define fname
    (g,sp') <- popStack (unLabel sp)
    return ((OpConst a, OpConst b, OpConst c, OpConst d, OpConst e, OpConst f, OpConst g), FInfo br (Labeled sp') hp spLim)

instance (AvxConstraint t 16, KnownNat n)
         => LlvmFunctionInputs ( E (Numbers t 16 16), E (Numbers t 16 16), E (Numbers t 16 16), E (Numbers t 16 16), E (Numbers t 16 16), E (Numbers t 16 16)
                               , VarArg n (E (Numbers t 16 16))) where
  genInput fname = do
    (br,sp::HsPointer,hp,_r1::HsInt,_r2::HsInt,_r3::HsInt,_r4::HsInt,_r5::HsInt,_r6::HsInt,spLim,a,b,c,d,e,f) <- define fname
    (args,sp') <- popArgs (unLabel sp)
    return ((OpConst a, OpConst b, OpConst c, OpConst d, OpConst e, OpConst f, fmap OpConst args), FInfo br (Labeled sp') hp spLim)


instance AvxConstraint t 16
         => LlvmFunctionOutputs ( E (Numbers t 16 16)) where
  genOutput (FInfo br sp hp spLim) r' = do
    r <- compileE r'
    retptr   <- getReturnPointer (unLabel sp)
    tailcall retptr (br, sp, hp, ireg, ireg, ireg, ireg, ireg, ireg, spLim, r)

instance AvxConstraint t 16
         => LlvmFunctionOutputs ( E (Numbers t 16 16), E (Numbers t 16 16)) where
  genOutput (FInfo br sp hp spLim) (r1',r2') = do
    (r1,r2) <- (,) <$> compileE r1' <*> compileE r2'
    retptr   <- getReturnPointer (unLabel sp)
    tailcall retptr (br, sp, hp, ireg, ireg, ireg, ireg, ireg, ireg, spLim, r1, r2)

instance AvxConstraint t 16
         => LlvmFunctionOutputs ( E (Numbers t 16 16), E (Numbers t 16 16), E (Numbers t 16 16)) where
  genOutput (FInfo br sp hp spLim) (r1',r2',r3') = do
    (r1,r2,r3) <- (,,) <$> compileE r1' <*> compileE r2' <*> compileE r3'
    retptr   <- getReturnPointer (unLabel sp)
    tailcall retptr (br, sp, hp, ireg, ireg, ireg, ireg, ireg, ireg, spLim, r1, r2, r3)

instance AvxConstraint t 16
         => LlvmFunctionOutputs ( E (Numbers t 16 16), E (Numbers t 16 16), E (Numbers t 16 16), E (Numbers t 16 16)) where
  genOutput (FInfo br sp hp spLim) (r1',r2',r3',r4') = do
    (r1,r2,r3,r4) <- (,,,) <$> compileE r1' <*> compileE r2' <*> compileE r3' <*> compileE r4'
    retptr   <- getReturnPointer (unLabel sp)
    tailcall retptr (br, sp, hp, ireg, ireg, ireg, ireg, ireg, ireg, spLim,r1,r2,r3,r4)

instance AvxConstraint t 16
         => LlvmFunctionOutputs ( E (Numbers t 16 16), E (Numbers t 16 16), E (Numbers t 16 16), E (Numbers t 16 16), E (Numbers t 16 16)) where
  genOutput (FInfo br sp hp spLim) (r1',r2',r3',r4',r5') = do
    (r1,r2,r3,r4,r5) <- (,,,,) <$> compileE r1' <*> compileE r2' <*> compileE r3' <*> compileE r4' <*> compileE r5'
    retptr   <- getReturnPointer (unLabel sp)
    tailcall retptr (br, sp, hp, ireg, ireg, ireg, ireg, ireg, ireg, spLim,r1,r2,r3,r4,r5)

instance AvxConstraint t 16
         => LlvmFunctionOutputs ( E (Numbers t 16 16), E (Numbers t 16 16), E (Numbers t 16 16), E (Numbers t 16 16), E (Numbers t 16 16), E (Numbers t 16 16)) where
  genOutput (FInfo br sp hp spLim) (r1',r2',r3',r4',r5',r6') = do
    (r1,r2,r3,r4,r5,r6) <- (,,,,,) <$> compileE r1' <*> compileE r2' <*> compileE r3' <*> compileE r4' <*> compileE r5' <*> compileE r6'
    retptr   <- getReturnPointer (unLabel sp)
    tailcall retptr (br, sp, hp, ireg, ireg, ireg, ireg, ireg, ireg, spLim,r1,r2,r3,r4,r5,r6)

instance (AvxConstraint t 16, KnownNat n)
         => LlvmFunctionOutputs ( E (Numbers t 16 16), E (Numbers t 16 16), E (Numbers t 16 16), E (Numbers t 16 16), E (Numbers t 16 16), E (Numbers t 16 16)
                                , E (Numbers t 16 n)) where
  genOutput (FInfo br (Labeled sp) hp spLim) (r1',r2',r3',r4',r5',r6',rn') = do
    (r1,r2,r3,r4,r5,r6,rn) <- (,,,,,,) <$> compileE r1' <*> compileE r2' <*> compileE r3' <*> compileE r4' <*> compileE r5' <*> compileE r6' <*> compileE rn'
    sp' <- pushStack sp rn
    retptr <- getReturnPointer sp'
    tailcall retptr (br, sp', hp, ireg, ireg, ireg, ireg, ireg, ireg, spLim,r1,r2,r3,r4,r5,r6)

instance (AvxConstraint t 16, KnownNat n)
         => LlvmFunctionOutputs ( E (Numbers t 16 16), E (Numbers t 16 16), E (Numbers t 16 16), E (Numbers t 16 16), E (Numbers t 16 16), E (Numbers t 16 16)
                                , VarArg n (E (Numbers t 16 16))) where
  genOutput (FInfo br (Labeled sp) hp spLim) (r1',r2',r3',r4',r5',r6',rn') = do
    (r1,r2,r3,r4,r5,r6) <- (,,,,,) <$> compileE r1' <*> compileE r2' <*> compileE r3' <*> compileE r4' <*> compileE r5' <*> compileE r6'
    rn <- mapM compileE rn'
    sp' <- pushArgs sp rn
    retptr <- getReturnPointer sp'
    tailcall retptr (br, sp', hp, ireg, ireg, ireg, ireg, ireg, ireg, spLim,r1,r2,r3,r4,r5,r6)


#endif

----------------------------------------------------------------------------------------------------
-- 32 x 8
----------------------------------------------------------------------------------------------------

instance AvxConstraint t 32 => LlvmFunctionInputs (E (Numbers t 32 8)) where
  genInput fname = do
    (br,sp,hp,_r1::HsInt,_r2::HsInt,_r3::HsInt,_r4::HsInt,_r5::HsInt,_r6::HsInt,spLim,a) <- define fname
    return (OpConst a, FInfo br sp hp spLim)

instance AvxConstraint t 32 => LlvmFunctionInputs (E (Numbers t 32 8), E (Numbers t 32 8)) where
  genInput fname = do
    (br,sp,hp,_r1::HsInt,_r2::HsInt,_r3::HsInt,_r4::HsInt,_r5::HsInt,_r6::HsInt,spLim,a,b) <- define fname
    return ((OpConst a, OpConst b), FInfo br sp hp spLim)

instance AvxConstraint t 32 => LlvmFunctionInputs (E (Numbers t 32 8), E (Numbers t 32 8), E (Numbers t 32 8)) where
  genInput fname = do
    (br,sp,hp,_r1::HsInt,_r2::HsInt,_r3::HsInt,_r4::HsInt,_r5::HsInt,_r6::HsInt,spLim,a,b,c) <- define fname
    return ((OpConst a, OpConst b, OpConst c), FInfo br sp hp spLim)

instance AvxConstraint t 32 => LlvmFunctionInputs (E (Numbers t 32 8), E (Numbers t 32 8), E (Numbers t 32 8), E (Numbers t 32 8)) where
  genInput fname = do
    (br,sp,hp,_r1::HsInt,_r2::HsInt,_r3::HsInt,_r4::HsInt,_r5::HsInt,_r6::HsInt,spLim,a,b,c,d) <- define fname
    return ((OpConst a, OpConst b, OpConst c, OpConst d), FInfo br sp hp spLim)

instance AvxConstraint t 32 => LlvmFunctionInputs (E (Numbers t 32 8), E (Numbers t 32 8), E (Numbers t 32 8), E (Numbers t 32 8), E (Numbers t 32 8)) where
  genInput fname = do
    (br,sp,hp,_r1::HsInt,_r2::HsInt,_r3::HsInt,_r4::HsInt,_r5::HsInt,_r6::HsInt,spLim,a,b,c,d,e) <- define fname
    return ((OpConst a, OpConst b, OpConst c, OpConst d, OpConst e), FInfo br sp hp spLim)

instance AvxConstraint t 32 => LlvmFunctionInputs (E (Numbers t 32 8), E (Numbers t 32 8), E (Numbers t 32 8), E (Numbers t 32 8), E (Numbers t 32 8), E (Numbers t 32 8)) where
  genInput fname = do
    (br,sp,hp,_r1::HsInt,_r2::HsInt,_r3::HsInt,_r4::HsInt,_r5::HsInt,_r6::HsInt,spLim,a,b,c,d,e,f) <- define fname
    return ((OpConst a, OpConst b, OpConst c, OpConst d, OpConst e, OpConst f), FInfo br sp hp spLim)

instance (AvxConstraint t 32, KnownNat n)
         => LlvmFunctionInputs ( E (Numbers t 32 8), E (Numbers t 32 8), E (Numbers t 32 8), E (Numbers t 32 8), E (Numbers t 32 8), E (Numbers t 32 8)
                               , E (Numbers t 32 n)) where
  genInput fname = do
    (br,sp::HsPointer,hp,_r1::HsInt,_r2::HsInt,_r3::HsInt,_r4::HsInt,_r5::HsInt,_r6::HsInt,spLim,a,b,c,d,e,f) <- define fname
    (g,sp') <- popStack (unLabel sp)
    return ((OpConst a, OpConst b, OpConst c, OpConst d, OpConst e, OpConst f, OpConst g), FInfo br (Labeled sp') hp spLim)

instance (AvxConstraint t 32, KnownNat n)
         => LlvmFunctionInputs ( E (Numbers t 32 8), E (Numbers t 32 8), E (Numbers t 32 8), E (Numbers t 32 8), E (Numbers t 32 8), E (Numbers t 32 8)
                               , VarArg n (E (Numbers t 32 8))) where
  genInput fname = do
    (br,sp::HsPointer,hp,_r1::HsInt,_r2::HsInt,_r3::HsInt,_r4::HsInt,_r5::HsInt,_r6::HsInt,spLim,a,b,c,d,e,f) <- define fname
    (args,sp') <- popArgs (unLabel sp)
    return ((OpConst a, OpConst b, OpConst c, OpConst d, OpConst e, OpConst f, fmap OpConst args), FInfo br (Labeled sp') hp spLim)


instance AvxConstraint t 32
         => LlvmFunctionOutputs ( E (Numbers t 32 8)) where
  genOutput (FInfo br sp hp spLim) r' = do
    r <- compileE r'
    retptr   <- getReturnPointer (unLabel sp)
    tailcall retptr (br, sp, hp, ireg, ireg, ireg, ireg, ireg, ireg, spLim, r)

instance AvxConstraint t 32
         => LlvmFunctionOutputs ( E (Numbers t 32 8), E (Numbers t 32 8)) where
  genOutput (FInfo br sp hp spLim) (r1',r2') = do
    (r1,r2) <- (,) <$> compileE r1' <*> compileE r2'
    retptr   <- getReturnPointer (unLabel sp)
    tailcall retptr (br, sp, hp, ireg, ireg, ireg, ireg, ireg, ireg, spLim, r1, r2)

instance AvxConstraint t 32
         => LlvmFunctionOutputs ( E (Numbers t 32 8), E (Numbers t 32 8), E (Numbers t 32 8)) where
  genOutput (FInfo br sp hp spLim) (r1',r2',r3') = do
    (r1,r2,r3) <- (,,) <$> compileE r1' <*> compileE r2' <*> compileE r3'
    retptr   <- getReturnPointer (unLabel sp)
    tailcall retptr (br, sp, hp, ireg, ireg, ireg, ireg, ireg, ireg, spLim, r1, r2, r3)

instance AvxConstraint t 32
         => LlvmFunctionOutputs ( E (Numbers t 32 8), E (Numbers t 32 8), E (Numbers t 32 8), E (Numbers t 32 8)) where
  genOutput (FInfo br sp hp spLim) (r1',r2',r3',r4') = do
    (r1,r2,r3,r4) <- (,,,) <$> compileE r1' <*> compileE r2' <*> compileE r3' <*> compileE r4'
    retptr   <- getReturnPointer (unLabel sp)
    tailcall retptr (br, sp, hp, ireg, ireg, ireg, ireg, ireg, ireg, spLim,r1,r2,r3,r4)

instance AvxConstraint t 32
         => LlvmFunctionOutputs ( E (Numbers t 32 8), E (Numbers t 32 8), E (Numbers t 32 8), E (Numbers t 32 8), E (Numbers t 32 8)) where
  genOutput (FInfo br sp hp spLim) (r1',r2',r3',r4',r5') = do
    (r1,r2,r3,r4,r5) <- (,,,,) <$> compileE r1' <*> compileE r2' <*> compileE r3' <*> compileE r4' <*> compileE r5'
    retptr   <- getReturnPointer (unLabel sp)
    tailcall retptr (br, sp, hp, ireg, ireg, ireg, ireg, ireg, ireg, spLim,r1,r2,r3,r4,r5)

instance AvxConstraint t 32
         => LlvmFunctionOutputs ( E (Numbers t 32 8), E (Numbers t 32 8), E (Numbers t 32 8), E (Numbers t 32 8), E (Numbers t 32 8), E (Numbers t 32 8)) where
  genOutput (FInfo br sp hp spLim) (r1',r2',r3',r4',r5',r6') = do
    (r1,r2,r3,r4,r5,r6) <- (,,,,,) <$> compileE r1' <*> compileE r2' <*> compileE r3' <*> compileE r4' <*> compileE r5' <*> compileE r6'
    retptr   <- getReturnPointer (unLabel sp)
    tailcall retptr (br, sp, hp, ireg, ireg, ireg, ireg, ireg, ireg, spLim,r1,r2,r3,r4,r5,r6)

instance (AvxConstraint t 32, KnownNat n)
         => LlvmFunctionOutputs ( E (Numbers t 32 8), E (Numbers t 32 8), E (Numbers t 32 8), E (Numbers t 32 8), E (Numbers t 32 8), E (Numbers t 32 8)
                                , E (Numbers t 32 n)) where
  genOutput (FInfo br (Labeled sp) hp spLim) (r1',r2',r3',r4',r5',r6',rn') = do
    (r1,r2,r3,r4,r5,r6,rn) <- (,,,,,,) <$> compileE r1' <*> compileE r2' <*> compileE r3' <*> compileE r4' <*> compileE r5' <*> compileE r6' <*> compileE rn'
    sp' <- pushStack sp rn
    retptr <- getReturnPointer sp'
    tailcall retptr (br, sp', hp, ireg, ireg, ireg, ireg, ireg, ireg, spLim,r1,r2,r3,r4,r5,r6)

instance (AvxConstraint t 32, KnownNat n)
         => LlvmFunctionOutputs ( E (Numbers t 32 8), E (Numbers t 32 8), E (Numbers t 32 8), E (Numbers t 32 8), E (Numbers t 32 8), E (Numbers t 32 8)
                                , VarArg n (E (Numbers t 32 8))) where
  genOutput (FInfo br (Labeled sp) hp spLim) (r1',r2',r3',r4',r5',r6',rn') = do
    (r1,r2,r3,r4,r5,r6) <- (,,,,,) <$> compileE r1' <*> compileE r2' <*> compileE r3' <*> compileE r4' <*> compileE r5' <*> compileE r6'
    rn <- mapM compileE rn'
    sp' <- pushArgs sp rn
    retptr <- getReturnPointer sp'
    tailcall retptr (br, sp', hp, ireg, ireg, ireg, ireg, ireg, ireg, spLim,r1,r2,r3,r4,r5,r6)

----------------------------------------------------------------------------------------------------
-- 64 x 4
----------------------------------------------------------------------------------------------------

instance AvxConstraint t 64 => LlvmFunctionInputs (E (Numbers t 64 4)) where
  genInput fname = do
    (br,sp,hp,_r1::HsInt,_r2::HsInt,_r3::HsInt,_r4::HsInt,_r5::HsInt,_r6::HsInt,spLim,a) <- define fname
    return (OpConst a, FInfo br sp hp spLim)

instance AvxConstraint t 64 => LlvmFunctionInputs (E (Numbers t 64 4), E (Numbers t 64 4)) where
  genInput fname = do
    (br,sp,hp,_r1::HsInt,_r2::HsInt,_r3::HsInt,_r4::HsInt,_r5::HsInt,_r6::HsInt,spLim,a,b) <- define fname
    return ((OpConst a, OpConst b), FInfo br sp hp spLim)

instance AvxConstraint t 64 => LlvmFunctionInputs (E (Numbers t 64 4), E (Numbers t 64 4), E (Numbers t 64 4)) where
  genInput fname = do
    (br,sp,hp,_r1::HsInt,_r2::HsInt,_r3::HsInt,_r4::HsInt,_r5::HsInt,_r6::HsInt,spLim,a,b,c) <- define fname
    return ((OpConst a, OpConst b, OpConst c), FInfo br sp hp spLim)

instance AvxConstraint t 64 => LlvmFunctionInputs (E (Numbers t 64 4), E (Numbers t 64 4), E (Numbers t 64 4), E (Numbers t 64 4)) where
  genInput fname = do
    (br,sp,hp,_r1::HsInt,_r2::HsInt,_r3::HsInt,_r4::HsInt,_r5::HsInt,_r6::HsInt,spLim,a,b,c,d) <- define fname
    return ((OpConst a, OpConst b, OpConst c, OpConst d), FInfo br sp hp spLim)

instance AvxConstraint t 64 => LlvmFunctionInputs (E (Numbers t 64 4), E (Numbers t 64 4), E (Numbers t 64 4), E (Numbers t 64 4), E (Numbers t 64 4)) where
  genInput fname = do
    (br,sp,hp,_r1::HsInt,_r2::HsInt,_r3::HsInt,_r4::HsInt,_r5::HsInt,_r6::HsInt,spLim,a,b,c,d,e) <- define fname
    return ((OpConst a, OpConst b, OpConst c, OpConst d, OpConst e), FInfo br sp hp spLim)

instance AvxConstraint t 64 => LlvmFunctionInputs (E (Numbers t 64 4), E (Numbers t 64 4), E (Numbers t 64 4), E (Numbers t 64 4), E (Numbers t 64 4), E (Numbers t 64 4)) where
  genInput fname = do
    (br,sp,hp,_r1::HsInt,_r2::HsInt,_r3::HsInt,_r4::HsInt,_r5::HsInt,_r6::HsInt,spLim,a,b,c,d,e,f) <- define fname
    return ((OpConst a, OpConst b, OpConst c, OpConst d, OpConst e, OpConst f), FInfo br sp hp spLim)

instance (AvxConstraint t 64, KnownNat n)
         => LlvmFunctionInputs ( E (Numbers t 64 4), E (Numbers t 64 4), E (Numbers t 64 4), E (Numbers t 64 4), E (Numbers t 64 4), E (Numbers t 64 4)
                               , E (Numbers t 64 n)) where
  genInput fname = do
    (br,sp::HsPointer,hp,_r1::HsInt,_r2::HsInt,_r3::HsInt,_r4::HsInt,_r5::HsInt,_r6::HsInt,spLim,a,b,c,d,e,f) <- define fname
    (g,sp') <- popStack (unLabel sp)
    return ((OpConst a, OpConst b, OpConst c, OpConst d, OpConst e, OpConst f, OpConst g), FInfo br (Labeled sp') hp spLim)

instance (AvxConstraint t 64, KnownNat n)
         => LlvmFunctionInputs ( E (Numbers t 64 4), E (Numbers t 64 4), E (Numbers t 64 4), E (Numbers t 64 4), E (Numbers t 64 4), E (Numbers t 64 4)
                               , VarArg n (E (Numbers t 64 4))) where
  genInput fname = do
    (br,sp::HsPointer,hp,_r1::HsInt,_r2::HsInt,_r3::HsInt,_r4::HsInt,_r5::HsInt,_r6::HsInt,spLim,a,b,c,d,e,f) <- define fname
    (args,sp') <- popArgs (unLabel sp)
    return ((OpConst a, OpConst b, OpConst c, OpConst d, OpConst e, OpConst f, fmap OpConst args), FInfo br (Labeled sp') hp spLim)


instance AvxConstraint t 64
         => LlvmFunctionOutputs ( E (Numbers t 64 4)) where
  genOutput (FInfo br sp hp spLim) r' = do
    r <- compileE r'
    retptr   <- getReturnPointer (unLabel sp)
    tailcall retptr (br, sp, hp, ireg, ireg, ireg, ireg, ireg, ireg, spLim, r)

instance AvxConstraint t 64
         => LlvmFunctionOutputs ( E (Numbers t 64 4), E (Numbers t 64 4)) where
  genOutput (FInfo br sp hp spLim) (r1',r2') = do
    (r1,r2) <- (,) <$> compileE r1' <*> compileE r2'
    retptr   <- getReturnPointer (unLabel sp)
    tailcall retptr (br, sp, hp, ireg, ireg, ireg, ireg, ireg, ireg, spLim, r1, r2)

instance AvxConstraint t 64
         => LlvmFunctionOutputs ( E (Numbers t 64 4), E (Numbers t 64 4), E (Numbers t 64 4)) where
  genOutput (FInfo br sp hp spLim) (r1',r2',r3') = do
    (r1,r2,r3) <- (,,) <$> compileE r1' <*> compileE r2' <*> compileE r3'
    retptr   <- getReturnPointer (unLabel sp)
    tailcall retptr (br, sp, hp, ireg, ireg, ireg, ireg, ireg, ireg, spLim, r1, r2, r3)

instance AvxConstraint t 64
         => LlvmFunctionOutputs ( E (Numbers t 64 4), E (Numbers t 64 4), E (Numbers t 64 4), E (Numbers t 64 4)) where
  genOutput (FInfo br sp hp spLim) (r1',r2',r3',r4') = do
    (r1,r2,r3,r4) <- (,,,) <$> compileE r1' <*> compileE r2' <*> compileE r3' <*> compileE r4'
    retptr   <- getReturnPointer (unLabel sp)
    tailcall retptr (br, sp, hp, ireg, ireg, ireg, ireg, ireg, ireg, spLim,r1,r2,r3,r4)

instance AvxConstraint t 64
         => LlvmFunctionOutputs ( E (Numbers t 64 4), E (Numbers t 64 4), E (Numbers t 64 4), E (Numbers t 64 4), E (Numbers t 64 4)) where
  genOutput (FInfo br sp hp spLim) (r1',r2',r3',r4',r5') = do
    (r1,r2,r3,r4,r5) <- (,,,,) <$> compileE r1' <*> compileE r2' <*> compileE r3' <*> compileE r4' <*> compileE r5'
    retptr   <- getReturnPointer (unLabel sp)
    tailcall retptr (br, sp, hp, ireg, ireg, ireg, ireg, ireg, ireg, spLim,r1,r2,r3,r4,r5)

instance AvxConstraint t 64
         => LlvmFunctionOutputs ( E (Numbers t 64 4), E (Numbers t 64 4), E (Numbers t 64 4), E (Numbers t 64 4), E (Numbers t 64 4), E (Numbers t 64 4)) where
  genOutput (FInfo br sp hp spLim) (r1',r2',r3',r4',r5',r6') = do
    (r1,r2,r3,r4,r5,r6) <- (,,,,,) <$> compileE r1' <*> compileE r2' <*> compileE r3' <*> compileE r4' <*> compileE r5' <*> compileE r6'
    retptr   <- getReturnPointer (unLabel sp)
    tailcall retptr (br, sp, hp, ireg, ireg, ireg, ireg, ireg, ireg, spLim,r1,r2,r3,r4,r5,r6)

instance (AvxConstraint t 64, KnownNat n)
         => LlvmFunctionOutputs ( E (Numbers t 64 4), E (Numbers t 64 4), E (Numbers t 64 4), E (Numbers t 64 4), E (Numbers t 64 4), E (Numbers t 64 4)
                                , E (Numbers t 64 n)) where
  genOutput (FInfo br (Labeled sp) hp spLim) (r1',r2',r3',r4',r5',r6',rn') = do
    (r1,r2,r3,r4,r5,r6,rn) <- (,,,,,,) <$> compileE r1' <*> compileE r2' <*> compileE r3' <*> compileE r4' <*> compileE r5' <*> compileE r6' <*> compileE rn'
    sp' <- pushStack sp rn
    retptr <- getReturnPointer sp'
    tailcall retptr (br, sp', hp, ireg, ireg, ireg, ireg, ireg, ireg, spLim,r1,r2,r3,r4,r5,r6)

instance (AvxConstraint t 64, KnownNat n)
         => LlvmFunctionOutputs ( E (Numbers t 64 4), E (Numbers t 64 4), E (Numbers t 64 4), E (Numbers t 64 4), E (Numbers t 64 4), E (Numbers t 64 4)
                                , VarArg n (E (Numbers t 64 4))) where
  genOutput (FInfo br (Labeled sp) hp spLim) (r1',r2',r3',r4',r5',r6',rn') = do
    (r1,r2,r3,r4,r5,r6) <- (,,,,,) <$> compileE r1' <*> compileE r2' <*> compileE r3' <*> compileE r4' <*> compileE r5' <*> compileE r6'
    rn <- mapM compileE rn'
    sp' <- pushArgs sp rn
    retptr <- getReturnPointer sp'
    tailcall retptr (br, sp', hp, ireg, ireg, ireg, ireg, ireg, ireg, spLim,r1,r2,r3,r4,r5,r6)


#endif


#ifdef USING_VECTORS_SSE

----------------------------------------------------------------------------------------------------
-- 8 x 16
----------------------------------------------------------------------------------------------------

instance LlvmNum t 8 => LlvmFunctionInputs (E (Numbers t 8 16)) where
  genInput fname = do
    (br,sp,hp,_r1::HsInt,_r2::HsInt,_r3::HsInt,_r4::HsInt,_r5::HsInt,_r6::HsInt,spLim,a) <- define fname
    return (OpConst a, FInfo br sp hp spLim)

instance LlvmNum t 8 => LlvmFunctionInputs (E (Numbers t 8 16), E (Numbers t 8 16)) where
  genInput fname = do
    (br,sp,hp,_r1::HsInt,_r2::HsInt,_r3::HsInt,_r4::HsInt,_r5::HsInt,_r6::HsInt,spLim,a,b) <- define fname
    return ((OpConst a, OpConst b), FInfo br sp hp spLim)

instance LlvmNum t 8 => LlvmFunctionInputs (E (Numbers t 8 16), E (Numbers t 8 16), E (Numbers t 8 16)) where
  genInput fname = do
    (br,sp,hp,_r1::HsInt,_r2::HsInt,_r3::HsInt,_r4::HsInt,_r5::HsInt,_r6::HsInt,spLim,a,b,c) <- define fname
    return ((OpConst a, OpConst b, OpConst c), FInfo br sp hp spLim)

instance LlvmNum t 8 => LlvmFunctionInputs (E (Numbers t 8 16), E (Numbers t 8 16), E (Numbers t 8 16), E (Numbers t 8 16)) where
  genInput fname = do
    (br,sp,hp,_r1::HsInt,_r2::HsInt,_r3::HsInt,_r4::HsInt,_r5::HsInt,_r6::HsInt,spLim,a,b,c,d) <- define fname
    return ((OpConst a, OpConst b, OpConst c, OpConst d), FInfo br sp hp spLim)

instance LlvmNum t 8 => LlvmFunctionInputs (E (Numbers t 8 16), E (Numbers t 8 16), E (Numbers t 8 16), E (Numbers t 8 16), E (Numbers t 8 16)) where
  genInput fname = do
    (br,sp,hp,_r1::HsInt,_r2::HsInt,_r3::HsInt,_r4::HsInt,_r5::HsInt,_r6::HsInt,spLim,a,b,c,d,e) <- define fname
    return ((OpConst a, OpConst b, OpConst c, OpConst d, OpConst e), FInfo br sp hp spLim)

instance LlvmNum t 8 => LlvmFunctionInputs (E (Numbers t 8 16), E (Numbers t 8 16), E (Numbers t 8 16), E (Numbers t 8 16), E (Numbers t 8 16), E (Numbers t 8 16)) where
  genInput fname = do
    (br,sp,hp,_r1::HsInt,_r2::HsInt,_r3::HsInt,_r4::HsInt,_r5::HsInt,_r6::HsInt,spLim,a,b,c,d,e,f) <- define fname
    return ((OpConst a, OpConst b, OpConst c, OpConst d, OpConst e, OpConst f), FInfo br sp hp spLim)

instance (LlvmNum t 8, KnownNat n)
         => LlvmFunctionInputs ( E (Numbers t 8 16), E (Numbers t 8 16), E (Numbers t 8 16), E (Numbers t 8 16), E (Numbers t 8 16), E (Numbers t 8 16)
                               , E (Numbers t 8 n)) where
  genInput fname = do
    (br,sp::HsPointer,hp,_r1::HsInt,_r2::HsInt,_r3::HsInt,_r4::HsInt,_r5::HsInt,_r6::HsInt,spLim,a,b,c,d,e,f) <- define fname
    (g,sp') <- popStack (unLabel sp)
    return ((OpConst a, OpConst b, OpConst c, OpConst d, OpConst e, OpConst f, OpConst g), FInfo br (Labeled sp') hp spLim)

instance (LlvmNum t 8, KnownNat n)
         => LlvmFunctionInputs ( E (Numbers t 8 16), E (Numbers t 8 16), E (Numbers t 8 16), E (Numbers t 8 16), E (Numbers t 8 16), E (Numbers t 8 16)
                               , VarArg n (E (Numbers t 8 16))) where
  genInput fname = do
    (br,sp::HsPointer,hp,_r1::HsInt,_r2::HsInt,_r3::HsInt,_r4::HsInt,_r5::HsInt,_r6::HsInt,spLim,a,b,c,d,e,f) <- define fname
    (args,sp') <- popArgs (unLabel sp)
    return ((OpConst a, OpConst b, OpConst c, OpConst d, OpConst e, OpConst f, fmap OpConst args), FInfo br (Labeled sp') hp spLim)


instance LlvmNum t 8
         => LlvmFunctionOutputs ( E (Numbers t 8 16)) where
  genOutput (FInfo br sp hp spLim) r' = do
    r <- compileE r'
    retptr   <- getReturnPointer (unLabel sp)
    tailcall retptr (br, sp, hp, ireg, ireg, ireg, ireg, ireg, ireg, spLim, r)

instance LlvmNum t 8
         => LlvmFunctionOutputs ( E (Numbers t 8 16), E (Numbers t 8 16)) where
  genOutput (FInfo br sp hp spLim) (r1',r2') = do
    (r1,r2) <- (,) <$> compileE r1' <*> compileE r2'
    retptr   <- getReturnPointer (unLabel sp)
    tailcall retptr (br, sp, hp, ireg, ireg, ireg, ireg, ireg, ireg, spLim, r1, r2)

instance LlvmNum t 8
         => LlvmFunctionOutputs ( E (Numbers t 8 16), E (Numbers t 8 16), E (Numbers t 8 16)) where
  genOutput (FInfo br sp hp spLim) (r1',r2',r3') = do
    (r1,r2,r3) <- (,,) <$> compileE r1' <*> compileE r2' <*> compileE r3'
    retptr   <- getReturnPointer (unLabel sp)
    tailcall retptr (br, sp, hp, ireg, ireg, ireg, ireg, ireg, ireg, spLim, r1, r2, r3)

instance LlvmNum t 8
         => LlvmFunctionOutputs ( E (Numbers t 8 16), E (Numbers t 8 16), E (Numbers t 8 16), E (Numbers t 8 16)) where
  genOutput (FInfo br sp hp spLim) (r1',r2',r3',r4') = do
    (r1,r2,r3,r4) <- (,,,) <$> compileE r1' <*> compileE r2' <*> compileE r3' <*> compileE r4'
    retptr   <- getReturnPointer (unLabel sp)
    tailcall retptr (br, sp, hp, ireg, ireg, ireg, ireg, ireg, ireg, spLim,r1,r2,r3,r4)

instance LlvmNum t 8
         => LlvmFunctionOutputs ( E (Numbers t 8 16), E (Numbers t 8 16), E (Numbers t 8 16), E (Numbers t 8 16), E (Numbers t 8 16)) where
  genOutput (FInfo br sp hp spLim) (r1',r2',r3',r4',r5') = do
    (r1,r2,r3,r4,r5) <- (,,,,) <$> compileE r1' <*> compileE r2' <*> compileE r3' <*> compileE r4' <*> compileE r5'
    retptr   <- getReturnPointer (unLabel sp)
    tailcall retptr (br, sp, hp, ireg, ireg, ireg, ireg, ireg, ireg, spLim,r1,r2,r3,r4,r5)

instance LlvmNum t 8
         => LlvmFunctionOutputs ( E (Numbers t 8 16), E (Numbers t 8 16), E (Numbers t 8 16), E (Numbers t 8 16), E (Numbers t 8 16), E (Numbers t 8 16)) where
  genOutput (FInfo br sp hp spLim) (r1',r2',r3',r4',r5',r6') = do
    (r1,r2,r3,r4,r5,r6) <- (,,,,,) <$> compileE r1' <*> compileE r2' <*> compileE r3' <*> compileE r4' <*> compileE r5' <*> compileE r6'
    retptr   <- getReturnPointer (unLabel sp)
    tailcall retptr (br, sp, hp, ireg, ireg, ireg, ireg, ireg, ireg, spLim,r1,r2,r3,r4,r5,r6)

instance (LlvmNum t 8, KnownNat n)
         => LlvmFunctionOutputs ( E (Numbers t 8 16), E (Numbers t 8 16), E (Numbers t 8 16), E (Numbers t 8 16), E (Numbers t 8 16), E (Numbers t 8 16)
                                , E (Numbers t 8 n)) where
  genOutput (FInfo br (Labeled sp) hp spLim) (r1',r2',r3',r4',r5',r6',rn') = do
    (r1,r2,r3,r4,r5,r6,rn) <- (,,,,,,) <$> compileE r1' <*> compileE r2' <*> compileE r3' <*> compileE r4' <*> compileE r5' <*> compileE r6' <*> compileE rn'
    sp' <- pushStack sp rn
    retptr <- getReturnPointer sp'
    tailcall retptr (br, sp', hp, ireg, ireg, ireg, ireg, ireg, ireg, spLim,r1,r2,r3,r4,r5,r6)

instance (LlvmNum t 8, KnownNat n)
         => LlvmFunctionOutputs ( E (Numbers t 8 16), E (Numbers t 8 16), E (Numbers t 8 16), E (Numbers t 8 16), E (Numbers t 8 16), E (Numbers t 8 16)
                                , VarArg n (E (Numbers t 8 16))) where
  genOutput (FInfo br (Labeled sp) hp spLim) (r1',r2',r3',r4',r5',r6',rn') = do
    (r1,r2,r3,r4,r5,r6) <- (,,,,,) <$> compileE r1' <*> compileE r2' <*> compileE r3' <*> compileE r4' <*> compileE r5' <*> compileE r6'
    rn <- mapM compileE rn'
    sp' <- pushArgs sp rn
    retptr <- getReturnPointer sp'
    tailcall retptr (br, sp', hp, ireg, ireg, ireg, ireg, ireg, ireg, spLim,r1,r2,r3,r4,r5,r6)

----------------------------------------------------------------------------------------------------
-- 16 x 8
----------------------------------------------------------------------------------------------------

instance LlvmNum t 16 => LlvmFunctionInputs (E (Numbers t 16 8)) where
  genInput fname = do
    (br,sp,hp,_r1::HsInt,_r2::HsInt,_r3::HsInt,_r4::HsInt,_r5::HsInt,_r6::HsInt,spLim,a) <- define fname
    return (OpConst a, FInfo br sp hp spLim)

instance LlvmNum t 16 => LlvmFunctionInputs (E (Numbers t 16 8), E (Numbers t 16 8)) where
  genInput fname = do
    (br,sp,hp,_r1::HsInt,_r2::HsInt,_r3::HsInt,_r4::HsInt,_r5::HsInt,_r6::HsInt,spLim,a,b) <- define fname
    return ((OpConst a, OpConst b), FInfo br sp hp spLim)

instance LlvmNum t 16 => LlvmFunctionInputs (E (Numbers t 16 8), E (Numbers t 16 8), E (Numbers t 16 8)) where
  genInput fname = do
    (br,sp,hp,_r1::HsInt,_r2::HsInt,_r3::HsInt,_r4::HsInt,_r5::HsInt,_r6::HsInt,spLim,a,b,c) <- define fname
    return ((OpConst a, OpConst b, OpConst c), FInfo br sp hp spLim)

instance LlvmNum t 16 => LlvmFunctionInputs (E (Numbers t 16 8), E (Numbers t 16 8), E (Numbers t 16 8), E (Numbers t 16 8)) where
  genInput fname = do
    (br,sp,hp,_r1::HsInt,_r2::HsInt,_r3::HsInt,_r4::HsInt,_r5::HsInt,_r6::HsInt,spLim,a,b,c,d) <- define fname
    return ((OpConst a, OpConst b, OpConst c, OpConst d), FInfo br sp hp spLim)

instance LlvmNum t 16 => LlvmFunctionInputs (E (Numbers t 16 8), E (Numbers t 16 8), E (Numbers t 16 8), E (Numbers t 16 8), E (Numbers t 16 8)) where
  genInput fname = do
    (br,sp,hp,_r1::HsInt,_r2::HsInt,_r3::HsInt,_r4::HsInt,_r5::HsInt,_r6::HsInt,spLim,a,b,c,d,e) <- define fname
    return ((OpConst a, OpConst b, OpConst c, OpConst d, OpConst e), FInfo br sp hp spLim)

instance LlvmNum t 16 => LlvmFunctionInputs (E (Numbers t 16 8), E (Numbers t 16 8), E (Numbers t 16 8), E (Numbers t 16 8), E (Numbers t 16 8), E (Numbers t 16 8)) where
  genInput fname = do
    (br,sp,hp,_r1::HsInt,_r2::HsInt,_r3::HsInt,_r4::HsInt,_r5::HsInt,_r6::HsInt,spLim,a,b,c,d,e,f) <- define fname
    return ((OpConst a, OpConst b, OpConst c, OpConst d, OpConst e, OpConst f), FInfo br sp hp spLim)

instance (LlvmNum t 16, KnownNat n)
         => LlvmFunctionInputs ( E (Numbers t 16 8), E (Numbers t 16 8), E (Numbers t 16 8), E (Numbers t 16 8), E (Numbers t 16 8), E (Numbers t 16 8)
                               , E (Numbers t 16 n)) where
  genInput fname = do
    (br,sp::HsPointer,hp,_r1::HsInt,_r2::HsInt,_r3::HsInt,_r4::HsInt,_r5::HsInt,_r6::HsInt,spLim,a,b,c,d,e,f) <- define fname
    (g,sp') <- popStack (unLabel sp)
    return ((OpConst a, OpConst b, OpConst c, OpConst d, OpConst e, OpConst f, OpConst g), FInfo br (Labeled sp') hp spLim)

instance (LlvmNum t 16, KnownNat n)
         => LlvmFunctionInputs ( E (Numbers t 16 8), E (Numbers t 16 8), E (Numbers t 16 8), E (Numbers t 16 8), E (Numbers t 16 8), E (Numbers t 16 8)
                               , VarArg n (E (Numbers t 16 8))) where
  genInput fname = do
    (br,sp::HsPointer,hp,_r1::HsInt,_r2::HsInt,_r3::HsInt,_r4::HsInt,_r5::HsInt,_r6::HsInt,spLim,a,b,c,d,e,f) <- define fname
    (args,sp') <- popArgs (unLabel sp)
    return ((OpConst a, OpConst b, OpConst c, OpConst d, OpConst e, OpConst f, fmap OpConst args), FInfo br (Labeled sp') hp spLim)


instance LlvmNum t 16
         => LlvmFunctionOutputs ( E (Numbers t 16 8)) where
  genOutput (FInfo br sp hp spLim) r' = do
    r <- compileE r'
    retptr   <- getReturnPointer (unLabel sp)
    tailcall retptr (br, sp, hp, ireg, ireg, ireg, ireg, ireg, ireg, spLim, r)

instance LlvmNum t 16
         => LlvmFunctionOutputs ( E (Numbers t 16 8), E (Numbers t 16 8)) where
  genOutput (FInfo br sp hp spLim) (r1',r2') = do
    (r1,r2) <- (,) <$> compileE r1' <*> compileE r2'
    retptr   <- getReturnPointer (unLabel sp)
    tailcall retptr (br, sp, hp, ireg, ireg, ireg, ireg, ireg, ireg, spLim, r1, r2)

instance LlvmNum t 16
         => LlvmFunctionOutputs ( E (Numbers t 16 8), E (Numbers t 16 8), E (Numbers t 16 8)) where
  genOutput (FInfo br sp hp spLim) (r1',r2',r3') = do
    (r1,r2,r3) <- (,,) <$> compileE r1' <*> compileE r2' <*> compileE r3'
    retptr   <- getReturnPointer (unLabel sp)
    tailcall retptr (br, sp, hp, ireg, ireg, ireg, ireg, ireg, ireg, spLim, r1, r2, r3)

instance LlvmNum t 16
         => LlvmFunctionOutputs ( E (Numbers t 16 8), E (Numbers t 16 8), E (Numbers t 16 8), E (Numbers t 16 8)) where
  genOutput (FInfo br sp hp spLim) (r1',r2',r3',r4') = do
    (r1,r2,r3,r4) <- (,,,) <$> compileE r1' <*> compileE r2' <*> compileE r3' <*> compileE r4'
    retptr   <- getReturnPointer (unLabel sp)
    tailcall retptr (br, sp, hp, ireg, ireg, ireg, ireg, ireg, ireg, spLim,r1,r2,r3,r4)

instance LlvmNum t 16
         => LlvmFunctionOutputs ( E (Numbers t 16 8), E (Numbers t 16 8), E (Numbers t 16 8), E (Numbers t 16 8), E (Numbers t 16 8)) where
  genOutput (FInfo br sp hp spLim) (r1',r2',r3',r4',r5') = do
    (r1,r2,r3,r4,r5) <- (,,,,) <$> compileE r1' <*> compileE r2' <*> compileE r3' <*> compileE r4' <*> compileE r5'
    retptr   <- getReturnPointer (unLabel sp)
    tailcall retptr (br, sp, hp, ireg, ireg, ireg, ireg, ireg, ireg, spLim,r1,r2,r3,r4,r5)

instance LlvmNum t 16
         => LlvmFunctionOutputs ( E (Numbers t 16 8), E (Numbers t 16 8), E (Numbers t 16 8), E (Numbers t 16 8), E (Numbers t 16 8), E (Numbers t 16 8)) where
  genOutput (FInfo br sp hp spLim) (r1',r2',r3',r4',r5',r6') = do
    (r1,r2,r3,r4,r5,r6) <- (,,,,,) <$> compileE r1' <*> compileE r2' <*> compileE r3' <*> compileE r4' <*> compileE r5' <*> compileE r6'
    retptr   <- getReturnPointer (unLabel sp)
    tailcall retptr (br, sp, hp, ireg, ireg, ireg, ireg, ireg, ireg, spLim,r1,r2,r3,r4,r5,r6)

instance (LlvmNum t 16, KnownNat n)
         => LlvmFunctionOutputs ( E (Numbers t 16 8), E (Numbers t 16 8), E (Numbers t 16 8), E (Numbers t 16 8), E (Numbers t 16 8), E (Numbers t 16 8)
                                , E (Numbers t 16 n)) where
  genOutput (FInfo br (Labeled sp) hp spLim) (r1',r2',r3',r4',r5',r6',rn') = do
    (r1,r2,r3,r4,r5,r6,rn) <- (,,,,,,) <$> compileE r1' <*> compileE r2' <*> compileE r3' <*> compileE r4' <*> compileE r5' <*> compileE r6' <*> compileE rn'
    sp' <- pushStack sp rn
    retptr <- getReturnPointer sp'
    tailcall retptr (br, sp', hp, ireg, ireg, ireg, ireg, ireg, ireg, spLim,r1,r2,r3,r4,r5,r6)

instance (LlvmNum t 16, KnownNat n)
         => LlvmFunctionOutputs ( E (Numbers t 16 8), E (Numbers t 16 8), E (Numbers t 16 8), E (Numbers t 16 8), E (Numbers t 16 8), E (Numbers t 16 8)
                                , VarArg n (E (Numbers t 16 8))) where
  genOutput (FInfo br (Labeled sp) hp spLim) (r1',r2',r3',r4',r5',r6',rn') = do
    (r1,r2,r3,r4,r5,r6) <- (,,,,,) <$> compileE r1' <*> compileE r2' <*> compileE r3' <*> compileE r4' <*> compileE r5' <*> compileE r6'
    rn <- mapM compileE rn'
    sp' <- pushArgs sp rn
    retptr <- getReturnPointer sp'
    tailcall retptr (br, sp', hp, ireg, ireg, ireg, ireg, ireg, ireg, spLim,r1,r2,r3,r4,r5,r6)

----------------------------------------------------------------------------------------------------
-- 32 x 4
----------------------------------------------------------------------------------------------------

instance LlvmNum t 32 => LlvmFunctionInputs (E (Numbers t 32 4)) where
  genInput fname = do
    (br,sp,hp,_r1::HsInt,_r2::HsInt,_r3::HsInt,_r4::HsInt,_r5::HsInt,_r6::HsInt,spLim,a) <- define fname
    return (OpConst a, FInfo br sp hp spLim)

instance LlvmNum t 32 => LlvmFunctionInputs (E (Numbers t 32 4), E (Numbers t 32 4)) where
  genInput fname = do
    (br,sp,hp,_r1::HsInt,_r2::HsInt,_r3::HsInt,_r4::HsInt,_r5::HsInt,_r6::HsInt,spLim,a,b) <- define fname
    return ((OpConst a, OpConst b), FInfo br sp hp spLim)

instance LlvmNum t 32 => LlvmFunctionInputs (E (Numbers t 32 4), E (Numbers t 32 4), E (Numbers t 32 4)) where
  genInput fname = do
    (br,sp,hp,_r1::HsInt,_r2::HsInt,_r3::HsInt,_r4::HsInt,_r5::HsInt,_r6::HsInt,spLim,a,b,c) <- define fname
    return ((OpConst a, OpConst b, OpConst c), FInfo br sp hp spLim)

instance LlvmNum t 32 => LlvmFunctionInputs (E (Numbers t 32 4), E (Numbers t 32 4), E (Numbers t 32 4), E (Numbers t 32 4)) where
  genInput fname = do
    (br,sp,hp,_r1::HsInt,_r2::HsInt,_r3::HsInt,_r4::HsInt,_r5::HsInt,_r6::HsInt,spLim,a,b,c,d) <- define fname
    return ((OpConst a, OpConst b, OpConst c, OpConst d), FInfo br sp hp spLim)

instance LlvmNum t 32 => LlvmFunctionInputs (E (Numbers t 32 4), E (Numbers t 32 4), E (Numbers t 32 4), E (Numbers t 32 4), E (Numbers t 32 4)) where
  genInput fname = do
    (br,sp,hp,_r1::HsInt,_r2::HsInt,_r3::HsInt,_r4::HsInt,_r5::HsInt,_r6::HsInt,spLim,a,b,c,d,e) <- define fname
    return ((OpConst a, OpConst b, OpConst c, OpConst d, OpConst e), FInfo br sp hp spLim)

instance LlvmNum t 32 => LlvmFunctionInputs (E (Numbers t 32 4), E (Numbers t 32 4), E (Numbers t 32 4), E (Numbers t 32 4), E (Numbers t 32 4), E (Numbers t 32 4)) where
  genInput fname = do
    (br,sp,hp,_r1::HsInt,_r2::HsInt,_r3::HsInt,_r4::HsInt,_r5::HsInt,_r6::HsInt,spLim,a,b,c,d,e,f) <- define fname
    return ((OpConst a, OpConst b, OpConst c, OpConst d, OpConst e, OpConst f), FInfo br sp hp spLim)

instance (LlvmNum t 32, KnownNat n)
         => LlvmFunctionInputs ( E (Numbers t 32 4), E (Numbers t 32 4), E (Numbers t 32 4), E (Numbers t 32 4), E (Numbers t 32 4), E (Numbers t 32 4)
                               , E (Numbers t 32 n)) where
  genInput fname = do
    (br,sp::HsPointer,hp,_r1::HsInt,_r2::HsInt,_r3::HsInt,_r4::HsInt,_r5::HsInt,_r6::HsInt,spLim,a,b,c,d,e,f) <- define fname
    (g,sp') <- popStack (unLabel sp)
    return ((OpConst a, OpConst b, OpConst c, OpConst d, OpConst e, OpConst f, OpConst g), FInfo br (Labeled sp') hp spLim)

instance (LlvmNum t 32, KnownNat n)
         => LlvmFunctionInputs ( E (Numbers t 32 4), E (Numbers t 32 4), E (Numbers t 32 4), E (Numbers t 32 4), E (Numbers t 32 4), E (Numbers t 32 4)
                               , VarArg n (E (Numbers t 32 4))) where
  genInput fname = do
    (br,sp::HsPointer,hp,_r1::HsInt,_r2::HsInt,_r3::HsInt,_r4::HsInt,_r5::HsInt,_r6::HsInt,spLim,a,b,c,d,e,f) <- define fname
    (args,sp') <- popArgs (unLabel sp)
    return ((OpConst a, OpConst b, OpConst c, OpConst d, OpConst e, OpConst f, fmap OpConst args), FInfo br (Labeled sp') hp spLim)


instance LlvmNum t 32
         => LlvmFunctionOutputs ( E (Numbers t 32 4)) where
  genOutput (FInfo br sp hp spLim) r' = do
    r <- compileE r'
    retptr   <- getReturnPointer (unLabel sp)
    tailcall retptr (br, sp, hp, ireg, ireg, ireg, ireg, ireg, ireg, spLim, r)

instance LlvmNum t 32
         => LlvmFunctionOutputs ( E (Numbers t 32 4), E (Numbers t 32 4)) where
  genOutput (FInfo br sp hp spLim) (r1',r2') = do
    (r1,r2) <- (,) <$> compileE r1' <*> compileE r2'
    retptr   <- getReturnPointer (unLabel sp)
    tailcall retptr (br, sp, hp, ireg, ireg, ireg, ireg, ireg, ireg, spLim, r1, r2)

instance LlvmNum t 32
         => LlvmFunctionOutputs ( E (Numbers t 32 4), E (Numbers t 32 4), E (Numbers t 32 4)) where
  genOutput (FInfo br sp hp spLim) (r1',r2',r3') = do
    (r1,r2,r3) <- (,,) <$> compileE r1' <*> compileE r2' <*> compileE r3'
    retptr   <- getReturnPointer (unLabel sp)
    tailcall retptr (br, sp, hp, ireg, ireg, ireg, ireg, ireg, ireg, spLim, r1, r2, r3)

instance LlvmNum t 32
         => LlvmFunctionOutputs ( E (Numbers t 32 4), E (Numbers t 32 4), E (Numbers t 32 4), E (Numbers t 32 4)) where
  genOutput (FInfo br sp hp spLim) (r1',r2',r3',r4') = do
    (r1,r2,r3,r4) <- (,,,) <$> compileE r1' <*> compileE r2' <*> compileE r3' <*> compileE r4'
    retptr   <- getReturnPointer (unLabel sp)
    tailcall retptr (br, sp, hp, ireg, ireg, ireg, ireg, ireg, ireg, spLim,r1,r2,r3,r4)

instance LlvmNum t 32
         => LlvmFunctionOutputs ( E (Numbers t 32 4), E (Numbers t 32 4), E (Numbers t 32 4), E (Numbers t 32 4), E (Numbers t 32 4)) where
  genOutput (FInfo br sp hp spLim) (r1',r2',r3',r4',r5') = do
    (r1,r2,r3,r4,r5) <- (,,,,) <$> compileE r1' <*> compileE r2' <*> compileE r3' <*> compileE r4' <*> compileE r5'
    retptr   <- getReturnPointer (unLabel sp)
    tailcall retptr (br, sp, hp, ireg, ireg, ireg, ireg, ireg, ireg, spLim,r1,r2,r3,r4,r5)

instance LlvmNum t 32
         => LlvmFunctionOutputs ( E (Numbers t 32 4), E (Numbers t 32 4), E (Numbers t 32 4), E (Numbers t 32 4), E (Numbers t 32 4), E (Numbers t 32 4)) where
  genOutput (FInfo br sp hp spLim) (r1',r2',r3',r4',r5',r6') = do
    (r1,r2,r3,r4,r5,r6) <- (,,,,,) <$> compileE r1' <*> compileE r2' <*> compileE r3' <*> compileE r4' <*> compileE r5' <*> compileE r6'
    retptr   <- getReturnPointer (unLabel sp)
    tailcall retptr (br, sp, hp, ireg, ireg, ireg, ireg, ireg, ireg, spLim,r1,r2,r3,r4,r5,r6)

instance (LlvmNum t 32, KnownNat n)
         => LlvmFunctionOutputs ( E (Numbers t 32 4), E (Numbers t 32 4), E (Numbers t 32 4), E (Numbers t 32 4), E (Numbers t 32 4), E (Numbers t 32 4)
                                , E (Numbers t 32 n)) where
  genOutput (FInfo br (Labeled sp) hp spLim) (r1',r2',r3',r4',r5',r6',rn') = do
    (r1,r2,r3,r4,r5,r6,rn) <- (,,,,,,) <$> compileE r1' <*> compileE r2' <*> compileE r3' <*> compileE r4' <*> compileE r5' <*> compileE r6' <*> compileE rn'
    sp' <- pushStack sp rn
    retptr <- getReturnPointer sp'
    tailcall retptr (br, sp', hp, ireg, ireg, ireg, ireg, ireg, ireg, spLim,r1,r2,r3,r4,r5,r6)

instance (LlvmNum t 32, KnownNat n)
         => LlvmFunctionOutputs ( E (Numbers t 32 4), E (Numbers t 32 4), E (Numbers t 32 4), E (Numbers t 32 4), E (Numbers t 32 4), E (Numbers t 32 4)
                                , VarArg n (E (Numbers t 32 4))) where
  genOutput (FInfo br (Labeled sp) hp spLim) (r1',r2',r3',r4',r5',r6',rn') = do
    (r1,r2,r3,r4,r5,r6) <- (,,,,,) <$> compileE r1' <*> compileE r2' <*> compileE r3' <*> compileE r4' <*> compileE r5' <*> compileE r6'
    rn <- mapM compileE rn'
    sp' <- pushArgs sp rn
    retptr <- getReturnPointer sp'
    tailcall retptr (br, sp', hp, ireg, ireg, ireg, ireg, ireg, ireg, spLim,r1,r2,r3,r4,r5,r6)

----------------------------------------------------------------------------------------------------
-- 64 x 2
----------------------------------------------------------------------------------------------------

instance LlvmNum t 64 => LlvmFunctionInputs (E (Numbers t 64 2)) where
  genInput fname = do
    (br,sp,hp,_r1::HsInt,_r2::HsInt,_r3::HsInt,_r4::HsInt,_r5::HsInt,_r6::HsInt,spLim,a) <- define fname
    return (OpConst a, FInfo br sp hp spLim)

instance LlvmNum t 64 => LlvmFunctionInputs (E (Numbers t 64 2), E (Numbers t 64 2)) where
  genInput fname = do
    (br,sp,hp,_r1::HsInt,_r2::HsInt,_r3::HsInt,_r4::HsInt,_r5::HsInt,_r6::HsInt,spLim,a,b) <- define fname
    return ((OpConst a, OpConst b), FInfo br sp hp spLim)

instance LlvmNum t 64 => LlvmFunctionInputs (E (Numbers t 64 2), E (Numbers t 64 2), E (Numbers t 64 2)) where
  genInput fname = do
    (br,sp,hp,_r1::HsInt,_r2::HsInt,_r3::HsInt,_r4::HsInt,_r5::HsInt,_r6::HsInt,spLim,a,b,c) <- define fname
    return ((OpConst a, OpConst b, OpConst c), FInfo br sp hp spLim)

instance LlvmNum t 64 => LlvmFunctionInputs (E (Numbers t 64 2), E (Numbers t 64 2), E (Numbers t 64 2), E (Numbers t 64 2)) where
  genInput fname = do
    (br,sp,hp,_r1::HsInt,_r2::HsInt,_r3::HsInt,_r4::HsInt,_r5::HsInt,_r6::HsInt,spLim,a,b,c,d) <- define fname
    return ((OpConst a, OpConst b, OpConst c, OpConst d), FInfo br sp hp spLim)

instance LlvmNum t 64 => LlvmFunctionInputs (E (Numbers t 64 2), E (Numbers t 64 2), E (Numbers t 64 2), E (Numbers t 64 2), E (Numbers t 64 2)) where
  genInput fname = do
    (br,sp,hp,_r1::HsInt,_r2::HsInt,_r3::HsInt,_r4::HsInt,_r5::HsInt,_r6::HsInt,spLim,a,b,c,d,e) <- define fname
    return ((OpConst a, OpConst b, OpConst c, OpConst d, OpConst e), FInfo br sp hp spLim)

instance LlvmNum t 64 => LlvmFunctionInputs (E (Numbers t 64 2), E (Numbers t 64 2), E (Numbers t 64 2), E (Numbers t 64 2), E (Numbers t 64 2), E (Numbers t 64 2)) where
  genInput fname = do
    (br,sp,hp,_r1::HsInt,_r2::HsInt,_r3::HsInt,_r4::HsInt,_r5::HsInt,_r6::HsInt,spLim,a,b,c,d,e,f) <- define fname
    return ((OpConst a, OpConst b, OpConst c, OpConst d, OpConst e, OpConst f), FInfo br sp hp spLim)

instance (LlvmNum t 64, KnownNat n)
         => LlvmFunctionInputs ( E (Numbers t 64 2), E (Numbers t 64 2), E (Numbers t 64 2), E (Numbers t 64 2), E (Numbers t 64 2), E (Numbers t 64 2)
                               , E (Numbers t 64 n)) where
  genInput fname = do
    (br,sp::HsPointer,hp,_r1::HsInt,_r2::HsInt,_r3::HsInt,_r4::HsInt,_r5::HsInt,_r6::HsInt,spLim,a,b,c,d,e,f) <- define fname
    (g,sp') <- popStack (unLabel sp)
    return ((OpConst a, OpConst b, OpConst c, OpConst d, OpConst e, OpConst f, OpConst g), FInfo br (Labeled sp') hp spLim)

instance (LlvmNum t 64, KnownNat n)
         => LlvmFunctionInputs ( E (Numbers t 64 2), E (Numbers t 64 2), E (Numbers t 64 2), E (Numbers t 64 2), E (Numbers t 64 2), E (Numbers t 64 2)
                               , VarArg n (E (Numbers t 64 2))) where
  genInput fname = do
    (br,sp::HsPointer,hp,_r1::HsInt,_r2::HsInt,_r3::HsInt,_r4::HsInt,_r5::HsInt,_r6::HsInt,spLim,a,b,c,d,e,f) <- define fname
    (args,sp') <- popArgs (unLabel sp)
    return ((OpConst a, OpConst b, OpConst c, OpConst d, OpConst e, OpConst f, fmap OpConst args), FInfo br (Labeled sp') hp spLim)


instance LlvmNum t 64
         => LlvmFunctionOutputs ( E (Numbers t 64 2)) where
  genOutput (FInfo br sp hp spLim) r' = do
    r <- compileE r'
    retptr   <- getReturnPointer (unLabel sp)
    tailcall retptr (br, sp, hp, ireg, ireg, ireg, ireg, ireg, ireg, spLim, r)

instance LlvmNum t 64
         => LlvmFunctionOutputs ( E (Numbers t 64 2), E (Numbers t 64 2)) where
  genOutput (FInfo br sp hp spLim) (r1',r2') = do
    (r1,r2) <- (,) <$> compileE r1' <*> compileE r2'
    retptr   <- getReturnPointer (unLabel sp)
    tailcall retptr (br, sp, hp, ireg, ireg, ireg, ireg, ireg, ireg, spLim, r1, r2)

instance LlvmNum t 64
         => LlvmFunctionOutputs ( E (Numbers t 64 2), E (Numbers t 64 2), E (Numbers t 64 2)) where
  genOutput (FInfo br sp hp spLim) (r1',r2',r3') = do
    (r1,r2,r3) <- (,,) <$> compileE r1' <*> compileE r2' <*> compileE r3'
    retptr   <- getReturnPointer (unLabel sp)
    tailcall retptr (br, sp, hp, ireg, ireg, ireg, ireg, ireg, ireg, spLim, r1, r2, r3)

instance LlvmNum t 64
         => LlvmFunctionOutputs ( E (Numbers t 64 2), E (Numbers t 64 2), E (Numbers t 64 2), E (Numbers t 64 2)) where
  genOutput (FInfo br sp hp spLim) (r1',r2',r3',r4') = do
    (r1,r2,r3,r4) <- (,,,) <$> compileE r1' <*> compileE r2' <*> compileE r3' <*> compileE r4'
    retptr   <- getReturnPointer (unLabel sp)
    tailcall retptr (br, sp, hp, ireg, ireg, ireg, ireg, ireg, ireg, spLim,r1,r2,r3,r4)

instance LlvmNum t 64
         => LlvmFunctionOutputs ( E (Numbers t 64 2), E (Numbers t 64 2), E (Numbers t 64 2), E (Numbers t 64 2), E (Numbers t 64 2)) where
  genOutput (FInfo br sp hp spLim) (r1',r2',r3',r4',r5') = do
    (r1,r2,r3,r4,r5) <- (,,,,) <$> compileE r1' <*> compileE r2' <*> compileE r3' <*> compileE r4' <*> compileE r5'
    retptr   <- getReturnPointer (unLabel sp)
    tailcall retptr (br, sp, hp, ireg, ireg, ireg, ireg, ireg, ireg, spLim,r1,r2,r3,r4,r5)

instance LlvmNum t 64
         => LlvmFunctionOutputs ( E (Numbers t 64 2), E (Numbers t 64 2), E (Numbers t 64 2), E (Numbers t 64 2), E (Numbers t 64 2), E (Numbers t 64 2)) where
  genOutput (FInfo br sp hp spLim) (r1',r2',r3',r4',r5',r6') = do
    (r1,r2,r3,r4,r5,r6) <- (,,,,,) <$> compileE r1' <*> compileE r2' <*> compileE r3' <*> compileE r4' <*> compileE r5' <*> compileE r6'
    retptr   <- getReturnPointer (unLabel sp)
    tailcall retptr (br, sp, hp, ireg, ireg, ireg, ireg, ireg, ireg, spLim,r1,r2,r3,r4,r5,r6)

instance (LlvmNum t 64, KnownNat n)
         => LlvmFunctionOutputs ( E (Numbers t 64 2), E (Numbers t 64 2), E (Numbers t 64 2), E (Numbers t 64 2), E (Numbers t 64 2), E (Numbers t 64 2)
                                , E (Numbers t 64 n)) where
  genOutput (FInfo br (Labeled sp) hp spLim) (r1',r2',r3',r4',r5',r6',rn') = do
    (r1,r2,r3,r4,r5,r6,rn) <- (,,,,,,) <$> compileE r1' <*> compileE r2' <*> compileE r3' <*> compileE r4' <*> compileE r5' <*> compileE r6' <*> compileE rn'
    sp' <- pushStack sp rn
    retptr <- getReturnPointer sp'
    tailcall retptr (br, sp', hp, ireg, ireg, ireg, ireg, ireg, ireg, spLim,r1,r2,r3,r4,r5,r6)

instance (LlvmNum t 64, KnownNat n)
         => LlvmFunctionOutputs ( E (Numbers t 64 2), E (Numbers t 64 2), E (Numbers t 64 2), E (Numbers t 64 2), E (Numbers t 64 2), E (Numbers t 64 2)
                                , VarArg n (E (Numbers t 64 2))) where
  genOutput (FInfo br (Labeled sp) hp spLim) (r1',r2',r3',r4',r5',r6',rn') = do
    (r1,r2,r3,r4,r5,r6) <- (,,,,,) <$> compileE r1' <*> compileE r2' <*> compileE r3' <*> compileE r4' <*> compileE r5' <*> compileE r6'
    rn <- mapM compileE rn'
    sp' <- pushArgs sp rn
    retptr <- getReturnPointer sp'
    tailcall retptr (br, sp', hp, ireg, ireg, ireg, ireg, ireg, ireg, spLim,r1,r2,r3,r4,r5,r6)

#endif






