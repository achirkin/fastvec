{-# LANGUAGE MagicHash, UnboxedTuples #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeFamilies, DataKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Geometry.Instances.Floating4
-- Copyright   :  Copyright (C) 2015 Artem M. Chirkin <chirkin@arch.ethz.ch>
-- License     :  BSD3
--
-- Maintainer  :  Artem M. Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  Experimental
-- Portability :
--
--
-----------------------------------------------------------------------------

module Data.Geometry.Instances.Floating4 () where


import GHC.Exts

import Foreign.C.Types

import Data.Geometry.VectorMath
import Data.Geometry.Prim.FloatX4
import Data.Geometry.Types
import Data.Geometry.Instances.Num ()
import Data.Geometry.Instances.Fractional ()

#define emptyc(x) x

-- params: type, vectortype, elPrimType, Vector constr, Elem constr, Elem newtype, num ending
#define FLOATING4V(T,ET,VT,VC,EC,EC2,e)                                   \
instance Floating (Vector 4 T) where {     \
    {-# INLINE sqrt #-};      \
    sqrt (VC a) = VC (sqrt/**/VT a);      \
    {-# INLINE pi #-};      \
    pi = case pi of      \
        {EC2(EC f) -> VC (broadcast/**/VT f)};      \
    {-# INLINE exp #-};      \
    exp (VC a) = case unpack/**/VT a of      \
        {(# x, y, z, t #) -> VC (pack/**/VT (# exp/**/ET x, exp/**/ET y, exp/**/ET z, exp/**/ET t #))};      \
    {-# INLINE log #-};      \
    log (VC a) = case unpack/**/VT a of      \
        {(# x, y, z, t #) -> VC (pack/**/VT (# log/**/ET x, log/**/ET y, log/**/ET z, log/**/ET t #))};      \
    {-# INLINE sin #-};      \
    sin (VC a) = case unpack/**/VT a of      \
        {(# x, y, z, t #) -> VC (pack/**/VT (# sin/**/ET x, sin/**/ET y, sin/**/ET z, sin/**/ET t #))};      \
    {-# INLINE cos #-};      \
    cos (VC a) = case unpack/**/VT a of      \
        {(# x, y, z, t #) -> VC (pack/**/VT (# cos/**/ET x, cos/**/ET y, cos/**/ET z, cos/**/ET t #))};      \
    {-# INLINE asin #-};      \
    asin (VC a) = case unpack/**/VT a of      \
        {(# x, y, z, t #) -> VC (pack/**/VT (# asin/**/ET x, asin/**/ET y, asin/**/ET z, asin/**/ET t #))};      \
    {-# INLINE acos #-};      \
    acos (VC a) = case unpack/**/VT a of      \
        {(# x, y, z, t #) -> VC (pack/**/VT (# acos/**/ET x, acos/**/ET y, acos/**/ET z, acos/**/ET t #))};      \
    {-# INLINE atan #-};      \
    atan (VC a) = case unpack/**/VT a of      \
        {(# x, y, z, t #) -> VC (pack/**/VT (# atan/**/ET x, atan/**/ET y, atan/**/ET z, atan/**/ET t #))};      \
    {-# INLINE sinh #-};      \
    sinh (VC a) = case unpack/**/VT a of      \
        {(# x, y, z, t #) -> VC (pack/**/VT (# sinh/**/ET x, sin/**/ET y, sin/**/ET z, sin/**/ET t #))};      \
    {-# INLINE cosh #-};      \
    cosh (VC a) = case unpack/**/VT a of      \
        {(# x, y, z, t #) -> VC (pack/**/VT (# cosh/**/ET x, cos/**/ET y, cos/**/ET z, cos/**/ET t #))};      \
    {-# INLINE tanh #-};      \
    tanh (VC a) = case unpack/**/VT a of      \
        {(# x, y, z, t #) -> VC (pack/**/VT (# tanh/**/ET x, tanh/**/ET y, tanh/**/ET z, tanh/**/ET t #))};      \
    {-# INLINE asinh #-};      \
    asinh (VC a) = case unpack/**/VT (      \
                      plus/**/VT a (      \
                        sqrt/**/VT (      \
                            plus/**/VT (times/**/VT a a)      \
                                         (broadcast/**/VT 1/**/e)      \
                        )      \
                      )      \
                    ) of      \
        {(# x, y, z, t #) -> VC (pack/**/VT (# log/**/ET x, log/**/ET y, log/**/ET z, log/**/ET t #))};      \
    {-# INLINE acosh #-};      \
    acosh (VC a) = case unpack/**/VT (      \
                      plus/**/VT a (      \
                        sqrt/**/VT (      \
                            minus/**/VT (times/**/VT a a)      \
                                          (broadcast/**/VT 1/**/e)      \
                        )      \
                      )      \
                    ) of      \
        {(# x, y, z, t #) -> VC (pack/**/VT (# log/**/ET x, log/**/ET y, log/**/ET z, log/**/ET t #)) };     \
    {-# INLINE atanh #-};      \
    atanh (VC a) | one <- broadcast/**/VT 1/**/e = case unpack/**/VT (      \
                        divide/**/VT (plus/**/VT one a)      \
                                       (minus/**/VT one a)      \
                    ) of      \
        {(# x, y, z, t #) -> VC (times/**/VT (broadcast/**/VT 0.5#)      \
                    (pack/**/VT (# log/**/ET x, log/**/ET y, log/**/ET z, log/**/ET t #))      \
                    )}     }


-- params: type, vectortype, elPrimType, Matrix constr, Elem constr, Elem newtype, num ending
#define FLOATING4M(T,ET,VT,MC,EC,EC2,e) \
instance Floating (Matrix 4 T) where {     \
    {-# INLINE sqrt #-};      \
    sqrt (MC a1 a2 a3 a4) = MC (sqrt/**/VT a1)      \
                                 (sqrt/**/VT a2)      \
                                 (sqrt/**/VT a3)      \
                                 (sqrt/**/VT a4);      \
    {-# INLINE pi #-};      \
    pi = case pi of      \
        {EC2(EC f) -> case broadcast/**/VT f of b -> MC b b b b};     \
    {-# INLINE exp #-};      \
    exp (MC a1 a2 a3 a4) = case (# unpack/**/VT a1      \
                                 , unpack/**/VT a2      \
                                 , unpack/**/VT a3      \
                                 , unpack/**/VT a4 #) of      \
        {(# (# x11, x21, x31, x41 #)      \
         ,(# x12, x22, x32, x42 #)      \
         ,(# x13, x23, x33, x43 #)      \
         ,(# x14, x24, x34, x44 #)      \
         #) -> MC (pack/**/VT (# exp/**/ET x11, exp/**/ET x21, exp/**/ET x31, exp/**/ET x41 #))      \
                  (pack/**/VT (# exp/**/ET x12, exp/**/ET x22, exp/**/ET x32, exp/**/ET x42 #))      \
                  (pack/**/VT (# exp/**/ET x13, exp/**/ET x23, exp/**/ET x33, exp/**/ET x43 #))      \
                  (pack/**/VT (# exp/**/ET x14, exp/**/ET x24, exp/**/ET x34, exp/**/ET x44 #))};      \
    {-# INLINE log #-};      \
    log (MC a1 a2 a3 a4) = case (# unpack/**/VT a1      \
                                 , unpack/**/VT a2      \
                                 , unpack/**/VT a3      \
                                 , unpack/**/VT a4 #) of      \
        {(# (# x11, x21, x31, x41 #)      \
         ,(# x12, x22, x32, x42 #)      \
         ,(# x13, x23, x33, x43 #)      \
         ,(# x14, x24, x34, x44 #)      \
         #) -> MC (pack/**/VT (# log/**/ET x11, log/**/ET x21, log/**/ET x31, log/**/ET x41 #))      \
                  (pack/**/VT (# log/**/ET x12, log/**/ET x22, log/**/ET x32, log/**/ET x42 #))      \
                  (pack/**/VT (# log/**/ET x13, log/**/ET x23, log/**/ET x33, log/**/ET x43 #))      \
                  (pack/**/VT (# log/**/ET x14, log/**/ET x24, log/**/ET x34, log/**/ET x44 #))};     \
    {-# INLINE sin #-};      \
    sin (MC a1 a2 a3 a4) = case (# unpack/**/VT a1      \
                                 , unpack/**/VT a2      \
                                 , unpack/**/VT a3      \
                                 , unpack/**/VT a4 #) of      \
        {(# (# x11, x21, x31, x41 #)      \
         ,(# x12, x22, x32, x42 #)      \
         ,(# x13, x23, x33, x43 #)      \
         ,(# x14, x24, x34, x44 #)      \
         #) -> MC (pack/**/VT (# sin/**/ET x11, sin/**/ET x21, sin/**/ET x31, sin/**/ET x41 #))      \
                  (pack/**/VT (# sin/**/ET x12, sin/**/ET x22, sin/**/ET x32, sin/**/ET x42 #))      \
                  (pack/**/VT (# sin/**/ET x13, sin/**/ET x23, sin/**/ET x33, sin/**/ET x43 #))      \
                  (pack/**/VT (# sin/**/ET x14, sin/**/ET x24, sin/**/ET x34, sin/**/ET x44 #))};      \
    {-# INLINE cos #-};      \
    cos (MC a1 a2 a3 a4) = case (# unpack/**/VT a1      \
                                 , unpack/**/VT a2      \
                                 , unpack/**/VT a3      \
                                 , unpack/**/VT a4 #) of      \
        {(# (# x11, x21, x31, x41 #)      \
         ,(# x12, x22, x32, x42 #)      \
         ,(# x13, x23, x33, x43 #)      \
         ,(# x14, x24, x34, x44 #)      \
         #) -> MC (pack/**/VT (# cos/**/ET x11, cos/**/ET x21, cos/**/ET x31, cos/**/ET x41 #))      \
                  (pack/**/VT (# cos/**/ET x12, cos/**/ET x22, cos/**/ET x32, cos/**/ET x42 #))      \
                  (pack/**/VT (# cos/**/ET x13, cos/**/ET x23, cos/**/ET x33, cos/**/ET x43 #))      \
                  (pack/**/VT (# cos/**/ET x14, cos/**/ET x24, cos/**/ET x34, cos/**/ET x44 #))};      \
    {-# INLINE asin #-};      \
    asin (MC a1 a2 a3 a4) = case (# unpack/**/VT a1      \
                                 , unpack/**/VT a2      \
                                 , unpack/**/VT a3      \
                                 , unpack/**/VT a4 #) of      \
        {(# (# x11, x21, x31, x41 #)      \
         ,(# x12, x22, x32, x42 #)      \
         ,(# x13, x23, x33, x43 #)      \
         ,(# x14, x24, x34, x44 #)      \
         #) -> MC (pack/**/VT (# asin/**/ET x11, asin/**/ET x21, asin/**/ET x31, asin/**/ET x41 #))      \
                  (pack/**/VT (# asin/**/ET x12, asin/**/ET x22, asin/**/ET x32, asin/**/ET x42 #))      \
                  (pack/**/VT (# asin/**/ET x13, asin/**/ET x23, asin/**/ET x33, asin/**/ET x43 #))      \
                  (pack/**/VT (# asin/**/ET x14, asin/**/ET x24, asin/**/ET x34, asin/**/ET x44 #))};      \
    {-# INLINE acos #-};      \
    acos (MC a1 a2 a3 a4) = case (# unpack/**/VT a1      \
                                 , unpack/**/VT a2      \
                                 , unpack/**/VT a3      \
                                 , unpack/**/VT a4 #) of      \
        {(# (# x11, x21, x31, x41 #)      \
         ,(# x12, x22, x32, x42 #)      \
         ,(# x13, x23, x33, x43 #)      \
         ,(# x14, x24, x34, x44 #)      \
         #) -> MC (pack/**/VT (# acos/**/ET x11, acos/**/ET x21, acos/**/ET x31, acos/**/ET x41 #))      \
                  (pack/**/VT (# acos/**/ET x12, acos/**/ET x22, acos/**/ET x32, acos/**/ET x42 #))      \
                  (pack/**/VT (# acos/**/ET x13, acos/**/ET x23, acos/**/ET x33, acos/**/ET x43 #))      \
                  (pack/**/VT (# acos/**/ET x14, acos/**/ET x24, acos/**/ET x34, acos/**/ET x44 #))};      \
    {-# INLINE atan #-};      \
    atan (MC a1 a2 a3 a4) = case (# unpack/**/VT a1      \
                                 , unpack/**/VT a2      \
                                 , unpack/**/VT a3      \
                                 , unpack/**/VT a4 #) of      \
        {(# (# x11, x21, x31, x41 #)      \
         ,(# x12, x22, x32, x42 #)      \
         ,(# x13, x23, x33, x43 #)      \
         ,(# x14, x24, x34, x44 #)      \
         #) -> MC (pack/**/VT (# atan/**/ET x11, atan/**/ET x21, atan/**/ET x31, atan/**/ET x41 #))      \
                  (pack/**/VT (# atan/**/ET x12, atan/**/ET x22, atan/**/ET x32, atan/**/ET x42 #))      \
                  (pack/**/VT (# atan/**/ET x13, atan/**/ET x23, atan/**/ET x33, atan/**/ET x43 #))      \
                  (pack/**/VT (# atan/**/ET x14, atan/**/ET x24, atan/**/ET x34, atan/**/ET x44 #))};      \
    {-# INLINE sinh #-};      \
    sinh (MC a1 a2 a3 a4) = case (# unpack/**/VT a1      \
                                 , unpack/**/VT a2      \
                                 , unpack/**/VT a3      \
                                 , unpack/**/VT a4 #) of      \
        {(# (# x11, x21, x31, x41 #)      \
         ,(# x12, x22, x32, x42 #)      \
         ,(# x13, x23, x33, x43 #)      \
         ,(# x14, x24, x34, x44 #)      \
         #) -> MC (pack/**/VT (# sinh/**/ET x11, sinh/**/ET x21, sinh/**/ET x31, sinh/**/ET x41 #))      \
                  (pack/**/VT (# sinh/**/ET x12, sinh/**/ET x22, sinh/**/ET x32, sinh/**/ET x42 #))      \
                  (pack/**/VT (# sinh/**/ET x13, sinh/**/ET x23, sinh/**/ET x33, sinh/**/ET x43 #))      \
                  (pack/**/VT (# sinh/**/ET x14, sinh/**/ET x24, sinh/**/ET x34, sinh/**/ET x44 #))};      \
    {-# INLINE cosh #-};      \
    cosh (MC a1 a2 a3 a4) = case (# unpack/**/VT a1      \
                                 , unpack/**/VT a2      \
                                 , unpack/**/VT a3      \
                                 , unpack/**/VT a4 #) of      \
        {(# (# x11, x21, x31, x41 #)      \
         ,(# x12, x22, x32, x42 #)      \
         ,(# x13, x23, x33, x43 #)      \
         ,(# x14, x24, x34, x44 #)      \
         #) -> MC (pack/**/VT (# cosh/**/ET x11, cosh/**/ET x21, cosh/**/ET x31, cosh/**/ET x41 #))      \
                  (pack/**/VT (# cosh/**/ET x12, cosh/**/ET x22, cosh/**/ET x32, cosh/**/ET x42 #))      \
                  (pack/**/VT (# cosh/**/ET x13, cosh/**/ET x23, cosh/**/ET x33, cosh/**/ET x43 #))      \
                  (pack/**/VT (# cosh/**/ET x14, cosh/**/ET x24, cosh/**/ET x34, cosh/**/ET x44 #))};      \
    {-# INLINE tanh #-};      \
    tanh (MC a1 a2 a3 a4) = case (# unpack/**/VT a1      \
                                 , unpack/**/VT a2      \
                                 , unpack/**/VT a3      \
                                 , unpack/**/VT a4 #) of      \
        {(# (# x11, x21, x31, x41 #)      \
         ,(# x12, x22, x32, x42 #)      \
         ,(# x13, x23, x33, x43 #)      \
         ,(# x14, x24, x34, x44 #)      \
         #) -> MC (pack/**/VT (# tanh/**/ET x11, tanh/**/ET x21, tanh/**/ET x31, tanh/**/ET x41 #))      \
                  (pack/**/VT (# tanh/**/ET x12, tanh/**/ET x22, tanh/**/ET x32, tanh/**/ET x42 #))      \
                  (pack/**/VT (# tanh/**/ET x13, tanh/**/ET x23, tanh/**/ET x33, tanh/**/ET x43 #))      \
                  (pack/**/VT (# tanh/**/ET x14, tanh/**/ET x24, tanh/**/ET x34, tanh/**/ET x44 #))};      \
    {-# INLINE asinh #-};      \
    asinh (MC a1 a2 a3 a4) = case broadcast/**/VT 1/**/e of      \
      {b1 -> let {f a = unpack/**/VT (      \
                      plus/**/VT a (      \
                        sqrt/**/VT (      \
                            plus/**/VT (times/**/VT a a) b1      \
                        )      \
                      )      \
                    ) } in case (# f a1, f a2, f a3, f a4 #) of      \
        {(# (# x11, x21, x31, x41 #)      \
         ,(# x12, x22, x32, x42 #)      \
         ,(# x13, x23, x33, x43 #)      \
         ,(# x14, x24, x34, x44 #)      \
         #) -> MC (pack/**/VT (# log/**/ET x11, log/**/ET x21, log/**/ET x31, log/**/ET x41 #))      \
                   (pack/**/VT (# log/**/ET x12, log/**/ET x22, log/**/ET x32, log/**/ET x42 #))      \
                   (pack/**/VT (# log/**/ET x13, log/**/ET x23, log/**/ET x33, log/**/ET x43 #))      \
                   (pack/**/VT (# log/**/ET x14, log/**/ET x24, log/**/ET x34, log/**/ET x44 #))}};      \
    {-# INLINE acosh #-};      \
    acosh (MC a1 a2 a3 a4) = case broadcast/**/VT 1/**/e of      \
      {b1 -> let {f a = unpack/**/VT (      \
                      plus/**/VT a (      \
                        sqrt/**/VT (      \
                            minus/**/VT (times/**/VT a a) b1      \
                        )      \
                      )      \
                    ) } in case (# f a1, f a2, f a3, f a4 #) of      \
        {(# (# x11, x21, x31, x41 #)      \
         ,(# x12, x22, x32, x42 #)      \
         ,(# x13, x23, x33, x43 #)      \
         ,(# x14, x24, x34, x44 #)      \
         #) -> MC (pack/**/VT (# log/**/ET x11, log/**/ET x21, log/**/ET x31, log/**/ET x41 #))      \
                   (pack/**/VT (# log/**/ET x12, log/**/ET x22, log/**/ET x32, log/**/ET x42 #))      \
                   (pack/**/VT (# log/**/ET x13, log/**/ET x23, log/**/ET x33, log/**/ET x43 #))      \
                   (pack/**/VT (# log/**/ET x14, log/**/ET x24, log/**/ET x34, log/**/ET x44 #))}};      \
    {-# INLINE atanh #-};      \
    atanh (MC a1 a2 a3 a4) = case (# broadcast/**/VT 1/**/e, broadcast/**/VT 0.5# #) of      \
      {(# b1,b5 #) -> let {f a = unpack/**/VT (      \
                                 divide/**/VT (plus/**/VT b1 a)      \
                                       (minus/**/VT b1 a)      \
                               ) ;     \
                          g = times/**/VT b5 } in case (# f a1, f a2, f a3, f a4 #) of      \
        {(# (# x11, x21, x31, x41 #)      \
         ,(# x12, x22, x32, x42 #)      \
         ,(# x13, x23, x33, x43 #)      \
         ,(# x14, x24, x34, x44 #)      \
         #) -> MC (g (pack/**/VT (# log/**/ET x11, log/**/ET x21, log/**/ET x31, log/**/ET x41 #)))      \
                   (g (pack/**/VT (# log/**/ET x12, log/**/ET x22, log/**/ET x32, log/**/ET x42 #)))      \
                   (g (pack/**/VT (# log/**/ET x13, log/**/ET x23, log/**/ET x33, log/**/ET x43 #)))      \
                   (g (pack/**/VT (# log/**/ET x14, log/**/ET x24, log/**/ET x34, log/**/ET x44 #)))} } }


FLOATING4V(Float,Float#,FloatX4#,V4F,F#, emptyc,.0#)
FLOATING4V(CFloat,Float#,FloatX4#,V4CF,F#,CFloat,.0#)


FLOATING4M(Float,Float#,FloatX4#,M4F,F#, emptyc,.0#)
FLOATING4M(CFloat,Float#,FloatX4#,M4CF,F#,CFloat,.0#)
