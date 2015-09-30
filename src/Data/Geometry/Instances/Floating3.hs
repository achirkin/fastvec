{-# LANGUAGE MagicHash, UnboxedTuples #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeFamilies, DataKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Geometry.Instances.Floating3
-- Copyright   :  Copyright (C) 2015 Artem M. Chirkin <chirkin@arch.ethz.ch>
-- License     :  BSD3
--
-- Maintainer  :  Artem M. Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  Experimental
-- Portability :
--
--
-----------------------------------------------------------------------------

module Data.Geometry.Instances.Floating3 () where


import GHC.Exts

import Foreign.C.Types

import Data.Geometry.VectorMath
import Data.Geometry.Prim.FloatX3
import Data.Geometry.Types
import Data.Geometry.Instances.Num ()
import Data.Geometry.Instances.Fractional ()

#define emptyc(x) x

-- params: type, vectortype, elPrimType, Vector constr, Elem constr, Elem newtype, num ending
#define FLOATING3V(T,ET,VT,VC,EC,EC2,e)                                   \
instance Floating (Vector 3 T) where {     \
    {-# INLINE sqrt #-};      \
    sqrt (VC a) = VC (sqrt/**/VT a);      \
    {-# INLINE pi #-};      \
    pi = case pi of      \
        {EC2(EC f) -> VC (broadcast/**/VT f)};      \
    {-# INLINE exp #-};      \
    exp (VC a) = case unpack/**/VT a of      \
        {(# x, y, z, _ #) -> VC (pack/**/VT (# exp/**/ET x, exp/**/ET y, exp/**/ET z, 0/**/e #))};      \
    {-# INLINE log #-};      \
    log (VC a) = case unpack/**/VT a of      \
        {(# x, y, z, _ #) -> VC (pack/**/VT (# log/**/ET x, log/**/ET y, log/**/ET z, 0/**/e #))};      \
    {-# INLINE sin #-};      \
    sin (VC a) = case unpack/**/VT a of      \
        {(# x, y, z, _ #) -> VC (pack/**/VT (# sin/**/ET x, sin/**/ET y, sin/**/ET z, 0/**/e #))};      \
    {-# INLINE cos #-};      \
    cos (VC a) = case unpack/**/VT a of      \
        {(# x, y, z, _ #) -> VC (pack/**/VT (# cos/**/ET x, cos/**/ET y, cos/**/ET z, 0/**/e #))};      \
    {-# INLINE asin #-};      \
    asin (VC a) = case unpack/**/VT a of      \
        {(# x, y, z, _ #) -> VC (pack/**/VT (# asin/**/ET x, asin/**/ET y, asin/**/ET z, 0/**/e #))};      \
    {-# INLINE acos #-};      \
    acos (VC a) = case unpack/**/VT a of      \
        {(# x, y, z, _ #) -> VC (pack/**/VT (# acos/**/ET x, acos/**/ET y, acos/**/ET z, 0/**/e #))};      \
    {-# INLINE atan #-};      \
    atan (VC a) = case unpack/**/VT a of      \
        {(# x, y, z, _ #) -> VC (pack/**/VT (# atan/**/ET x, atan/**/ET y, atan/**/ET z, 0/**/e #))};      \
    {-# INLINE sinh #-};      \
    sinh (VC a) = case unpack/**/VT a of      \
        {(# x, y, z, _ #) -> VC (pack/**/VT (# sinh/**/ET x, sin/**/ET y, sin/**/ET z, 0/**/e #))};      \
    {-# INLINE cosh #-};      \
    cosh (VC a) = case unpack/**/VT a of      \
        {(# x, y, z, _ #) -> VC (pack/**/VT (# cosh/**/ET x, cos/**/ET y, cos/**/ET z, 0/**/e #))};      \
    {-# INLINE tanh #-};      \
    tanh (VC a) = case unpack/**/VT a of      \
        {(# x, y, z, _ #) -> VC (pack/**/VT (# tanh/**/ET x, tanh/**/ET y, tanh/**/ET z, 0/**/e #))};      \
    {-# INLINE asinh #-};      \
    asinh (VC a) = case unpack/**/VT (      \
                      plus/**/VT a (      \
                        sqrt/**/VT (      \
                            plus/**/VT (times/**/VT a a)      \
                                         (broadcast/**/VT 1/**/e)      \
                        )      \
                      )      \
                    ) of      \
        {(# x, y, z, _ #) -> VC (pack/**/VT (# log/**/ET x, log/**/ET y, log/**/ET z, 0/**/e #))};      \
    {-# INLINE acosh #-};      \
    acosh (VC a) = case unpack/**/VT (      \
                      plus/**/VT a (      \
                        sqrt/**/VT (      \
                            minus/**/VT (times/**/VT a a)      \
                                          (broadcast/**/VT 1/**/e)      \
                        )      \
                      )      \
                    ) of      \
        {(# x, y, z, _ #) -> VC (pack/**/VT (# log/**/ET x, log/**/ET y, log/**/ET z, 0/**/e #)) };     \
    {-# INLINE atanh #-};      \
    atanh (VC a) | one <- broadcast/**/VT 1/**/e = case unpack/**/VT (      \
                        divide/**/VT (plus/**/VT one a)      \
                                       (minus/**/VT one a)      \
                    ) of      \
        {(# x, y, z, _ #) -> VC (times/**/VT (broadcast/**/VT 0.5#)      \
                    (pack/**/VT (# log/**/ET x, log/**/ET y, log/**/ET z, 0/**/e #))      \
                    )}     }


-- params: type, vectortype, elPrimType, Matrix constr, Elem constr, Elem newtype, num ending
#define FLOATING3M(T,ET,VT,MC,EC,EC2,e) \
instance Floating (Matrix 3 T) where {     \
    {-# INLINE sqrt #-};      \
    sqrt (MC a1 a2 a3) = MC (sqrt/**/VT a1)      \
                            (sqrt/**/VT a2)      \
                            (sqrt/**/VT a3);      \
    {-# INLINE pi #-};      \
    pi = case pi of      \
        {EC2(EC f) -> case broadcast/**/VT f of b -> MC b b b};     \
    {-# INLINE exp #-};      \
    exp (MC a1 a2 a3) = case (# unpack/**/VT a1      \
                              , unpack/**/VT a2      \
                              , unpack/**/VT a3 #) of      \
        {(# (# x11, x21, x31, _ #)      \
         ,(# x12, x22, x32, _ #)      \
         ,(# x13, x23, x33, _ #)     \
         #) -> MC (pack/**/VT (# exp/**/ET x11, exp/**/ET x21, exp/**/ET x31, 0/**/e #))      \
                  (pack/**/VT (# exp/**/ET x12, exp/**/ET x22, exp/**/ET x32, 0/**/e #))      \
                  (pack/**/VT (# exp/**/ET x13, exp/**/ET x23, exp/**/ET x33, 0/**/e #))};      \
    {-# INLINE log #-};      \
    log (MC a1 a2 a3) = case (# unpack/**/VT a1      \
                              , unpack/**/VT a2      \
                              , unpack/**/VT a3 #) of      \
        {(# (# x11, x21, x31, _ #)      \
          , (# x12, x22, x32, _ #)      \
          , (# x13, x23, x33, _ #)     \
         #) -> MC (pack/**/VT (# log/**/ET x11, log/**/ET x21, log/**/ET x31, 0/**/e #))      \
                  (pack/**/VT (# log/**/ET x12, log/**/ET x22, log/**/ET x32, 0/**/e #))      \
                  (pack/**/VT (# log/**/ET x13, log/**/ET x23, log/**/ET x33, 0/**/e #))};     \
    {-# INLINE sin #-};      \
    sin (MC a1 a2 a3) = case (# unpack/**/VT a1      \
                              , unpack/**/VT a2      \
                              , unpack/**/VT a3 #) of      \
        {(# (# x11, x21, x31, _ #)      \
          , (# x12, x22, x32, _ #)      \
          , (# x13, x23, x33, _ #)      \
         #) -> MC (pack/**/VT (# sin/**/ET x11, sin/**/ET x21, sin/**/ET x31, 0/**/e #))      \
                  (pack/**/VT (# sin/**/ET x12, sin/**/ET x22, sin/**/ET x32, 0/**/e #))      \
                  (pack/**/VT (# sin/**/ET x13, sin/**/ET x23, sin/**/ET x33, 0/**/e #))};      \
    {-# INLINE cos #-};      \
    cos (MC a1 a2 a3) = case (# unpack/**/VT a1      \
                              , unpack/**/VT a2      \
                              , unpack/**/VT a3 #) of      \
        {(# (# x11, x21, x31, _ #)      \
          , (# x12, x22, x32, _ #)      \
          , (# x13, x23, x33, _ #)      \
         #) -> MC (pack/**/VT (# cos/**/ET x11, cos/**/ET x21, cos/**/ET x31, 0/**/e #))      \
                  (pack/**/VT (# cos/**/ET x12, cos/**/ET x22, cos/**/ET x32, 0/**/e #))      \
                  (pack/**/VT (# cos/**/ET x13, cos/**/ET x23, cos/**/ET x33, 0/**/e #)) };      \
    {-# INLINE asin #-};      \
    asin (MC a1 a2 a3) = case (# unpack/**/VT a1      \
                               , unpack/**/VT a2      \
                               , unpack/**/VT a3 #) of      \
        {(# (# x11, x21, x31, _ #)      \
          , (# x12, x22, x32, _ #)      \
          , (# x13, x23, x33, _ #)      \
         #) -> MC (pack/**/VT (# asin/**/ET x11, asin/**/ET x21, asin/**/ET x31, 0/**/e #))      \
                  (pack/**/VT (# asin/**/ET x12, asin/**/ET x22, asin/**/ET x32, 0/**/e #))      \
                  (pack/**/VT (# asin/**/ET x13, asin/**/ET x23, asin/**/ET x33, 0/**/e #))};      \
    {-# INLINE acos #-};      \
    acos (MC a1 a2 a3) = case (# unpack/**/VT a1      \
                               , unpack/**/VT a2      \
                               , unpack/**/VT a3 #) of      \
        {(# (# x11, x21, x31, _ #)      \
          , (# x12, x22, x32, _ #)      \
          , (# x13, x23, x33, _ #)      \
         #) -> MC (pack/**/VT (# acos/**/ET x11, acos/**/ET x21, acos/**/ET x31, 0/**/e #))      \
                  (pack/**/VT (# acos/**/ET x12, acos/**/ET x22, acos/**/ET x32, 0/**/e #))      \
                  (pack/**/VT (# acos/**/ET x13, acos/**/ET x23, acos/**/ET x33, 0/**/e #))};      \
    {-# INLINE atan #-};      \
    atan (MC a1 a2 a3) = case (# unpack/**/VT a1      \
                               , unpack/**/VT a2      \
                               , unpack/**/VT a3 #) of      \
        {(# (# x11, x21, x31, _ #)      \
          , (# x12, x22, x32, _ #)      \
          , (# x13, x23, x33, _ #)       \
         #) -> MC (pack/**/VT (# atan/**/ET x11, atan/**/ET x21, atan/**/ET x31, 0/**/e #))      \
                  (pack/**/VT (# atan/**/ET x12, atan/**/ET x22, atan/**/ET x32, 0/**/e #))      \
                  (pack/**/VT (# atan/**/ET x13, atan/**/ET x23, atan/**/ET x33, 0/**/e #))};      \
    {-# INLINE sinh #-};      \
    sinh (MC a1 a2 a3) = case (# unpack/**/VT a1      \
                               , unpack/**/VT a2      \
                               , unpack/**/VT a3 #) of      \
        {(# (# x11, x21, x31, _ #)      \
          , (# x12, x22, x32, _ #)      \
          , (# x13, x23, x33, _ #)      \
         #) -> MC (pack/**/VT (# sinh/**/ET x11, sinh/**/ET x21, sinh/**/ET x31, 0/**/e #))      \
                  (pack/**/VT (# sinh/**/ET x12, sinh/**/ET x22, sinh/**/ET x32, 0/**/e #))      \
                  (pack/**/VT (# sinh/**/ET x13, sinh/**/ET x23, sinh/**/ET x33, 0/**/e #)) };      \
    {-# INLINE cosh #-};      \
    cosh (MC a1 a2 a3) = case (# unpack/**/VT a1      \
                               , unpack/**/VT a2      \
                               , unpack/**/VT a3 #) of      \
        {(# (# x11, x21, x31, _ #)      \
          , (# x12, x22, x32, _ #)      \
          , (# x13, x23, x33, _ #)      \
         #) -> MC (pack/**/VT (# cosh/**/ET x11, cosh/**/ET x21, cosh/**/ET x31, 0/**/e #))      \
                  (pack/**/VT (# cosh/**/ET x12, cosh/**/ET x22, cosh/**/ET x32, 0/**/e #))      \
                  (pack/**/VT (# cosh/**/ET x13, cosh/**/ET x23, cosh/**/ET x33, 0/**/e #))};      \
    {-# INLINE tanh #-};      \
    tanh (MC a1 a2 a3) = case (# unpack/**/VT a1      \
                               , unpack/**/VT a2      \
                               , unpack/**/VT a3 #) of      \
        {(# (# x11, x21, x31, _ #)      \
          , (# x12, x22, x32, _ #)      \
          , (# x13, x23, x33, _ #)       \
         #) -> MC (pack/**/VT (# tanh/**/ET x11, tanh/**/ET x21, tanh/**/ET x31, 0/**/e #))      \
                  (pack/**/VT (# tanh/**/ET x12, tanh/**/ET x22, tanh/**/ET x32, 0/**/e #))      \
                  (pack/**/VT (# tanh/**/ET x13, tanh/**/ET x23, tanh/**/ET x33, 0/**/e #))};      \
    {-# INLINE asinh #-};      \
    asinh (MC a1 a2 a3) = case broadcast/**/VT 1/**/e of      \
      {b1 -> let {f a = unpack/**/VT (      \
                      plus/**/VT a (      \
                        sqrt/**/VT (      \
                            plus/**/VT (times/**/VT a a) b1      \
                        )      \
                      )      \
                    ) } in case (# f a1, f a2, f a3 #) of      \
        {(# (# x11, x21, x31, _ #)      \
          , (# x12, x22, x32, _ #)      \
          , (# x13, x23, x33, _ #)      \
          #) -> MC (pack/**/VT (# log/**/ET x11, log/**/ET x21, log/**/ET x31, 0/**/e #))      \
                   (pack/**/VT (# log/**/ET x12, log/**/ET x22, log/**/ET x32, 0/**/e #))      \
                   (pack/**/VT (# log/**/ET x13, log/**/ET x23, log/**/ET x33, 0/**/e #))}};      \
    {-# INLINE acosh #-};      \
    acosh (MC a1 a2 a3) = case broadcast/**/VT 1/**/e of      \
      {b1 -> let {f a = unpack/**/VT (      \
                      plus/**/VT a (      \
                        sqrt/**/VT (      \
                            minus/**/VT (times/**/VT a a) b1      \
                        )      \
                      )      \
                    ) } in case (# f a1, f a2, f a3 #) of      \
        {(# (# x11, x21, x31, _ #)      \
          , (# x12, x22, x32, _ #)      \
          , (# x13, x23, x33, _ #)      \
          #) -> MC (pack/**/VT (# log/**/ET x11, log/**/ET x21, log/**/ET x31, 0/**/e #))      \
                   (pack/**/VT (# log/**/ET x12, log/**/ET x22, log/**/ET x32, 0/**/e #))      \
                   (pack/**/VT (# log/**/ET x13, log/**/ET x23, log/**/ET x33, 0/**/e #))}};      \
    {-# INLINE atanh #-};      \
    atanh (MC a1 a2 a3) = case (# broadcast/**/VT 1/**/e, broadcast/**/VT 0.5# #) of      \
      {(# b1,b5 #) -> let {f a = unpack/**/VT (      \
                                 divide/**/VT (plus/**/VT b1 a)      \
                                       (minus/**/VT b1 a)      \
                               ) ;     \
                          g = times/**/VT b5 } in case (# f a1, f a2, f a3 #) of      \
        {(# (# x11, x21, x31, _ #)      \
          , (# x12, x22, x32, _ #)      \
          , (# x13, x23, x33, _ #)      \
          #) -> MC (g (pack/**/VT (# log/**/ET x11, log/**/ET x21, log/**/ET x31, 0/**/e #)))      \
                   (g (pack/**/VT (# log/**/ET x12, log/**/ET x22, log/**/ET x32, 0/**/e #)))      \
                   (g (pack/**/VT (# log/**/ET x13, log/**/ET x23, log/**/ET x33, 0/**/e #)))} } }


FLOATING3V(Float,Float#,FloatX3#,V3F,F#, emptyc,.0#)
FLOATING3V(CFloat,Float#,FloatX3#,V3CF,F#,CFloat,.0#)


FLOATING3M(Float,Float#,FloatX3#,M3F,F#, emptyc,.0#)
FLOATING3M(CFloat,Float#,FloatX3#,M3CF,F#,CFloat,.0#)
