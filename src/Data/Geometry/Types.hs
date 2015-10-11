{-# LANGUAGE MagicHash, UnboxedTuples #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeFamilies, DataKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
#if defined(ghcjs_HOST_OS)
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
#else
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Geometry.Types
-- Copyright   :  Copyright (C) 2015 Artem M. Chirkin <chirkin@arch.ethz.ch>
-- License     :  BSD3
--
-- Maintainer  :  Artem M. Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  Experimental
--
--
-----------------------------------------------------------------------------

module Data.Geometry.Types where

import Data.Geometry.VectorMath


#if defined(ghcjs_HOST_OS)

import GHCJS.Types
--import GHCJS.Internal.Types
import Data.Geometry.Prim.JSNum
--import Unsafe.Coerce
import GHC.TypeLits (KnownNat)

instance (KnownNat n, JSNum a) => VectorMath n a where
    {-# SPECIALIZE instance VectorMath 4 Int #-}
    {-# SPECIALIZE instance VectorMath 4 Float #-}
    {-# SPECIALIZE instance VectorMath 4 Double #-}
    {-# INLINE eye #-}
    eye = toMatrix . eyeJSMat $ dim (undefined :: Matrix n a)
    {-# INLINE diag #-}
    diag x = toMatrix . diagJSMat (fromNum x) $ dim (undefined :: Matrix n a)
    {-# INLINE transpose #-}
    transpose m = toMatrix . transposeJSMat (jsref m) . dim $ m
    {-# INLINE det #-}
    det m = toNum . detJSMat (jsref m) . dim $ m
    {-# INLINE trace #-}
    trace m = toNum . traceJSMat (jsref m) . dim $ m
    {-# INLINE fromDiag #-}
    fromDiag m = toVector . fromDiagJSMat (jsref m) . dim $ m
    {-# INLINE toDiag #-}
    toDiag = toMatrix . toDiagJSMat . jsref
    {-# INLINE (.*.) #-}
    a .*. b = toVector $ dotBJSVec (jsref a) (jsref b)
    {-# INLINE dot #-}
    dot a b = toNum $ dotJSVec (jsref a) (jsref b)


instance JSNum a => Vector4Math a where
    {-# SPECIALIZE instance Vector4Math Int #-}
    {-# SPECIALIZE instance Vector4Math Float #-}
    {-# SPECIALIZE instance Vector4Math Double #-}
    {-# INLINE vector4 #-}
    vector4 a b c d = toVector $ jsVector4 (fromNum a) (fromNum b) (fromNum c) (fromNum d)
    {-# INLINE matrix4x4 #-}
    matrix4x4 a b c d = toMatrix $ jsMatrix4 (jsref a) (jsref b) (jsref c) (jsref d)

instance JSNum a => Vector3Math a where
    {-# SPECIALIZE instance Vector3Math Int #-}
    {-# SPECIALIZE instance Vector3Math Float #-}
    {-# SPECIALIZE instance Vector3Math Double #-}
    {-# INLINE vector3 #-}
    vector3 a b c = toVector $ jsVector3 (fromNum a) (fromNum b) (fromNum c)
    {-# INLINE matrix3x3 #-}
    matrix3x3 a b c = toMatrix $ jsMatrix3 (jsref a) (jsref b) (jsref c)

instance JSNum a => Vector2Math a where
    {-# SPECIALIZE instance Vector2Math Int #-}
    {-# SPECIALIZE instance Vector2Math Float #-}
    {-# SPECIALIZE instance Vector2Math Double #-}
    {-# INLINE vector2 #-}
    vector2 a b = toVector $ jsVector2 (fromNum a) (fromNum b)
    {-# INLINE matrix2x2 #-}
    matrix2x2 a b = toMatrix $ jsMatrix2 (jsref a) (jsref b)

instance (JSNum a, KnownNat n) => MatrixProduct Matrix n a where
    prod a b = toMatrix $ prodJSMM (jsref a) (jsref b) (dim b)

instance (JSNum a, KnownNat n) => MatrixProduct Vector n a where
    prod a b = toVector $ prodJSMV (jsref a) (jsref b)

instance JSNum a => VectorFracMath 4 a where
    {-# SPECIALIZE instance VectorFracMath 4 Float #-}
    {-# SPECIALIZE instance VectorFracMath 4 Double #-}
    {-# INLINE inverse #-}
    inverse = toMatrix . inverseJSM4 . jsref

instance JSNum a => VectorFracMath 3 a where
    {-# SPECIALIZE instance VectorFracMath 3 Float #-}
    {-# SPECIALIZE instance VectorFracMath 3 Double #-}
    {-# INLINE inverse #-}
    inverse = toMatrix . inverseJSM3 . jsref

instance JSNum a => VectorFracMath 2 a where
    {-# SPECIALIZE instance VectorFracMath 2 Float #-}
    {-# SPECIALIZE instance VectorFracMath 2 Double #-}
    {-# INLINE inverse #-}
    inverse = toMatrix . inverseJSM2 . jsref

#else

import GHC.Exts
import GHC.Int

import Foreign.C.Types

import Data.Geometry.Prim.Int32X4
import Data.Geometry.Prim.FloatX3
import Data.Geometry.Prim.FloatX4

#define emptyc(x) x

-- params: type, vectortype, Vector constr, Matrix constr
--       , Elem constr, num ending, plusOp, Elem newtype
#define VECTORMATH4(T,VT,VC,MC,EC,e,pOp, EC2)                                            \
instance VectorMath 4 T where {                                                          \
    data Vector 4 T = VC VT;                                                             \
    data Matrix 4 T = MC VT VT VT VT;                                                    \
    {-# INLINE eye #-};                                                                  \
    eye = MC (pack/**/VT (# 1/**/e, 0/**/e, 0/**/e, 0/**/e #))                           \
             (pack/**/VT (# 0/**/e, 1/**/e, 0/**/e, 0/**/e #))                           \
             (pack/**/VT (# 0/**/e, 0/**/e, 1/**/e, 0/**/e #))                           \
             (pack/**/VT (# 0/**/e, 0/**/e, 0/**/e, 1/**/e #));                          \
    {-# INLINE diag #-};                                                                 \
    diag (EC2(EC x)) = MC (pack/**/VT (#    x  , 0/**/e, 0/**/e, 0/**/e #))              \
                          (pack/**/VT (# 0/**/e,    x  , 0/**/e, 0/**/e #))              \
                          (pack/**/VT (# 0/**/e, 0/**/e,    x  , 0/**/e #))              \
                          (pack/**/VT (# 0/**/e, 0/**/e, 0/**/e,    x   #));             \
    {-# INLINE transpose #-};                                                            \
    transpose (MC c1 c2 c3 c4) = case transposeM/**/VT c1 c2 c3 c4 of                    \
       {(# r1, r2, r3, r4 #) -> MC r1 r2 r3 r4};                                         \
    {-# INLINE det #-};                                                                  \
    det (MC c1 c2 c3 c4) = case unpack/**/VT (detM/**/VT c1 c2 c3 c4) of                 \
       {(# r1, _, _, _ #) -> EC2(EC r1)};                                                \
    {-# INLINE trace #-};                                                                \
    trace (MC a1 a2 a3 a4) = case (# unpack/**/VT a1                                     \
                                   , unpack/**/VT a2                                     \
                                   , unpack/**/VT a3                                     \
                                   , unpack/**/VT a4 #) of                               \
       {(#(# x11,  _ ,  _ ,  _  #)                                                       \
         ,(#  _ , x22,  _ ,  _  #)                                                       \
         ,(#  _ ,  _ , x33,  _  #)                                                       \
         ,(#  _ ,  _ ,  _ , x44 #)                                                       \
         #) -> EC2(EC (x11 pOp x22 pOp x33 pOp x44))};                                   \
    {-# INLINE fromDiag #-};                                                             \
    fromDiag (MC a1 a2 a3 a4) = case (# unpack/**/VT a1                                  \
                                      , unpack/**/VT a2                                  \
                                      , unpack/**/VT a3                                  \
                                      , unpack/**/VT a4 #) of                            \
       {(#(# x11,  _ ,  _ ,  _  #)                                                       \
         ,(#  _ , x22,  _ ,  _  #)                                                       \
         ,(#  _ ,  _ , x33,  _  #)                                                       \
         ,(#  _ ,  _ ,  _ , x44 #)                                                       \
         #) -> VC (pack/**/VT (# x11, x22, x33, x44 #))};                                \
    {-# INLINE toDiag #-};                                                               \
    toDiag (VC a) = case unpack/**/VT a of                                               \
       {(# x11, x22, x33, x44 #) -> MC (pack/**/VT (# x11 , 0/**/e, 0/**/e, 0/**/e #))   \
                                       (pack/**/VT (# 0/**/e, x22 , 0/**/e, 0/**/e #))   \
                                       (pack/**/VT (# 0/**/e, 0/**/e, x33 , 0/**/e #))   \
                                       (pack/**/VT (# 0/**/e, 0/**/e, 0/**/e, x44  #))}; \
    {-# INLINE (.*.) #-};                                                                \
    VC a .*. VC b = VC (dot/**/VT a b);                                                  \
    {-# INLINE dot #-} ;                                                                 \
    dot (VC a) (VC b) = case unpack/**/VT (dot/**/VT a b) of                             \
       {(# r1, _, _, _ #) -> EC2(EC r1)}                                                 }


VECTORMATH4(Int32,Int32X4#,V4I32,M4I32,I32#,#,+#, emptyc)
VECTORMATH4(Int,Int32X4#,V4I,M4I,I#,#,+#, emptyc)
VECTORMATH4(CInt,Int32X4#,V4CI,M4CI,I32#,#,+#,CInt)

VECTORMATH4(Float,FloatX4#,V4F,M4F,F#,.0#,`plusFloat#`,emptyc)
VECTORMATH4(CFloat,FloatX4#,V4CF,M4CF,F#,.0#,`plusFloat#`,CFloat)

-- params: type, vectortype, Vector constr, Matrix constr, Elem constr, Elem newtype
#define VECTOR4MATH(T,VT,VC,MC,EC,EC2)                          \
instance Vector4Math T where {                                  \
    {-# INLINE vector4 #-};                                     \
    vector4 (EC2(EC x)) (EC2(EC y)) (EC2(EC z)) (EC2(EC t))     \
        = VC (pack/**/VT (# x, y, z, t #));                     \
    {-# INLINE matrix4x4 #-};                                   \
    matrix4x4 (VC c1) (VC c2) (VC c3) (VC c4) = MC c1 c2 c3 c4  }


VECTOR4MATH(Int32,Int32X4#,V4I32,M4I32,I32#,emptyc)
VECTOR4MATH(Int,Int32X4#,V4I,M4I,I#,emptyc)
VECTOR4MATH(CInt,Int32X4#,V4CI,M4CI,I32#,CInt)

VECTOR4MATH(Float,FloatX4#,V4F,M4F,F#,emptyc)
VECTOR4MATH(CFloat,FloatX4#,V4CF,M4CF,F#,CFloat)

#define MATRIXPRODUCT4(T,VT,VC,MC)                                 \
instance MatrixProduct Matrix 4 T where {                          \
    {-# INLINE prod #-};                                           \
    prod (MC a1 a2 a3 a4) (MC b1 b2 b3 b4)                         \
        = case prodMM/**/VT a1 a2 a3 a4 b1 b2 b3 b4 of             \
          {(# r1, r2, r3, r4 #) -> MC r1 r2 r3 r4}};               \
instance MatrixProduct Vector 4 T where {                          \
    {-# INLINE prod #-};                                           \
    prod (MC a1 a2 a3 a4) (VC b) = VC (prodMV/**/VT a1 a2 a3 a4 b) }

MATRIXPRODUCT4(Int32,Int32X4#,V4I32,M4I32)
MATRIXPRODUCT4(Int,Int32X4#,V4I,M4I)
MATRIXPRODUCT4(CInt,Int32X4#,V4CI,M4CI)

MATRIXPRODUCT4(Float,FloatX4#,V4F,M4F)
MATRIXPRODUCT4(CFloat,FloatX4#,V4CF,M4CF)

#define VECTORFRACMATH4(T,VT,VC,MC)                               \
instance VectorFracMath 4 T where {                               \
    {-# INLINE inverse #-};                                       \
    inverse (MC c1 c2 c3 c4) = case inverseM/**/VT c1 c2 c3 c4 of \
        (# r1, r2, r3, r4 #) -> MC r1 r2 r3 r4                    }

VECTORFRACMATH4(Float,FloatX4#,V4F,M4F)
VECTORFRACMATH4(CFloat,FloatX4#,V4CF,M4CF)


-- params: type, vectortype, Vector constr, Matrix constr
--       , Elem constr, num ending, plusOp, Elem newtype
#define VECTORMATH3(T,VT,VC,MC,EC,e,pOp, EC2)                                            \
instance VectorMath 3 T where {                                                          \
    data Vector 3 T = VC VT;                                                             \
    data Matrix 3 T = MC VT VT VT;                                                    \
    {-# INLINE eye #-};                                                                  \
    eye = MC (pack/**/VT (# 1/**/e, 0/**/e, 0/**/e, 0/**/e #))                           \
             (pack/**/VT (# 0/**/e, 1/**/e, 0/**/e, 0/**/e #))                           \
             (pack/**/VT (# 0/**/e, 0/**/e, 1/**/e, 0/**/e #));                          \
    {-# INLINE diag #-};                                                                 \
    diag (EC2(EC x)) = MC (pack/**/VT (#    x  , 0/**/e, 0/**/e, 0/**/e #))              \
                          (pack/**/VT (# 0/**/e,    x  , 0/**/e, 0/**/e #))              \
                          (pack/**/VT (# 0/**/e, 0/**/e,    x  , 0/**/e #));             \
    {-# INLINE transpose #-};                                                            \
    transpose (MC c1 c2 c3) = case transposeM/**/VT c1 c2 c3 of                    \
       {(# r1, r2, r3 #) -> MC r1 r2 r3};                                         \
    {-# INLINE det #-};                                                                  \
    det (MC c1 c2 c3) = case unpack/**/VT (detM/**/VT c1 c2 c3) of                 \
       {(# r1, _, _, _ #) -> EC2(EC r1)};                                                \
    {-# INLINE trace #-};                                                                \
    trace (MC a1 a2 a3) = case (# unpack/**/VT a1                                     \
                                , unpack/**/VT a2                                     \
                                , unpack/**/VT a3 #) of                               \
       {(#(# x11,  _ ,  _ ,  _  #)                                                       \
         ,(#  _ , x22,  _ ,  _  #)                                                       \
         ,(#  _ ,  _ , x33,  _  #)                                                       \
         #) -> EC2(EC (x11 pOp x22 pOp x33))};                                   \
    {-# INLINE fromDiag #-};                                                             \
    fromDiag (MC a1 a2 a3) = case (# unpack/**/VT a1                                  \
                                   , unpack/**/VT a2                                  \
                                   , unpack/**/VT a3 #) of                            \
       {(#(# x11,  _ ,  _ ,  _  #)                                                       \
         ,(#  _ , x22,  _ ,  _  #)                                                       \
         ,(#  _ ,  _ , x33,  _  #)                                                       \
         #) -> VC (pack/**/VT (# x11, x22, x33, 0/**/e #))};                             \
    {-# INLINE toDiag #-};                                                               \
    toDiag (VC a) = case unpack/**/VT a of                                               \
       {(# x11, x22, x33, _ #) -> MC (pack/**/VT (# x11 , 0/**/e, 0/**/e, 0/**/e #))   \
                                     (pack/**/VT (# 0/**/e, x22 , 0/**/e, 0/**/e #))   \
                                     (pack/**/VT (# 0/**/e, 0/**/e, x33 , 0/**/e #))}; \
    {-# INLINE (.*.) #-};                                                                \
    VC a .*. VC b = VC (dot/**/VT a b);                                                  \
    {-# INLINE dot #-} ;                                                                 \
    dot (VC a) (VC b) = case unpack/**/VT (dot/**/VT a b) of                             \
       {(# r1, _, _, _ #) -> EC2(EC r1)}                                                 }



VECTORMATH3(Float,FloatX3#,V3F,M3F,F#,.0#,`plusFloat#`,emptyc)
VECTORMATH3(CFloat,FloatX3#,V3CF,M3CF,F#,.0#,`plusFloat#`,CFloat)

-- params: type, vectortype, Vector constr, Matrix constr, Elem constr, Elem newtype
#define VECTOR3MATH(T,VT,VC,MC,EC,EC2,e)                          \
instance Vector3Math T where {                                  \
    {-# INLINE vector3 #-};                                     \
    vector3 (EC2(EC x)) (EC2(EC y)) (EC2(EC z))     \
        = VC (pack/**/VT (# x, y, z, 0/**/e #));                     \
    {-# INLINE matrix3x3 #-};                                   \
    matrix3x3 (VC c1) (VC c2) (VC c3) = MC c1 c2 c3  }

VECTOR3MATH(Float,FloatX3#,V3F,M3F,F#,emptyc,.0#)
VECTOR3MATH(CFloat,FloatX3#,V3CF,M3CF,F#,CFloat,.0#)

#define MATRIXPRODUCT3(T,VT,VC,MC)                                 \
instance MatrixProduct Matrix 3 T where {                          \
    {-# INLINE prod #-};                                           \
    prod (MC a1 a2 a3) (MC b1 b2 b3)                         \
        = case prodMM/**/VT a1 a2 a3 b1 b2 b3 of             \
          {(# r1, r2, r3 #) -> MC r1 r2 r3}};               \
instance MatrixProduct Vector 3 T where {                          \
    {-# INLINE prod #-};                                           \
    prod (MC a1 a2 a3) (VC b) = VC (prodMV/**/VT a1 a2 a3 b) }

MATRIXPRODUCT3(Float,FloatX3#,V3F,M3F)
MATRIXPRODUCT3(CFloat,FloatX3#,V3CF,M3CF)

#define VECTORFRACMATH3(T,VT,VC,MC)                               \
instance VectorFracMath 3 T where {                               \
    {-# INLINE inverse #-};                                       \
    inverse (MC c1 c2 c3) = case inverseM/**/VT c1 c2 c3 of \
        (# r1, r2, r3 #) -> MC r1 r2 r3                    }

VECTORFRACMATH3(Float,FloatX3#,V3F,M3F)
VECTORFRACMATH3(CFloat,FloatX3#,V3CF,M3CF)

#endif
