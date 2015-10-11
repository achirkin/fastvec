{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeFamilies, DataKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
#if defined(ghcjs_HOST_OS)
{-# LANGUAGE ScopedTypeVariables #-}
#else
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Geometry.Instances.Fractional
-- Copyright   :  Copyright (C) 2015 Artem M. Chirkin <chirkin@arch.ethz.ch>
-- License     :  BSD3
--
-- Maintainer  :  Artem M. Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  Experimental
-- Portability :
--
--
-----------------------------------------------------------------------------

module Data.Geometry.Instances.Fractional () where


import Data.Geometry.VectorMath
import Data.Geometry.Instances.Num ()


#if defined(ghcjs_HOST_OS)

import GHC.TypeLits (KnownNat)

import Data.Geometry.Prim.JSNum

import GHCJS.Types

instance (KnownNat n, JSNum t, Fractional t) => Fractional (Vector n t) where
    {-# SPECIALIZE instance Fractional (Vector 4 Float) #-}
    {-# SPECIALIZE instance Fractional (Vector 4 Double) #-}
    {-# SPECIALIZE instance Fractional (Vector 3 Float) #-}
    {-# SPECIALIZE instance Fractional (Vector 3 Double) #-}
    {-# SPECIALIZE instance Fractional (Vector 2 Float) #-}
    {-# SPECIALIZE instance Fractional (Vector 2 Double) #-}
    {-# INLINE (/) #-}
    a / b = toVector $ divideJSVec (jsref a) (jsref b)
    {-# INLINE recip #-}
    recip a = toVector $ recipJSVec (jsref a)
    {-# INLINE fromRational #-}
    fromRational i = v
        where v = toVector . broadcastJSVec (fromNum (fromRational i :: t)) $ dim v

instance (KnownNat n, JSNum t, Fractional t) => Fractional (Matrix n t) where
    {-# SPECIALIZE instance Fractional (Matrix 4 Float) #-}
    {-# SPECIALIZE instance Fractional (Matrix 4 Double) #-}
    {-# SPECIALIZE instance Fractional (Matrix 3 Float) #-}
    {-# SPECIALIZE instance Fractional (Matrix 3 Double) #-}
    {-# SPECIALIZE instance Fractional (Matrix 2 Float) #-}
    {-# SPECIALIZE instance Fractional (Matrix 2 Double) #-}
    {-# INLINE (/) #-}
    a / b = toMatrix $ divideJSVec (jsref a) (jsref b)
    {-# INLINE recip #-}
    recip a = toMatrix $ recipJSVec (jsref a)
    {-# INLINE fromRational #-}
    fromRational i = v
        where v = toMatrix $ broadcastJSVec (fromNum (fromRational i :: t)) (n*n)
              n = dim v


#else

import GHC.Exts

import Foreign.C.Types

import Data.Geometry.Prim.FloatX3
import Data.Geometry.Prim.FloatX4
import Data.Geometry.Types

#define emptyc(x) x

-- params: type, vectortype, Dimensionality, Vector constr, Elem constr, Elem newtype
#define FRACATIONALV(T,VT,P,VC,EC,EC2)                            \
instance Fractional (Vector P T) where {                           \
    {-# INLINE (/) #-};                                            \
    (VC a) / (VC b) = VC (divide/**/VT a b);                       \
    {-# INLINE recip #-};                                          \
    recip (VC a) = VC (recip/**/VT a);                             \
    {-# INLINE fromRational #-};                                   \
    fromRational r = case fromRational r of                        \
        EC2(EC f) -> VC (broadcast/**/VT f)  }


FRACATIONALV(Float,FloatX4#,4,V4F,F#, emptyc)
FRACATIONALV(CFloat,FloatX4#,4,V4CF,F#,CFloat)

FRACATIONALV(Float,FloatX3#,3,V3F,F#, emptyc)
FRACATIONALV(CFloat,FloatX3#,3,V3CF,F#,CFloat)


-- params: type, vectortype, Matrix constr, Elem constr, Elem newtype
#define FRACATIONAL4M(T,VT,MC,EC,EC2)                            \
instance Fractional (Matrix 4 T) where {                           \
    {-# INLINE (/) #-};                                            \
    (MC a1 a2 a3 a4) / (MC b1 b2 b3 b4) = MC (divide/**/VT a1 b1)  \
                                             (divide/**/VT a2 b2)  \
                                             (divide/**/VT a3 b3)  \
                                             (divide/**/VT a4 b4); \
    {-# INLINE recip #-};                                          \
    recip (MC a1 a2 a3 a4) = MC (recip/**/VT a1)                   \
                                (recip/**/VT a2)                   \
                                (recip/**/VT a3)                   \
                                (recip/**/VT a4);                  \
    {-# INLINE fromRational #-};                                   \
    fromRational r = case fromRational r of                        \
        EC2(EC f) -> case broadcast/**/VT f of b -> MC b b b b     }

FRACATIONAL4M(Float,FloatX4#,M4F,F#, emptyc)
FRACATIONAL4M(CFloat,FloatX4#,M4CF,F#,CFloat)

-- params: type, vectortype, Matrix constr, Elem constr, Elem newtype
#define FRACATIONAL3M(T,VT,MC,EC,EC2)                            \
instance Fractional (Matrix 3 T) where {                           \
    {-# INLINE (/) #-};                                            \
    (MC a1 a2 a3) / (MC b1 b2 b3) = MC (divide/**/VT a1 b1)  \
                                       (divide/**/VT a2 b2)  \
                                       (divide/**/VT a3 b3); \
    {-# INLINE recip #-};                                          \
    recip (MC a1 a2 a3) = MC (recip/**/VT a1)                   \
                             (recip/**/VT a2)                   \
                             (recip/**/VT a3);                  \
    {-# INLINE fromRational #-};                                   \
    fromRational r = case fromRational r of                        \
        EC2(EC f) -> case broadcast/**/VT f of b -> MC b b b     }

FRACATIONAL3M(Float,FloatX3#,M3F,F#, emptyc)
FRACATIONAL3M(CFloat,FloatX3#,M3CF,F#,CFloat)

#endif
