{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeFamilies, DataKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Geometry.Instances.Num
-- Copyright   :  Copyright (C) 2015 Artem M. Chirkin <chirkin@arch.ethz.ch>
-- License     :  BSD3
--
-- Maintainer  :  Artem M. Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  Experimental
-- Portability :
--
--
-----------------------------------------------------------------------------

module Data.Geometry.Instances.Num () where


import Data.Geometry.VectorMath

#if defined(ghcjs_HOST_OS)

import GHC.TypeLits (KnownNat)

import Data.Geometry.Prim.JSNum

import GHCJS.Types

instance KnownNat n => Num (Vector n t) where
    {-# SPECIALIZE instance Num (Vector 4 Float) #-}
    {-# SPECIALIZE instance Num (Vector 4 Double) #-}
    {-# SPECIALIZE instance Num (Vector 4 Int) #-}
    {-# SPECIALIZE instance Num (Vector 3 Float) #-}
    {-# SPECIALIZE instance Num (Vector 3 Double) #-}
    {-# SPECIALIZE instance Num (Vector 3 Int) #-}
    {-# SPECIALIZE instance Num (Vector 2 Float) #-}
    {-# SPECIALIZE instance Num (Vector 2 Double) #-}
    {-# SPECIALIZE instance Num (Vector 2 Int) #-}
    {-# INLINE (+) #-}
    a + b = toVector $ plusJSVec (jsref a) (jsref b)
    {-# INLINE (-) #-}
    a - b = toVector $ minusJSVec (jsref a) (jsref b)
    {-# INLINE (*) #-}
    a * b = toVector $ timesJSVec (jsref a) (jsref b)
    {-# INLINE negate #-}
    negate = toVector . negateJSVec . jsref
    {-# INLINE abs #-}
    abs = toVector . absJSVec . jsref
    {-# INLINE signum #-}
    signum = toVector . signumJSVec . jsref
    {-# INLINE fromInteger #-}
    fromInteger i = v
        where v = toVector . broadcastJSVec (fromNum (fromInteger i :: Int)) $ dim v

instance KnownNat n => Num (Matrix n t) where
    {-# SPECIALIZE instance Num (Matrix 4 Float) #-}
    {-# SPECIALIZE instance Num (Matrix 4 Double) #-}
    {-# SPECIALIZE instance Num (Matrix 4 Int) #-}
    {-# SPECIALIZE instance Num (Matrix 3 Float) #-}
    {-# SPECIALIZE instance Num (Matrix 3 Double) #-}
    {-# SPECIALIZE instance Num (Matrix 3 Int) #-}
    {-# SPECIALIZE instance Num (Matrix 2 Float) #-}
    {-# SPECIALIZE instance Num (Matrix 2 Double) #-}
    {-# SPECIALIZE instance Num (Matrix 2 Int) #-}
    {-# INLINE (+) #-}
    a + b = toMatrix $ plusJSVec (jsref a) (jsref b)
    {-# INLINE (-) #-}
    a - b = toMatrix $ minusJSVec (jsref a) (jsref b)
    {-# INLINE (*) #-}
    a * b = toMatrix $ timesJSVec (jsref a) (jsref b)
    {-# INLINE negate #-}
    negate = toMatrix . negateJSVec . jsref
    {-# INLINE abs #-}
    abs = toMatrix . absJSVec . jsref
    {-# INLINE signum #-}
    signum = toMatrix . signumJSVec . jsref
    {-# INLINE fromInteger #-}
    fromInteger i = v
        where v = toMatrix $ broadcastJSVec (fromNum (fromInteger i :: Int)) (n*n)
              n = dim v

#else

import GHC.Exts
import GHC.Int

import Foreign.C.Types

import Data.Geometry.Prim.Int32X4
import Data.Geometry.Prim.FloatX3
import Data.Geometry.Prim.FloatX4
import Data.Geometry.Types

#define emptyc(x) x

-- params: type, vectortype, Vector constr, Matrix constr, Elem constr, Elem newtype
#define NUMV(T,VT,P,VC,EC,EC2)                                   \
instance Num (Vector P T) where {                                 \
    {-# INLINE (+) #-};                                           \
    (VC a) + (VC b) = VC (plus/**/VT a b);                        \
    {-# INLINE (-) #-};                                           \
    (VC a) - (VC b) = VC (minus/**/VT a b);                       \
    {-# INLINE (*) #-};                                           \
    (VC a) * (VC b) = VC (times/**/VT a b);                       \
    {-# INLINE negate #-};                                        \
    negate (VC a) = VC (negate/**/VT a);                          \
    {-# INLINE abs #-};                                           \
    abs (VC a) = VC (abs/**/VT a);                                \
    {-# INLINE signum #-};                                        \
    signum (VC a) = VC (signum/**/VT a);                          \
    {-# INLINE fromInteger #-};                                   \
    fromInteger i = case fromInteger i of                         \
        {EC2(EC f) -> VC (broadcast/**/VT f)}}


NUMV(Int32,Int32X4#,4,V4I32,I32#, emptyc)
NUMV(Int,Int32X4#,4,V4I,I#, emptyc)
NUMV(CInt,Int32X4#,4,V4CI,I32#,CInt)

NUMV(Float,FloatX4#,4,V4F,F#, emptyc)
NUMV(CFloat,FloatX4#,4,V4CF,F#,CFloat)


NUMV(Float,FloatX3#,3,V3F,F#, emptyc)
NUMV(CFloat,FloatX3#,3,V3CF,F#,CFloat)




-- params: type, vectortype, Vector constr, Matrix constr, Elem constr, Elem newtype
#define NUM4M(T,VT,MC,EC,EC2)                                   \
instance Num (Matrix 4 T) where {                                 \
    {-# INLINE (+) #-};                                           \
    (MC a1 a2 a3 a4) + (MC b1 b2 b3 b4) = MC (plus/**/VT a1 b1)   \
                                             (plus/**/VT a2 b2)   \
                                             (plus/**/VT a3 b3)   \
                                             (plus/**/VT a4 b4);  \
    {-# INLINE (-) #-};                                           \
    (MC a1 a2 a3 a4) - (MC b1 b2 b3 b4) = MC (minus/**/VT a1 b1)  \
                                             (minus/**/VT a2 b2)  \
                                             (minus/**/VT a3 b3)  \
                                             (minus/**/VT a4 b4); \
    {-# INLINE (*) #-};                                           \
    (MC a1 a2 a3 a4) * (MC b1 b2 b3 b4) = MC (times/**/VT a1 b1)  \
                                             (times/**/VT a2 b2)  \
                                             (times/**/VT a3 b3)  \
                                             (times/**/VT a4 b4); \
    {-# INLINE negate #-};                                        \
    negate (MC a1 a2 a3 a4) = MC (negate/**/VT a1)                \
                                 (negate/**/VT a2)                \
                                 (negate/**/VT a3)                \
                                 (negate/**/VT a4);               \
    {-# INLINE abs #-};                                           \
    abs (MC a1 a2 a3 a4) = MC (abs/**/VT a1)                      \
                              (abs/**/VT a2)                      \
                              (abs/**/VT a3)                      \
                              (abs/**/VT a4);                     \
    {-# INLINE signum #-};                                        \
    signum (MC a1 a2 a3 a4) = MC (signum/**/VT a1)                \
                                 (signum/**/VT a2)                \
                                 (signum/**/VT a3)                \
                                 (signum/**/VT a4);               \
    {-# INLINE fromInteger #-};                                   \
    fromInteger i = case fromInteger i of                         \
        {EC2(EC f) -> case broadcast/**/VT f of b -> MC b b b b}  }




NUM4M(Int32,Int32X4#,M4I32,I32#, emptyc)
NUM4M(Int,Int32X4#,M4I,I#, emptyc)
NUM4M(CInt,Int32X4#,M4CI,I32#,CInt)

NUM4M(Float,FloatX4#,M4F,F#, emptyc)
NUM4M(CFloat,FloatX4#,M4CF,F#,CFloat)


-- params: type, vectortype, Vector constr, Matrix constr, Elem constr, Elem newtype
#define NUM3M(T,VT,MC,EC,EC2)                                 \
instance Num (Matrix 3 T) where {                                 \
    {-# INLINE (+) #-};                                           \
    (MC a1 a2 a3) + (MC b1 b2 b3) = MC (plus/**/VT a1 b1)   \
                                       (plus/**/VT a2 b2)   \
                                       (plus/**/VT a3 b3);  \
    {-# INLINE (-) #-};                                           \
    (MC a1 a2 a3) - (MC b1 b2 b3) = MC (minus/**/VT a1 b1)  \
                                       (minus/**/VT a2 b2)  \
                                       (minus/**/VT a3 b3); \
    {-# INLINE (*) #-};                                           \
    (MC a1 a2 a3) * (MC b1 b2 b3) = MC (times/**/VT a1 b1)  \
                                       (times/**/VT a2 b2)  \
                                       (times/**/VT a3 b3); \
    {-# INLINE negate #-};                                        \
    negate (MC a1 a2 a3) = MC (negate/**/VT a1)                \
                              (negate/**/VT a2)                \
                              (negate/**/VT a3);               \
    {-# INLINE abs #-};                                           \
    abs (MC a1 a2 a3) = MC (abs/**/VT a1)                      \
                           (abs/**/VT a2)                      \
                           (abs/**/VT a3);                     \
    {-# INLINE signum #-};                                        \
    signum (MC a1 a2 a3) = MC (signum/**/VT a1)                \
                              (signum/**/VT a2)                \
                              (signum/**/VT a3);               \
    {-# INLINE fromInteger #-};                                   \
    fromInteger i = case fromInteger i of                         \
        {EC2(EC f) -> case broadcast/**/VT f of b -> MC b b b}  }


NUM3M(Float,FloatX3#,M3F,F#, emptyc)
NUM3M(CFloat,FloatX3#,M3CF,F#,CFloat)

#endif
