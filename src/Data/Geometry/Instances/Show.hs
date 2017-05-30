{-# LANGUAGE MagicHash, UnboxedTuples #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeFamilies, DataKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -pgmP cpphs -optP-traditional -optP--cpp #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Geometry.Instances.Show
-- Copyright   :  Copyright (C) 2015 Artem M. Chirkin <chirkin@arch.ethz.ch>
-- License     :  BSD3
--
-- Maintainer  :  Artem M. Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  Experimental
-- Portability :
--
--
-----------------------------------------------------------------------------

module Data.Geometry.Instances.Show () where



import Data.Geometry.VectorMath

#if defined(ghcjs_HOST_OS)

import Data.Coerce (coerce)
import JsHs.JSString (unpack')
import GHC.TypeLits (KnownNat)

import Data.Geometry.Prim.JSNum

instance Show (Vector n a) where
    show = unpack' . showJSVec . coerce

instance KnownNat n => Show (Matrix n a) where
    show m = unpack' . showJSMat (coerce m) $ dim m


#else

import GHC.Exts
import GHC.Int

import Foreign.C.Types

import Data.Geometry.Prim.FloatX3
import Data.Geometry.Types

#define emptyc(x) x

-- params: type, vectortype, Vector constr, Matrix constr, Elem constr, Elem newtype
#define SHOW4(T,VT,VC,MC,EC, EC2)                                           \
instance Show (Vector 4 T) where {     \
    show (VC a) = case unpack/**/VT a of \
        {(# a1, a2, a3, a4 #) -> "Vec " ++ f a1 ++ f a2 ++ f a3 ++ f a4} \
        where f x = ' ' : show (EC2(EC x))};   \
instance Show (Matrix 4 T) where {     \
    show (MC c1 c2 c3 c4) = "Mat\n"      \
        ++ f m11 ++ f m12 ++ f m13 ++ f m14 ++ "\n"      \
        ++ f m21 ++ f m22 ++ f m23 ++ f m24 ++ "\n"      \
        ++ f m31 ++ f m32 ++ f m33 ++ f m34 ++ "\n"      \
        ++ f m41 ++ f m42 ++ f m43 ++ f m44      \
        where {(# m11, m21, m31, m41 #) = unpack/**/VT c1;      \
               (# m12, m22, m32, m42 #) = unpack/**/VT c2 ;     \
               (# m13, m23, m33, m43 #) = unpack/**/VT c3 ;     \
               (# m14, m24, m34, m44 #) = unpack/**/VT c4 ;     \
               f x = ' ' : show (EC2(EC x))  }                }


SHOW4(Int32,Int32X4#,V4I32,M4I32,I32#, emptyc)
SHOW4(Int,Int32X4#,V4I,M4I,I#, emptyc)
SHOW4(CInt,Int32X4#,V4CI,M4CI,I32#,CInt)

SHOW4(Float,FloatX4#,V4F,M4F,F#,emptyc)
SHOW4(CFloat,FloatX4#,V4CF,M4CF,F#,CFloat)


-- params: type, vectortype, Vector constr, Matrix constr, Elem constr, Elem newtype
#define SHOW3(T,VT,VC,MC,EC, EC2)                                           \
instance Show (Vector 3 T) where {     \
    show (VC a) = case unpack/**/VT a of \
        {(# a1, a2, a3, _ #) -> "Vec " ++ f a1 ++ f a2 ++ f a3} \
        where f x = ' ' : show (EC2(EC x))};   \
instance Show (Matrix 3 T) where {     \
    show (MC c1 c2 c3) = "Mat\n"      \
        ++ f m11 ++ f m12 ++ f m13 ++ "\n"      \
        ++ f m21 ++ f m22 ++ f m23 ++ "\n"      \
        ++ f m31 ++ f m32 ++ f m33     \
        where {(# m11, m21, m31, _ #) = unpack/**/VT c1;      \
               (# m12, m22, m32, _ #) = unpack/**/VT c2 ;     \
               (# m13, m23, m33, _ #) = unpack/**/VT c3 ;     \
               f x = ' ' : show (EC2(EC x))  }                }

SHOW3(Float,FloatX3#,V3F,M3F,F#,emptyc)
SHOW3(CFloat,FloatX3#,V3CF,M3CF,F#,CFloat)

#endif
