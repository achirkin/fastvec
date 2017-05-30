{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeFamilies, DataKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -pgmP cpphs -optP-traditional -optP--cpp #-}

#if defined(ghcjs_HOST_OS)
{-# LANGUAGE ScopedTypeVariables #-}
#else
{-# LANGUAGE MagicHash, UnboxedTuples #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Geometry.Instances.Storable
-- Copyright   :  Copyright (C) 2015 Artem M. Chirkin <chirkin@arch.ethz.ch>
-- License     :  BSD3
--
-- Maintainer  :  Artem M. Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  Experimental
--
--
-----------------------------------------------------------------------------

module Data.Geometry.Instances.Storable () where


import Foreign.Storable
import Foreign.C.Types
import Data.Int
import Data.Word

import Data.Geometry.VectorMath


#if defined(ghcjs_HOST_OS)

import Data.Coerce (coerce)

import GHC.TypeLits (KnownNat)
import JsHs.Types.Prim
import JsHs.Types

import Data.Geometry.Prim.JSNum

#define STORABLEV(T,JSType,JSSize) \
instance (KnownNat n) => Storable (Vector n T) where { \
    {-# SPECIALIZE instance Storable (Vector 4 T) #-};\
    {-# SPECIALIZE instance Storable (Vector 3 T) #-};\
    {-# SPECIALIZE instance Storable (Vector 2 T) #-};\
    {-# INLINE sizeOf #-};\
    sizeOf _ = div JSSize 8 * dim (undefined :: Vector n T);\
    {-# INLINE alignment #-};\
    alignment _ = div JSSize 8;\
    {-# INLINE peekElemOff #-};\
    peekElemOff ptr offset = case fromPtr ptr of {\
        JSVal ref -> return\
                . coerce\
                . readElemOffJSVec/**/JSType/**/JSSize ref (offset*dim (undefined :: Vector n T))\
                $ dim (undefined :: Vector n T)};\
    {-# INLINE peekByteOff #-};\
    peekByteOff ptr offset = case fromPtr ptr of {\
        JSVal ref -> return\
                . coerce\
                . readByteOffJSVec/**/JSType/**/JSSize ref offset\
                $ dim (undefined :: Vector n T)};\
    {-# INLINE peek #-};\
    peek ptr = case fromPtr ptr of {\
        JSVal ref -> return\
                . coerce\
                . readElemOffJSVec/**/JSType/**/JSSize ref 0\
                $ dim (undefined :: Vector n T)};\
    {-# INLINE pokeElemOff #-};\
    pokeElemOff ptr offset v = case fromPtr ptr of {\
        JSVal ref -> writeElemOffJSVec/**/JSType/**/JSSize ref (offset*dim (undefined :: Vector n T)) (coerce v)};\
    {-# INLINE pokeByteOff #-};\
    pokeByteOff ptr offset v = case fromPtr ptr of {\
        JSVal ref -> writeByteOffJSVec/**/JSType/**/JSSize ref offset (coerce v)};\
    {-# INLINE poke #-};\
    poke ptr v = case fromPtr ptr of {\
        JSVal ref -> writeElemOffJSVec/**/JSType/**/JSSize ref 0 (coerce v)}}

STORABLEV(Int,Int,32)
STORABLEV(Int32,Int,32)
STORABLEV(Int16,Int,16)
STORABLEV(Int8,Int,8)
STORABLEV(Word,Uint,32)
STORABLEV(Word32,Uint,32)
STORABLEV(Word16,Uint,16)
STORABLEV(Word8,Uint,8)
STORABLEV(Float,Float,32)
STORABLEV(Double,Float,64)
STORABLEV(CChar,Int,8)
STORABLEV(CSChar,Int,8)
STORABLEV(CUChar,Uint,8)
STORABLEV(CShort,Int,16)
STORABLEV(CUShort,Uint,16)
STORABLEV(CInt,Int,32)
STORABLEV(CUInt,Uint,32)
STORABLEV(CLong,Int,32)
STORABLEV(CULong,Uint,32)
STORABLEV(CFloat,Float,32)
STORABLEV(CDouble,Float,64)


#define STORABLEM(T,JSType,JSSize) \
instance (KnownNat n) => Storable (Matrix n T) where { \
    {-# SPECIALIZE instance Storable (Matrix 4 T) #-};\
    {-# SPECIALIZE instance Storable (Matrix 3 T) #-};\
    {-# SPECIALIZE instance Storable (Matrix 2 T) #-};\
    {-# INLINE sizeOf #-};\
    sizeOf _ = case dim (undefined :: Vector n T) of {n -> div JSSize 8 *n*n};\
    {-# INLINE alignment #-};\
    alignment _ = div JSSize 8;\
    {-# INLINE peekElemOff #-};\
    peekElemOff ptr offset = case (fromPtr ptr, dim (undefined :: Vector n T)) of {\
        (JSVal ref, n) -> return\
                . coerce\
                . readElemOffJSVec/**/JSType/**/JSSize ref (offset*n*n)\
                $ n*n};\
    {-# INLINE peekByteOff #-};\
    peekByteOff ptr offset = case (fromPtr ptr, dim (undefined :: Vector n T)) of {\
        (JSVal ref, n) -> return\
                . coerce\
                . readByteOffJSVec/**/JSType/**/JSSize ref offset\
                $ n*n};\
    {-# INLINE peek #-};\
    peek ptr = case (fromPtr ptr, dim (undefined :: Vector n T)) of {\
        (JSVal ref, n) -> return\
                . coerce\
                . readElemOffJSVec/**/JSType/**/JSSize ref 0\
                $ n*n};\
    {-# INLINE pokeElemOff #-};\
    pokeElemOff ptr offset v = case (fromPtr ptr, dim (undefined :: Vector n T)) of {\
        (JSVal ref,n) -> writeElemOffJSVec/**/JSType/**/JSSize ref (offset*n*n) (coerce v)};\
    {-# INLINE pokeByteOff #-};\
    pokeByteOff ptr offset v = case fromPtr ptr of {\
        JSVal ref -> writeByteOffJSVec/**/JSType/**/JSSize ref offset (coerce v)};\
    {-# INLINE poke #-};\
    poke ptr v = case fromPtr ptr of {\
        JSVal ref -> writeElemOffJSVec/**/JSType/**/JSSize ref 0 (coerce v)}}


STORABLEM(Int,Int,32)
STORABLEM(Int32,Int,32)
STORABLEM(Int16,Int,16)
STORABLEM(Int8,Int,8)
STORABLEM(Word,Uint,32)
STORABLEM(Word32,Uint,32)
STORABLEM(Word16,Uint,16)
STORABLEM(Word8,Uint,8)
STORABLEM(Float,Float,32)
STORABLEM(Double,Float,64)
STORABLEM(CChar,Int,8)
STORABLEM(CSChar,Int,8)
STORABLEM(CUChar,Uint,8)
STORABLEM(CShort,Int,16)
STORABLEM(CUShort,Uint,16)
STORABLEM(CInt,Int,32)
STORABLEM(CUInt,Uint,32)
STORABLEM(CLong,Int,32)
STORABLEM(CULong,Uint,32)
STORABLEM(CFloat,Float,32)
STORABLEM(CDouble,Float,64)

#else


import GHC.Exts
import GHC.Base (IO (..))


import Data.Geometry.Types

#define emptyc(x) x

-- params: type, vectortype, Vector constr, size and align in bytes
#define STORABLEV(T,VT,P,VC,SIZE,ALIGN)                                   \
instance Storable (Vector P T) where {     \
    sizeOf _ = SIZE;      \
    alignment _ = ALIGN;     \
    peekElemOff (Ptr a) (I# i) = IO $ \s -> case read/**/VT/**/OffAddr# a (i *# SIZE#) s      \
        of {(# s2, x #) -> (# s2, VC x #)};      \
    pokeElemOff (Ptr a) (I# i) (VC x) = IO $ \s -> case write/**/VT/**/OffAddr# a (i *# SIZE#) x s      \
        of {s2 -> (# s2, () #)};      \
    peekByteOff (Ptr a) (I# i) = IO $ \s -> case read/**/VT/**/OffAddr# a i s      \
        of {(# s2, x #) -> (# s2, VC x #) };      \
    pokeByteOff (Ptr a) (I# i) (VC x) = IO $ \s -> case write/**/VT/**/OffAddr# a i x s      \
        of {s2 -> (# s2, () #) };     \
    peek (Ptr a) = IO $ \s -> case read/**/VT/**/OffAddr# a 0# s      \
        of {(# s2, x #) -> (# s2, VC x #)};      \
    poke (Ptr a) (VC x) = IO $ \s -> case write/**/VT/**/OffAddr# a 0# x s      \
        of {s2 -> (# s2, () #)}  }

STORABLEV(Int,Int32X4,4,V4I,16,16)
STORABLEV(Int32,Int32X4,4,V4I32,16,16)
STORABLEV(CInt,Int32X4,4,V4CI,16,16)

STORABLEV(Float,FloatX4,4,V4F,16,16)
STORABLEV(CFloat,FloatX4,4,V4CF,16,16)


STORABLEV(Float,FloatX4,3,V3F,16,16)
STORABLEV(CFloat,FloatX4,3,V3CF,16,16)

-- params: type, vectortype, Matrix constr, size and align in bytes
#define STORABLE4M(T,VT,MC,SIZE,VSIZE,ALIGN)                                   \
instance Storable (Matrix 4 T) where {     \
    sizeOf _ = SIZE;      \
    alignment _ = ALIGN;      \
    peekElemOff (Ptr a) (I# i) = IO $ \s -> case read/**/VT/**/OffAddr# a (i *# SIZE/**/#) s of      \
       {(# s1, x1 #) -> case read/**/VT/**/OffAddr# a (i *# SIZE/**/# +# VSIZE/**/#) s1 of      \
         (# s2, x2 #) -> case read/**/VT/**/OffAddr# a (i *# SIZE/**/# +# VSIZE/**/# *# 2#) s2 of      \
          (# s3, x3 #) -> case read/**/VT/**/OffAddr# a (i *# SIZE/**/# +# VSIZE/**/# *# 3#) s3 of      \
           (# s4, x4 #) -> (# s4, MC x1 x2 x3 x4 #) };     \
    pokeElemOff (Ptr a) (I# i) (MC x1 x2 x3 x4) = IO $ \s -> case     \
            write/**/VT/**/OffAddr# a (i *# SIZE/**/# +# VSIZE/**/# *# 3# ) x4      \
           (write/**/VT/**/OffAddr# a (i *# SIZE/**/# +# VSIZE/**/# *# 2# ) x3      \
           (write/**/VT/**/OffAddr# a (i *# SIZE/**/# +# VSIZE/**/# ) x2      \
           (write/**/VT/**/OffAddr# a (i *# SIZE/**/#) x1 s))) of {s4 -> (# s4, () #) } ;    \
    peekByteOff (Ptr a) (I# i) = IO $ \s -> case read/**/VT/**/OffAddr# a (i *# SIZE/**/#) s of      \
       {(# s1, x1 #) -> case read/**/VT/**/OffAddr# a (i +# VSIZE/**/# ) s1 of      \
         (# s2, x2 #) -> case read/**/VT/**/OffAddr# a (i +# VSIZE/**/# *# 2# ) s2 of      \
          (# s3, x3 #) -> case read/**/VT/**/OffAddr# a (i +# VSIZE/**/# *# 3# ) s3 of      \
           (# s4, x4 #) -> (# s4, MC x1 x2 x3 x4 #) } ;    \
    pokeByteOff (Ptr a) (I# i) (MC x1 x2 x3 x4) = IO $ \s ->      \
      case  write/**/VT/**/OffAddr# a (i +# VSIZE/**/# *# 3# ) x4      \
           (write/**/VT/**/OffAddr# a (i +# VSIZE/**/# *# 2# ) x3      \
           (write/**/VT/**/OffAddr# a (i +# VSIZE/**/# ) x2      \
           (write/**/VT/**/OffAddr# a i x1 s))) of { s4 -> (# s4, () #)  };    \
    peek (Ptr a) = IO $ \s -> case read/**/VT/**/OffAddr# a 0# s of      \
       {(# s1, x1 #) -> case read/**/VT/**/OffAddr# a VSIZE/**/# s1 of      \
         (# s2, x2 #) -> case read/**/VT/**/OffAddr# a ( VSIZE/**/# *# 2# ) s2 of      \
          (# s3, x3 #) -> case read/**/VT/**/OffAddr# a ( VSIZE/**/# *# 3# ) s3 of      \
           (# s4, x4 #) -> (# s4, MC x1 x2 x3 x4 #) };     \
    poke (Ptr a) (MC x1 x2 x3 x4) = IO $ \s ->      \
      case  write/**/VT/**/OffAddr# a ( VSIZE/**/# *# 3# ) x4      \
           (write/**/VT/**/OffAddr# a ( VSIZE/**/# *# 2# ) x3      \
           (write/**/VT/**/OffAddr# a VSIZE/**/# x2      \
           (write/**/VT/**/OffAddr# a 0# x1 s))) of { s4 -> (# s4, () #)}  }


STORABLE4M(Int,Int32X4,M4I,64,16,16)
STORABLE4M(Int32,Int32X4,M4I32,64,16,16)
STORABLE4M(CInt,Int32X4,M4CI,64,16,16)

STORABLE4M(Float,FloatX4,M4F,64,16,16)
STORABLE4M(CFloat,FloatX4,M4CF,64,16,16)



-- params: type, vectortype, Matrix constr, size and align in bytes
#define STORABLE3M(T,VT,MC,SIZE,VSIZE,ALIGN)                                   \
instance Storable (Matrix 3 T) where {     \
    sizeOf _ = SIZE;      \
    alignment _ = ALIGN;      \
    peekElemOff (Ptr a) (I# i) = IO $ \s -> case read/**/VT/**/OffAddr# a (i *# SIZE/**/#) s of      \
       {(# s1, x1 #) -> case read/**/VT/**/OffAddr# a (i *# SIZE/**/# +# VSIZE/**/#) s1 of      \
         (# s2, x2 #) -> case read/**/VT/**/OffAddr# a (i *# SIZE/**/# +# VSIZE/**/# *# 2#) s2 of      \
          (# s3, x3 #) -> (# s3, MC x1 x2 x3 #) };     \
    pokeElemOff (Ptr a) (I# i) (MC x1 x2 x3) = IO $ \s -> case     \
            write/**/VT/**/OffAddr# a (i *# SIZE/**/# +# VSIZE/**/# *# 2# ) x3      \
           (write/**/VT/**/OffAddr# a (i *# SIZE/**/# +# VSIZE/**/# ) x2      \
           (write/**/VT/**/OffAddr# a (i *# SIZE/**/#) x1 s)) of {s3 -> (# s3, () #) } ;    \
    peekByteOff (Ptr a) (I# i) = IO $ \s -> case read/**/VT/**/OffAddr# a (i *# SIZE/**/#) s of      \
       {(# s1, x1 #) -> case read/**/VT/**/OffAddr# a (i +# VSIZE/**/# ) s1 of      \
         (# s2, x2 #) -> case read/**/VT/**/OffAddr# a (i +# VSIZE/**/# *# 2# ) s2 of      \
          (# s3, x3 #) -> (# s3, MC x1 x2 x3 #) } ;    \
    pokeByteOff (Ptr a) (I# i) (MC x1 x2 x3) = IO $ \s ->      \
      case  write/**/VT/**/OffAddr# a (i +# VSIZE/**/# *# 2# ) x3      \
           (write/**/VT/**/OffAddr# a (i +# VSIZE/**/# ) x2      \
           (write/**/VT/**/OffAddr# a i x1 s)) of { s3 -> (# s3, () #)  };    \
    peek (Ptr a) = IO $ \s -> case read/**/VT/**/OffAddr# a 0# s of      \
       {(# s1, x1 #) -> case read/**/VT/**/OffAddr# a VSIZE/**/# s1 of      \
         (# s2, x2 #) -> case read/**/VT/**/OffAddr# a ( VSIZE/**/# *# 2# ) s2 of      \
          (# s3, x3 #) -> (# s3, MC x1 x2 x3 #) };     \
    poke (Ptr a) (MC x1 x2 x3) = IO $ \s ->      \
      case  write/**/VT/**/OffAddr# a ( VSIZE/**/# *# 2# ) x3      \
           (write/**/VT/**/OffAddr# a VSIZE/**/# x2      \
           (write/**/VT/**/OffAddr# a 0# x1 s)) of { s3 -> (# s3, () #)}  }



STORABLE3M(Float,FloatX4,M3F,48,16,16)
STORABLE3M(CFloat,FloatX4,M3CF,48,16,16)

#endif
