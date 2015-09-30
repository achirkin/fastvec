{-# LANGUAGE MagicHash, UnboxedTuples #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeFamilies, DataKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Geometry.Instances.Storable
-- Copyright   :  Copyright (C) 2015 Artem M. Chirkin <chirkin@arch.ethz.ch>
-- License     :  BSD3
--
-- Maintainer  :  Artem M. Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  Experimental
-- Portability :
--
--
-----------------------------------------------------------------------------

module Data.Geometry.Instances.Storable () where


import GHC.Exts
import GHC.Int
import GHC.Base (IO (..))

import Foreign.C.Types
import Foreign.Storable

import Data.Geometry.VectorMath
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
