{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeFamilies, DataKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Geometry.Instances.Eq
-- Copyright   :  Copyright (C) 2015 Artem M. Chirkin <chirkin@arch.ethz.ch>
-- License     :  BSD3
--
-- Maintainer  :  Artem M. Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  Experimental
-- Portability :
--
--
-----------------------------------------------------------------------------

module Data.Geometry.Instances.Eq () where


import GHC.Exts
import GHC.Int

import Foreign.C.Types

import Data.Geometry.VectorMath
import Data.Geometry.Prim.Int32X4
import Data.Geometry.Prim.FloatX3
import Data.Geometry.Prim.FloatX4
import Data.Geometry.Types

-- params: type, vectortype, Vector constr, Matrix constr, True value in Int32#
#define EQ4(T,VT,VC,MC,TRUE)                                           \
instance Eq (Vector 4 T) where {                                       \
    {-# INLINE (==) #-};                                               \
    (VC a) == (VC b) = case eq/**/VT a b of                            \
       {TRUE -> True;                                                  \
         _   -> False};                                                \
    {-# INLINE (/=) #-};                                               \
    (VC a) /= (VC b) = case eq/**/VT a b of                            \
       {TRUE -> False;                                                 \
         _   -> True } };                                              \
instance Eq (Matrix 4 T) where {                                       \
    {-# INLINE (==) #-};                                               \
    (MC a1 a2 a3 a4) == (MC b1 b2 b3 b4) = case eq/**/VT a1 b1 `andI#` \
                                                eq/**/VT a2 b2 `andI#` \
                                                eq/**/VT a3 b3 `andI#` \
                                                eq/**/VT a4 b4 of      \
       {TRUE -> True;                                                  \
         _   -> False };                                               \
    {-# INLINE (/=) #-};                                               \
    (MC a1 a2 a3 a4) /= (MC b1 b2 b3 b4) = case eq/**/VT a1 b1 `andI#` \
                                                eq/**/VT a2 b2 `andI#` \
                                                eq/**/VT a3 b3 `andI#` \
                                                eq/**/VT a4 b4 of      \
       {TRUE -> False;                                                 \
         _   -> True }                                                 }


EQ4(Int32,Int32X4#,V4I32,M4I32,0xFFFF#)
EQ4(Int,Int32X4#,V4I,M4I,0xFFFF#)
EQ4(CInt,Int32X4#,V4CI,M4CI,0xFFFF#)

EQ4(Float,FloatX4#,V4F,M4F,15#)
EQ4(CFloat,FloatX4#,V4CF,M4CF,15#)


-- params: type, vectortype, Vector constr, Matrix constr, True value in Int32#
#define EQ3(T,VT,VC,MC,TRUE)                                           \
instance Eq (Vector 3 T) where {                                       \
    {-# INLINE (==) #-};                                               \
    (VC a) == (VC b) = case eq/**/VT a b of                            \
       {TRUE -> True;                                                  \
         _   -> False};                                                \
    {-# INLINE (/=) #-};                                               \
    (VC a) /= (VC b) = case eq/**/VT a b of                            \
       {TRUE -> False;                                                 \
         _   -> True } };                                              \
instance Eq (Matrix 3 T) where {                                       \
    {-# INLINE (==) #-};                                               \
    (MC a1 a2 a3) == (MC b1 b2 b3) = case eq/**/VT a1 b1 `andI#` \
                                                eq/**/VT a2 b2 `andI#` \
                                                eq/**/VT a3 b3 of      \
       {TRUE -> True;                                                  \
         _   -> False };                                               \
    {-# INLINE (/=) #-};                                               \
    (MC a1 a2 a3) /= (MC b1 b2 b3) = case eq/**/VT a1 b1 `andI#` \
                                                eq/**/VT a2 b2 `andI#` \
                                                eq/**/VT a3 b3 of      \
       {TRUE -> False;                                                 \
         _   -> True }                                                 }


EQ3(Float,FloatX3#,V3F,M3F,15#)
EQ3(CFloat,FloatX3#,V3CF,M3CF,15#)
