{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeFamilies, DataKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Geometry.Instances.Ord
-- Copyright   :  Copyright (C) 2015 Artem M. Chirkin <chirkin@arch.ethz.ch>
-- License     :  BSD3
--
-- Maintainer  :  Artem M. Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  Experimental
-- Portability :
--
--
-----------------------------------------------------------------------------

module Data.Geometry.Instances.Ord () where

import Data.Geometry.VectorMath
import Data.Geometry.Instances.Eq ()

#if defined(ghcjs_HOST_OS)

import Data.Geometry.Prim.JSNum

import GHCJS.Types

instance Ord (Vector n t) where
    {-# INLINE (>) #-}
    a > b = gtJSVec (jsref a) (jsref b)
    {-# INLINE (<) #-}
    a < b = ltJSVec (jsref a) (jsref b)
    {-# INLINE (>=) #-}
    a >= b = geJSVec (jsref a) (jsref b)
    {-# INLINE (<=) #-}
    a <= b = leJSVec (jsref a) (jsref b)
    {-# INLINE max #-}
    max a b = toVector $ maxJSVec (jsref a) (jsref b)
    {-# INLINE min #-}
    min a b = toVector $ minJSVec (jsref a) (jsref b)
    {-# INLINE compare #-}
    compare a b = case cmpJSVec (jsref a) (jsref b) of
        1  -> GT
        -1 -> LT
        _  -> EQ

instance Ord (Matrix n t) where
    {-# INLINE (>) #-}
    a > b = gtJSVec (jsref a) (jsref b)
    {-# INLINE (<) #-}
    a < b = ltJSVec (jsref a) (jsref b)
    {-# INLINE (>=) #-}
    a >= b = geJSVec (jsref a) (jsref b)
    {-# INLINE (<=) #-}
    a <= b = leJSVec (jsref a) (jsref b)
    {-# INLINE max #-}
    max a b = toMatrix $ maxJSVec (jsref a) (jsref b)
    {-# INLINE min #-}
    min a b = toMatrix $ minJSVec (jsref a) (jsref b)
    {-# INLINE compare #-}
    compare a b = case cmpJSVec (jsref a) (jsref b) of
        1  -> GT
        -1 -> LT
        _  -> EQ

#else

import GHC.Exts
import GHC.Int

import Foreign.C.Types

import Data.Geometry.Prim.Int32X4
import Data.Geometry.Prim.FloatX3
import Data.Geometry.Prim.FloatX4
import Data.Geometry.Types


-- params: type, vectortype, Vector constr, Matrix constr, True value in Int32#
#define ORD4(T,VT,VC,MC,TRUE)                                      \
instance Ord (Vector 4 T) where {                                  \
    {-# INLINE (<) #-};                                            \
    VC a < VC b = case lt/**/VT a b of                             \
       {TRUE -> True;                                              \
        _   -> False};                                             \
    {-# INLINE (>) #-};                                            \
    VC a > VC b = case gt/**/VT a b of                             \
       {TRUE -> True;                                              \
        _   -> False};                                             \
    {-# INLINE (<=) #-};                                           \
    VC a <= VC b = case le/**/VT a b of                            \
       {TRUE -> True;                                              \
        _   -> False};                                             \
    {-# INLINE (>=) #-};                                           \
    VC a >= VC b = case ge/**/VT a b of                            \
       {TRUE -> True;                                              \
        _   -> False};                                             \
    {-# INLINE max #-};                                            \
    VC a `max` VC b = VC (max/**/VT a b);                          \
    {-# INLINE min #-};                                            \
    VC a `min` VC b = VC (min/**/VT a b) };                        \
instance Ord (Matrix 4 T) where {                                  \
    {-# INLINE (<) #-};                                            \
    MC a1 a2 a3 a4 < MC b1 b2 b3 b4 = case lt/**/VT a1 b1 `andI#`  \
                                           lt/**/VT a2 b2 `andI#`  \
                                           lt/**/VT a3 b3 `andI#`  \
                                           lt/**/VT a4 b4 of       \
       {TRUE -> True;                                              \
        _   -> False};                                             \
    {-# INLINE (>) #-};                                            \
    MC a1 a2 a3 a4 > MC b1 b2 b3 b4 = case gt/**/VT a1 b1 `andI#`  \
                                           gt/**/VT a2 b2 `andI#`  \
                                           gt/**/VT a3 b3 `andI#`  \
                                           gt/**/VT a4 b4 of       \
       {TRUE -> True  ;                                            \
        _   -> False};                                             \
    {-# INLINE (<=) #-};                                           \
    MC a1 a2 a3 a4 <= MC b1 b2 b3 b4 = case le/**/VT a1 b1 `andI#` \
                                            le/**/VT a2 b2 `andI#` \
                                            le/**/VT a3 b3 `andI#` \
                                            le/**/VT a4 b4 of      \
       {TRUE -> True ;                                             \
        _   -> False};                                             \
    {-# INLINE (>=) #-};                                           \
    MC a1 a2 a3 a4 >= MC b1 b2 b3 b4 = case ge/**/VT a1 b1 `andI#` \
                                            ge/**/VT a2 b2 `andI#` \
                                            ge/**/VT a3 b3 `andI#` \
                                            ge/**/VT a4 b4 of      \
       {TRUE -> True ;                                             \
        _   -> False};                                             \
    {-# INLINE max #-};                                            \
    MC a1 a2 a3 a4 `max` MC b1 b2 b3 b4 = MC (max/**/VT a1 b1)     \
                                             (max/**/VT a2 b2)     \
                                             (max/**/VT a3 b3)     \
                                             (max/**/VT a4 b4);    \
    {-# INLINE min #-};                                            \
    MC a1 a2 a3 a4 `min` MC b1 b2 b3 b4 = MC (max/**/VT a1 b1)     \
                                             (max/**/VT a2 b2)     \
                                             (max/**/VT a3 b3)     \
                                             (max/**/VT a4 b4)     }


ORD4(Int32,Int32X4#,V4I32,M4I32,0xFFFF#)
ORD4(Int,Int32X4#,V4I,M4I,0xFFFF#)
ORD4(CInt,Int32X4#,V4CI,M4CI,0xFFFF#)

ORD4(Float,FloatX4#,V4F,M4F,15#)
ORD4(CFloat,FloatX4#,V4CF,M4CF,15#)




-- params: type, vectortype, Vector constr, Matrix constr, True value in Int32#
#define ORD3(T,VT,VC,MC,TRUE)                                      \
instance Ord (Vector 3 T) where {                                  \
    {-# INLINE (<) #-};                                            \
    VC a < VC b = case lt/**/VT a b of                             \
       {TRUE -> True;                                              \
        _   -> False};                                             \
    {-# INLINE (>) #-};                                            \
    VC a > VC b = case gt/**/VT a b of                             \
       {TRUE -> True;                                              \
        _   -> False};                                             \
    {-# INLINE (<=) #-};                                           \
    VC a <= VC b = case le/**/VT a b of                            \
       {TRUE -> True;                                              \
        _   -> False};                                             \
    {-# INLINE (>=) #-};                                           \
    VC a >= VC b = case ge/**/VT a b of                            \
       {TRUE -> True;                                              \
        _   -> False};                                             \
    {-# INLINE max #-};                                            \
    VC a `max` VC b = VC (max/**/VT a b);                          \
    {-# INLINE min #-};                                            \
    VC a `min` VC b = VC (min/**/VT a b) };                        \
instance Ord (Matrix 3 T) where {                                  \
    {-# INLINE (<) #-};                                            \
    MC a1 a2 a3 < MC b1 b2 b3 = case lt/**/VT a1 b1 `andI#`  \
                                           lt/**/VT a2 b2 `andI#`  \
                                           lt/**/VT a3 b3 of       \
       {TRUE -> True;                                              \
        _   -> False};                                             \
    {-# INLINE (>) #-};                                            \
    MC a1 a2 a3 > MC b1 b2 b3 = case gt/**/VT a1 b1 `andI#`  \
                                           gt/**/VT a2 b2 `andI#`  \
                                           gt/**/VT a3 b3 of       \
       {TRUE -> True  ;                                            \
        _   -> False};                                             \
    {-# INLINE (<=) #-};                                           \
    MC a1 a2 a3 <= MC b1 b2 b3 = case le/**/VT a1 b1 `andI#` \
                                            le/**/VT a2 b2 `andI#` \
                                            le/**/VT a3 b3 of      \
       {TRUE -> True ;                                             \
        _   -> False};                                             \
    {-# INLINE (>=) #-};                                           \
    MC a1 a2 a3 >= MC b1 b2 b3 = case ge/**/VT a1 b1 `andI#` \
                                            ge/**/VT a2 b2 `andI#` \
                                            ge/**/VT a3 b3 of      \
       {TRUE -> True ;                                             \
        _   -> False};                                             \
    {-# INLINE max #-};                                            \
    MC a1 a2 a3 `max` MC b1 b2 b3 = MC (max/**/VT a1 b1)     \
                                             (max/**/VT a2 b2)     \
                                             (max/**/VT a3 b3);    \
    {-# INLINE min #-};                                            \
    MC a1 a2 a3 `min` MC b1 b2 b3 = MC (max/**/VT a1 b1)     \
                                             (max/**/VT a2 b2)     \
                                             (max/**/VT a3 b3)     }


ORD3(Float,FloatX3#,V3F,M3F,15#)
ORD3(CFloat,FloatX3#,V3CF,M3CF,15#)

#endif
