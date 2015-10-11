{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Geometry.Instances.FloatingJS
-- Copyright   :  Copyright (C) 2015 Artem M. Chirkin <chirkin@arch.ethz.ch>
-- License     :  BSD3
--
-- Maintainer  :  Artem M. Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  Experimental
-- Portability :
--
--
-----------------------------------------------------------------------------

module Data.Geometry.Instances.FloatingJS () where

import Data.Geometry.VectorMath
import Data.Geometry.Instances.Num ()
import Data.Geometry.Instances.Fractional ()


import GHC.TypeLits (KnownNat)

import Data.Geometry.Prim.JSNum

import GHCJS.Types

instance (KnownNat n, JSNum t, Floating t)
         => Floating (Vector n t) where
    {-# SPECIALIZE instance Floating (Vector 4 Float) #-}
    {-# SPECIALIZE instance Floating (Vector 4 Double) #-}
    {-# SPECIALIZE instance Floating (Vector 3 Float) #-}
    {-# SPECIALIZE instance Floating (Vector 3 Double) #-}
    {-# SPECIALIZE instance Floating (Vector 2 Float) #-}
    {-# SPECIALIZE instance Floating (Vector 2 Double) #-}
    {-# INLINE pi #-}
    pi = toVector . piJSVec $ dim (undefined :: Vector n t)
    {-# INLINE exp #-}
    exp = toVector . expJSVec . jsref
    {-# INLINE log #-}
    log = toVector . logJSVec . jsref
    {-# INLINE (**) #-}
    a ** b = toVector $ powerJSVec (jsref a) (jsref b)
    {-# INLINE sqrt #-}
    sqrt = toVector . sqrtJSVec . jsref
    {-# INLINE sin #-}
    sin = toVector . sinJSVec . jsref
    {-# INLINE cos #-}
    cos = toVector . cosJSVec . jsref
    {-# INLINE tan #-}
    tan = toVector . tanJSVec . jsref
    {-# INLINE asin #-}
    asin = toVector . asinJSVec . jsref
    {-# INLINE acos #-}
    acos = toVector . acosJSVec . jsref
    {-# INLINE atan #-}
    atan = toVector . atanJSVec . jsref
    {-# INLINE sinh #-}
    sinh = toVector . sinhJSVec . jsref
    {-# INLINE cosh #-}
    cosh = toVector . coshJSVec . jsref
    {-# INLINE tanh #-}
    tanh = toVector . tanhJSVec . jsref
    {-# INLINE asinh #-}
    asinh = toVector . asinhJSVec . jsref
    {-# INLINE acosh #-}
    acosh = toVector . acoshJSVec . jsref
    {-# INLINE atanh #-}
    atanh = toVector . atanhJSVec . jsref


instance (KnownNat n, JSNum t, Floating t)
         => Floating (Matrix n t) where
    {-# SPECIALIZE instance Floating (Matrix 4 Float) #-}
    {-# SPECIALIZE instance Floating (Matrix 4 Double) #-}
    {-# SPECIALIZE instance Floating (Matrix 3 Float) #-}
    {-# SPECIALIZE instance Floating (Matrix 3 Double) #-}
    {-# SPECIALIZE instance Floating (Matrix 2 Float) #-}
    {-# SPECIALIZE instance Floating (Matrix 2 Double) #-}
    {-# INLINE pi #-}
    pi = toMatrix $ piJSVec (n*n)
        where n = dim (undefined :: Matrix n t)
    {-# INLINE exp #-}
    exp = toMatrix . expJSVec . jsref
    {-# INLINE log #-}
    log = toMatrix . logJSVec . jsref
    {-# INLINE (**) #-}
    a ** b = toMatrix $ powerJSVec (jsref a) (jsref b)
    {-# INLINE sqrt #-}
    sqrt = toMatrix . sqrtJSVec . jsref
    {-# INLINE sin #-}
    sin = toMatrix . sinJSVec . jsref
    {-# INLINE cos #-}
    cos = toMatrix . cosJSVec . jsref
    {-# INLINE tan #-}
    tan = toMatrix . tanJSVec . jsref
    {-# INLINE asin #-}
    asin = toMatrix . asinJSVec . jsref
    {-# INLINE acos #-}
    acos = toMatrix . acosJSVec . jsref
    {-# INLINE atan #-}
    atan = toMatrix . atanJSVec . jsref
    {-# INLINE sinh #-}
    sinh = toMatrix . sinhJSVec . jsref
    {-# INLINE cosh #-}
    cosh = toMatrix . coshJSVec . jsref
    {-# INLINE tanh #-}
    tanh = toMatrix . tanhJSVec . jsref
    {-# INLINE asinh #-}
    asinh = toMatrix . asinhJSVec . jsref
    {-# INLINE acosh #-}
    acosh = toMatrix . acoshJSVec . jsref
    {-# INLINE atanh #-}
    atanh = toMatrix . atanhJSVec . jsref
