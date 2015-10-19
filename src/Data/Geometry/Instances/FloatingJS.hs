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


import Data.Coerce (coerce)
import GHC.TypeLits (KnownNat)

import Data.Geometry.Prim.JSNum
import Data.Geometry.VectorMath
import Data.Geometry.Instances.Num ()
import Data.Geometry.Instances.Fractional ()



instance (KnownNat n, JSNum t, Floating t)
         => Floating (Vector n t) where
    {-# SPECIALIZE instance Floating (Vector 4 Float) #-}
    {-# SPECIALIZE instance Floating (Vector 4 Double) #-}
    {-# SPECIALIZE instance Floating (Vector 3 Float) #-}
    {-# SPECIALIZE instance Floating (Vector 3 Double) #-}
    {-# SPECIALIZE instance Floating (Vector 2 Float) #-}
    {-# SPECIALIZE instance Floating (Vector 2 Double) #-}
    {-# INLINE pi #-}
    pi = coerce . piJSVec $ dim (undefined :: Vector n t)
    {-# INLINE exp #-}
    exp = coerce . expJSVec . coerce
    {-# INLINE log #-}
    log = coerce . logJSVec . coerce
    {-# INLINE (**) #-}
    a ** b = coerce $ powerJSVec (coerce a) (coerce b)
    {-# INLINE sqrt #-}
    sqrt = coerce . sqrtJSVec . coerce
    {-# INLINE sin #-}
    sin = coerce . sinJSVec . coerce
    {-# INLINE cos #-}
    cos = coerce . cosJSVec . coerce
    {-# INLINE tan #-}
    tan = coerce . tanJSVec . coerce
    {-# INLINE asin #-}
    asin = coerce . asinJSVec . coerce
    {-# INLINE acos #-}
    acos = coerce . acosJSVec . coerce
    {-# INLINE atan #-}
    atan = coerce . atanJSVec . coerce
    {-# INLINE sinh #-}
    sinh = coerce . sinhJSVec . coerce
    {-# INLINE cosh #-}
    cosh = coerce . coshJSVec . coerce
    {-# INLINE tanh #-}
    tanh = coerce . tanhJSVec . coerce
    {-# INLINE asinh #-}
    asinh = coerce . asinhJSVec . coerce
    {-# INLINE acosh #-}
    acosh = coerce . acoshJSVec . coerce
    {-# INLINE atanh #-}
    atanh = coerce . atanhJSVec . coerce


instance (KnownNat n, JSNum t, Floating t)
         => Floating (Matrix n t) where
    {-# SPECIALIZE instance Floating (Matrix 4 Float) #-}
    {-# SPECIALIZE instance Floating (Matrix 4 Double) #-}
    {-# SPECIALIZE instance Floating (Matrix 3 Float) #-}
    {-# SPECIALIZE instance Floating (Matrix 3 Double) #-}
    {-# SPECIALIZE instance Floating (Matrix 2 Float) #-}
    {-# SPECIALIZE instance Floating (Matrix 2 Double) #-}
    {-# INLINE pi #-}
    pi = coerce $ piJSVec (n*n)
        where n = dim (undefined :: Matrix n t)
    {-# INLINE exp #-}
    exp = coerce . expJSVec . coerce
    {-# INLINE log #-}
    log = coerce . logJSVec . coerce
    {-# INLINE (**) #-}
    a ** b = coerce $ powerJSVec (coerce a) (coerce b)
    {-# INLINE sqrt #-}
    sqrt = coerce . sqrtJSVec . coerce
    {-# INLINE sin #-}
    sin = coerce . sinJSVec . coerce
    {-# INLINE cos #-}
    cos = coerce . cosJSVec . coerce
    {-# INLINE tan #-}
    tan = coerce . tanJSVec . coerce
    {-# INLINE asin #-}
    asin = coerce . asinJSVec . coerce
    {-# INLINE acos #-}
    acos = coerce . acosJSVec . coerce
    {-# INLINE atan #-}
    atan = coerce . atanJSVec . coerce
    {-# INLINE sinh #-}
    sinh = coerce . sinhJSVec . coerce
    {-# INLINE cosh #-}
    cosh = coerce . coshJSVec . coerce
    {-# INLINE tanh #-}
    tanh = coerce . tanhJSVec . coerce
    {-# INLINE asinh #-}
    asinh = coerce . asinhJSVec . coerce
    {-# INLINE acosh #-}
    acosh = coerce . acoshJSVec . coerce
    {-# INLINE atanh #-}
    atanh = coerce . atanhJSVec . coerce
