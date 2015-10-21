{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Geometry.Instances.Integral
-- Copyright   :  Copyright (C) 2015 Artem M. Chirkin <chirkin@arch.ethz.ch>
-- License     :  BSD3
--
-- Maintainer  :  Artem M. Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  Experimental
-- Portability :
--
--
-----------------------------------------------------------------------------

module Data.Geometry.Instances.Integral () where

import GHC.TypeLits (KnownNat)
import Data.Coerce (coerce)

import Data.Geometry.Types
import Data.Geometry.Prim.JSNum
import Data.Geometry.VectorMath
import Data.Geometry.Instances.Num ()
import Data.Geometry.Instances.Ord ()
import Data.Geometry.Instances.Enum ()
import Data.Geometry.Instances.Real ()

instance (Num t, Integral t, JSNum t, KnownNat n) => Integral (Vector n t) where
    {-# INLINE quot #-}
    quot a b = coerce $ js_quot (coerce a) (coerce b)
    {-# INLINE rem #-}
    rem a b = coerce $ js_rem (coerce a) (coerce b)
    {-# INLINE quotRem #-}
    quotRem a b = coerce $ js_quotRem (coerce a) (coerce b)
    {-# INLINE div #-}
    div a b = coerce $ js_div (coerce a) (coerce b)
    {-# INLINE mod #-}
    mod a b = coerce $ js_mod (coerce a) (coerce b)
    {-# INLINE divMod #-}
    divMod a b = coerce $ js_divMod (coerce a) (coerce b)
    {-# INLINE toInteger #-}
    toInteger v = toInteger $ indexVector 0 v

instance (Num t, Integral t, JSNum t, KnownNat n) => Integral (Matrix n t) where
    {-# INLINE quot #-}
    quot a b = coerce $ js_quot (coerce a) (coerce b)
    {-# INLINE rem #-}
    rem a b = coerce $ js_rem (coerce a) (coerce b)
    {-# INLINE quotRem #-}
    quotRem a b = coerce $ js_quotRem (coerce a) (coerce b)
    {-# INLINE div #-}
    div a b = coerce $ js_div (coerce a) (coerce b)
    {-# INLINE mod #-}
    mod a b = coerce $ js_mod (coerce a) (coerce b)
    {-# INLINE divMod #-}
    divMod a b = coerce $ js_divMod (coerce a) (coerce b)
    {-# INLINE toInteger #-}
    toInteger v = toInteger $ indexMatrix 0 0 v

{-# RULES
"fromIntegral/JSVectorNN" fromIntegral = coerce :: Vector n a -> Vector n b
"fromIntegral/JSVector23" fromIntegral = fromIntegralVecNM  :: Vector 2 a -> Vector 3 b
"fromIntegral/JSVector24" fromIntegral = fromIntegralVecNM  :: Vector 2 a -> Vector 4 b
"fromIntegral/JSVector25" fromIntegral = fromIntegralVecNM  :: Vector 2 a -> Vector 5 b
"fromIntegral/JSVector26" fromIntegral = fromIntegralVecNM  :: Vector 2 a -> Vector 6 b
"fromIntegral/JSVector32" fromIntegral = fromIntegralVecNM  :: Vector 3 a -> Vector 2 b
"fromIntegral/JSVector34" fromIntegral = fromIntegralVecNM  :: Vector 3 a -> Vector 4 b
"fromIntegral/JSVector35" fromIntegral = fromIntegralVecNM  :: Vector 3 a -> Vector 5 b
"fromIntegral/JSVector36" fromIntegral = fromIntegralVecNM  :: Vector 3 a -> Vector 6 b
"fromIntegral/JSVector42" fromIntegral = fromIntegralVecNM  :: Vector 4 a -> Vector 2 b
"fromIntegral/JSVector43" fromIntegral = fromIntegralVecNM  :: Vector 4 a -> Vector 3 b
"fromIntegral/JSVector45" fromIntegral = fromIntegralVecNM  :: Vector 4 a -> Vector 5 b
"fromIntegral/JSVector46" fromIntegral = fromIntegralVecNM  :: Vector 4 a -> Vector 6 b
"fromIntegral/JSVector52" fromIntegral = fromIntegralVecNM  :: Vector 5 a -> Vector 2 b
"fromIntegral/JSVector53" fromIntegral = fromIntegralVecNM  :: Vector 5 a -> Vector 3 b
"fromIntegral/JSVector54" fromIntegral = fromIntegralVecNM  :: Vector 5 a -> Vector 4 b
"fromIntegral/JSVector56" fromIntegral = fromIntegralVecNM  :: Vector 5 a -> Vector 6 b
"fromIntegral/JSVector62" fromIntegral = fromIntegralVecNM  :: Vector 6 a -> Vector 2 b
"fromIntegral/JSVector63" fromIntegral = fromIntegralVecNM  :: Vector 6 a -> Vector 3 b
"fromIntegral/JSVector64" fromIntegral = fromIntegralVecNM  :: Vector 6 a -> Vector 4 b
"fromIntegral/JSVector65" fromIntegral = fromIntegralVecNM  :: Vector 6 a -> Vector 5 b
    #-}

{-# INLINE fromIntegralVecNM #-}
fromIntegralVecNM :: (KnownNat n, KnownNat m) => Vector n a -> Vector m b
fromIntegralVecNM = f . resizeVector
    where f ::Vector m a -> Vector m b
          f = coerce

{-# RULES
"fromIntegral/JSMatrixNN" fromIntegral = coerce :: Matrix n a -> Matrix n b
"fromIntegral/JSMatrix23" fromIntegral = fromIntegralMatNM  :: Matrix 2 a -> Matrix 3 b
"fromIntegral/JSMatrix24" fromIntegral = fromIntegralMatNM  :: Matrix 2 a -> Matrix 4 b
"fromIntegral/JSMatrix25" fromIntegral = fromIntegralMatNM  :: Matrix 2 a -> Matrix 5 b
"fromIntegral/JSMatrix26" fromIntegral = fromIntegralMatNM  :: Matrix 2 a -> Matrix 6 b
"fromIntegral/JSMatrix32" fromIntegral = fromIntegralMatNM  :: Matrix 3 a -> Matrix 2 b
"fromIntegral/JSMatrix34" fromIntegral = fromIntegralMatNM  :: Matrix 3 a -> Matrix 4 b
"fromIntegral/JSMatrix35" fromIntegral = fromIntegralMatNM  :: Matrix 3 a -> Matrix 5 b
"fromIntegral/JSMatrix36" fromIntegral = fromIntegralMatNM  :: Matrix 3 a -> Matrix 6 b
"fromIntegral/JSMatrix42" fromIntegral = fromIntegralMatNM  :: Matrix 4 a -> Matrix 2 b
"fromIntegral/JSMatrix43" fromIntegral = fromIntegralMatNM  :: Matrix 4 a -> Matrix 3 b
"fromIntegral/JSMatrix45" fromIntegral = fromIntegralMatNM  :: Matrix 4 a -> Matrix 5 b
"fromIntegral/JSMatrix46" fromIntegral = fromIntegralMatNM  :: Matrix 4 a -> Matrix 6 b
"fromIntegral/JSMatrix52" fromIntegral = fromIntegralMatNM  :: Matrix 5 a -> Matrix 2 b
"fromIntegral/JSMatrix53" fromIntegral = fromIntegralMatNM  :: Matrix 5 a -> Matrix 3 b
"fromIntegral/JSMatrix54" fromIntegral = fromIntegralMatNM  :: Matrix 5 a -> Matrix 4 b
"fromIntegral/JSMatrix56" fromIntegral = fromIntegralMatNM  :: Matrix 5 a -> Matrix 6 b
"fromIntegral/JSMatrix62" fromIntegral = fromIntegralMatNM  :: Matrix 6 a -> Matrix 2 b
"fromIntegral/JSMatrix63" fromIntegral = fromIntegralMatNM  :: Matrix 6 a -> Matrix 3 b
"fromIntegral/JSMatrix64" fromIntegral = fromIntegralMatNM  :: Matrix 6 a -> Matrix 4 b
"fromIntegral/JSMatrix65" fromIntegral = fromIntegralMatNM  :: Matrix 6 a -> Matrix 5 b
    #-}

{-# INLINE fromIntegralMatNM #-}
fromIntegralMatNM :: (KnownNat n, KnownNat m) => Matrix n a -> Matrix m b
fromIntegralMatNM = f . resizeMatrix
    where f ::Matrix m a -> Matrix m b
          f = coerce

{-# INLINE gcdVector #-}
gcdVector :: Vector n a -> Vector n a -> Vector n a
gcdVector a b = coerce $ js_gcdVec (coerce a) (coerce b)

{-# INLINE gcdMatrix #-}
gcdMatrix :: Matrix n a -> Matrix n a -> Matrix n a
gcdMatrix a b = coerce $ js_gcdVec (coerce a) (coerce b)

{-# INLINE lcmVector #-}
lcmVector :: Vector n a -> Vector n a -> Vector n a
lcmVector a b = coerce $ js_lcmVec (coerce a) (coerce b)

{-# INLINE lcmMatrix #-}
lcmMatrix :: Matrix n a -> Matrix n a -> Matrix n a
lcmMatrix a b = coerce $ js_lcmVec (coerce a) (coerce b)

{-# INLINE powerVector #-}
powerVector :: Vector n a -> Vector n a -> Vector n a
powerVector a b = coerce $ powerJSVec (coerce a) (coerce b)

{-# INLINE powerMatrix #-}
powerMatrix :: Matrix n a -> Matrix n a -> Matrix n a
powerMatrix a b = coerce $ powerJSVec (coerce a) (coerce b)



{-# RULES
"gcd/JSVectorN" gcd = gcdVector :: Vector n a -> Vector n a -> Vector n a
"gcd/JSMatrixN" gcd = gcdMatrix :: Matrix n a -> Matrix n a -> Matrix n a
"lcm/JSVectorN" lcm = lcmVector :: Vector n a -> Vector n a -> Vector n a
"lcm/JSMatrixN" lcm = lcmMatrix :: Matrix n a -> Matrix n a -> Matrix n a
"^/JSVectorN"  (^)  = powerVector :: Vector n a -> Vector n a -> Vector n a
"^^/JSVectorN" (^^) = powerVector :: Vector n a -> Vector n a -> Vector n a
"^/JSMatrixN"  (^)  = powerMatrix :: Matrix n a -> Matrix n a -> Matrix n a
"^^/JSMatrixN" (^^) = powerMatrix :: Matrix n a -> Matrix n a -> Matrix n a
    #-}
