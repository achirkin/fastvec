{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Geometry.Instances.Real
-- Copyright   :  Copyright (C) 2015 Artem M. Chirkin <chirkin@arch.ethz.ch>
-- License     :  BSD3
--
-- Maintainer  :  Artem M. Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  Experimental
-- Portability :
--
--
-----------------------------------------------------------------------------

module Data.Geometry.Instances.Real () where



import GHC.TypeLits (KnownNat)
import Data.Coerce (coerce)

import Data.Geometry.Types
import Data.Geometry.Prim.JSNum
import Data.Geometry.VectorMath
import Data.Geometry.Instances.Num ()
import Data.Geometry.Instances.Ord ()

instance (Num t, JSNum t, Real t, KnownNat n) => Real (Vector n t) where
    {-# INLINE toRational #-}
    toRational = toRational . indexVector 0

{-# RULES
"realToFrac/JSVectorNN" realToFrac = coerce :: Vector n a -> Vector n b
"realToFrac/JSVector23" realToFrac = realToFracVecNM  :: Vector 2 a -> Vector 3 b
"realToFrac/JSVector24" realToFrac = realToFracVecNM  :: Vector 2 a -> Vector 4 b
"realToFrac/JSVector25" realToFrac = realToFracVecNM  :: Vector 2 a -> Vector 5 b
"realToFrac/JSVector26" realToFrac = realToFracVecNM  :: Vector 2 a -> Vector 6 b
"realToFrac/JSVector32" realToFrac = realToFracVecNM  :: Vector 3 a -> Vector 2 b
"realToFrac/JSVector34" realToFrac = realToFracVecNM  :: Vector 3 a -> Vector 4 b
"realToFrac/JSVector35" realToFrac = realToFracVecNM  :: Vector 3 a -> Vector 5 b
"realToFrac/JSVector36" realToFrac = realToFracVecNM  :: Vector 3 a -> Vector 6 b
"realToFrac/JSVector42" realToFrac = realToFracVecNM  :: Vector 4 a -> Vector 2 b
"realToFrac/JSVector43" realToFrac = realToFracVecNM  :: Vector 4 a -> Vector 3 b
"realToFrac/JSVector45" realToFrac = realToFracVecNM  :: Vector 4 a -> Vector 5 b
"realToFrac/JSVector46" realToFrac = realToFracVecNM  :: Vector 4 a -> Vector 6 b
"realToFrac/JSVector52" realToFrac = realToFracVecNM  :: Vector 5 a -> Vector 2 b
"realToFrac/JSVector53" realToFrac = realToFracVecNM  :: Vector 5 a -> Vector 3 b
"realToFrac/JSVector54" realToFrac = realToFracVecNM  :: Vector 5 a -> Vector 4 b
"realToFrac/JSVector56" realToFrac = realToFracVecNM  :: Vector 5 a -> Vector 6 b
"realToFrac/JSVector62" realToFrac = realToFracVecNM  :: Vector 6 a -> Vector 2 b
"realToFrac/JSVector63" realToFrac = realToFracVecNM  :: Vector 6 a -> Vector 3 b
"realToFrac/JSVector64" realToFrac = realToFracVecNM  :: Vector 6 a -> Vector 4 b
"realToFrac/JSVector65" realToFrac = realToFracVecNM  :: Vector 6 a -> Vector 5 b
    #-}

realToFracVecNM :: (KnownNat n, KnownNat m) => Vector n a -> Vector m b
realToFracVecNM = f . resizeVector
    where f ::Vector m a -> Vector m b
          f = coerce

instance (Num t, JSNum t, Real t, KnownNat n) => Real (Matrix n t) where
    {-# INLINE toRational #-}
    toRational = toRational . indexMatrix 0 0


{-# RULES
"realToFrac/JSMatrixNN" realToFrac = coerce :: Matrix n a -> Matrix n b
"realToFrac/JSMatrix23" realToFrac = realToFracMatNM  :: Matrix 2 a -> Matrix 3 b
"realToFrac/JSMatrix24" realToFrac = realToFracMatNM  :: Matrix 2 a -> Matrix 4 b
"realToFrac/JSMatrix25" realToFrac = realToFracMatNM  :: Matrix 2 a -> Matrix 5 b
"realToFrac/JSMatrix26" realToFrac = realToFracMatNM  :: Matrix 2 a -> Matrix 6 b
"realToFrac/JSMatrix32" realToFrac = realToFracMatNM  :: Matrix 3 a -> Matrix 2 b
"realToFrac/JSMatrix34" realToFrac = realToFracMatNM  :: Matrix 3 a -> Matrix 4 b
"realToFrac/JSMatrix35" realToFrac = realToFracMatNM  :: Matrix 3 a -> Matrix 5 b
"realToFrac/JSMatrix36" realToFrac = realToFracMatNM  :: Matrix 3 a -> Matrix 6 b
"realToFrac/JSMatrix42" realToFrac = realToFracMatNM  :: Matrix 4 a -> Matrix 2 b
"realToFrac/JSMatrix43" realToFrac = realToFracMatNM  :: Matrix 4 a -> Matrix 3 b
"realToFrac/JSMatrix45" realToFrac = realToFracMatNM  :: Matrix 4 a -> Matrix 5 b
"realToFrac/JSMatrix46" realToFrac = realToFracMatNM  :: Matrix 4 a -> Matrix 6 b
"realToFrac/JSMatrix52" realToFrac = realToFracMatNM  :: Matrix 5 a -> Matrix 2 b
"realToFrac/JSMatrix53" realToFrac = realToFracMatNM  :: Matrix 5 a -> Matrix 3 b
"realToFrac/JSMatrix54" realToFrac = realToFracMatNM  :: Matrix 5 a -> Matrix 4 b
"realToFrac/JSMatrix56" realToFrac = realToFracMatNM  :: Matrix 5 a -> Matrix 6 b
"realToFrac/JSMatrix62" realToFrac = realToFracMatNM  :: Matrix 6 a -> Matrix 2 b
"realToFrac/JSMatrix63" realToFrac = realToFracMatNM  :: Matrix 6 a -> Matrix 3 b
"realToFrac/JSMatrix64" realToFrac = realToFracMatNM  :: Matrix 6 a -> Matrix 4 b
"realToFrac/JSMatrix65" realToFrac = realToFracMatNM  :: Matrix 6 a -> Matrix 5 b
    #-}

realToFracMatNM :: (KnownNat n, KnownNat m) => Matrix n a -> Matrix m b
realToFracMatNM = f . resizeMatrix
    where f ::Matrix m a -> Matrix m b
          f = coerce
