{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash, UnboxedTuples #-}
{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Geometry.Types.FloatX4
-- Copyright   :  Copyright (C) 2015 Artem M. Chirkin <chirkin@arch.ethz.ch>
-- License     :  BSD3
--
-- Maintainer  :  Artem M. Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  Experimental
--
--
-----------------------------------------------------------------------------

module Data.Geometry.Types.FloatX4 where


import qualified GHC.Exts as Exts
import Data.Geometry.VectorMath
import Data.Geometry.Prim.FloatX4

instance VectorMath 4 Float where
    data Vec 4 Float = Vec4 Exts.FloatX4#
    data Mat 4 Float = Mat4 Exts.FloatX8#
    broadcastVec (Exts.F# x) = Vec4 (Exts.packFloatX4# (# x, x, x, x #))
    {-# INLINE broadcastVec #-}
--    broadcastMat (Exts.F# x) = Mat4 (Exts.packFloatX8# (# x, x, x, x, x, x, x, x, x, x, x, x, x, x, x, x #))
--    {-# INLINE broadcastMat #-}
--    eye = Mat4 (Exts.packFloatX16# (# 1.0#, 0.0#, 0.0#, 0.0#, 0.0#, 1.0#, 0.0#, 0.0#, 0.0#, 0.0#, 1.0#, 0.0#, 0.0#, 0.0#, 0.0#, 1.0# #))
--    {-# INLINE eye #-}
--    diag (Exts.F# x) = Mat4 (Exts.packFloatX16# (# x, 0.0#, 0.0#, 0.0#, 0.0#, x, 0.0#, 0.0#, 0.0#, 0.0#, x, 0.0#, 0.0#, 0.0#, 0.0#, x #))
--    {-# INLINE diag #-}

--    -- | Transpose Mat
--    transpose :: Mat n t -> Mat n t
--    -- | Determinant of  Mat
--    det :: Mat n t -> t
--    -- | Sum of diagonal elements
--    trace :: Mat n t -> t
--    -- | Get the diagonal elements from Mat into Vec
--    fromDiag :: Mat n t -> Vec n t
--    -- | Set Vec values into the diagonal elements of Mat
--    toDiag :: Vec n t -> Mat n t
--    -- | Scalar product -- sum of Vecs' components products, propagated into whole Vec
--    infixl 7 .*.
--    (.*.) :: Vec n t -> Vec n t -> Vec n t
--    -- | Scalar product -- sum of Vecs' components products -- a scalar
--    dot :: Vec n t -> Vec n t -> t
--    -- | Get element by its index
--    indexVec :: Int -> Vec n t -> t
--    -- | Get element by its index
--    indexMat :: Int -> Int -> Mat n t -> t
--    -- | Sum of absolute values
--    normL1 :: Vec n t -> t
--    -- | hypot function (square root of squares)
--    normL2 :: Vec n t -> t
--    -- | Maximum of absolute values
--    normLPInf :: Vec n t -> t
--    -- | Minimum of absolute values
--    normLNInf :: Vec n t -> t
--    -- | Norm in Lp space
--    normLP :: Int -> Vec n t -> t
--    -- | Dimensionality of a vector
--    dim :: Vec n t -> Int
