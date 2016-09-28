{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, DataKinds, KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Geometry.VectorMath
-- Copyright   :  Copyright (C) 2015 Artem M. Chirkin <chirkin@arch.ethz.ch>
-- License     :  BSD3
--
-- Maintainer  :  Artem M. Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  Experimental
--
--
-----------------------------------------------------------------------------

module Data.Geometry.VectorMath where

import GHC.TypeLits


class VectorMath (n :: Nat) t where
    data Vec (n :: Nat) t
    data Mat (n :: Nat) t
    -- | Fill Vec with the same value
    broadcastVec :: t -> Vec n t
    -- | Fill Mat with single value
    broadcastMat :: t -> Mat n t
    -- | Mat with 1 on diagonal and 0 elsewhere
    eye :: Mat n t
    -- | Put the same value on the Mat diagonal, 0 otherwise
    diag :: t -> Mat n t
    -- | Transpose Mat
    transpose :: Mat n t -> Mat n t
    -- | Determinant of  Mat
    det :: Mat n t -> t
    -- | Sum of diagonal elements
    trace :: Mat n t -> t
    -- | Get the diagonal elements from Mat into Vec
    fromDiag :: Mat n t -> Vec n t
    -- | Set Vec values into the diagonal elements of Mat
    toDiag :: Vec n t -> Mat n t
    -- | Scalar product -- sum of Vecs' components products, propagated into whole Vec
    infixl 7 .*.
    (.*.) :: Vec n t -> Vec n t -> Vec n t
    -- | Scalar product -- sum of Vecs' components products -- a scalar
    dot :: Vec n t -> Vec n t -> t
    -- | Get element by its index
    indexVec :: Int -> Vec n t -> t
    -- | Get element by its index
    indexMat :: Int -> Int -> Mat n t -> t
    -- | Sum of absolute values
    normL1 :: Vec n t -> t
    -- | hypot function (square root of squares)
    normL2 :: Vec n t -> t
    -- | Maximum of absolute values
    normLPInf :: Vec n t -> t
    -- | Minimum of absolute values
    normLNInf :: Vec n t -> t
    -- | Norm in Lp space
    normLP :: Int -> Vec n t -> t
    -- | Dimensionality of a vector
    dim :: Vec n t -> Int

{-# RULES
"normLP/L1" normLP 1 = normL1
"normLP/L2" normLP 2 = normL2
    #-}

class VectorMath n t => VectorFracMath n t where
    inverse :: Mat n t -> Mat n t


class Vec4Math t where
    vec4 :: t -> t -> t -> t -> Vec 4 t
    mat4x4 :: Vec 4 t -> Vec 4 t -> Vec 4 t -> Vec 4 t -> Mat 4 t
    unpackV4 :: Vec 4 t -> (t,t,t,t)
    rowsOfM4 :: Mat 4 t -> (Vec 4 t,Vec 4 t,Vec 4 t,Vec 4 t)
    colsOfM4 :: Mat 4 t -> (Vec 4 t,Vec 4 t,Vec 4 t,Vec 4 t)

class Vec3Math t where
    vec3 :: t -> t -> t -> Vec 3 t
    mat3x3 :: Vec 3 t -> Vec 3 t -> Vec 3 t -> Mat 3 t
    unpackV3 :: Vec 3 t -> (t,t,t)
    rowsOfM3 :: Mat 3 t -> (Vec 3 t,Vec 3 t,Vec 3 t)
    colsOfM3 :: Mat 3 t -> (Vec 3 t,Vec 3 t,Vec 3 t)


class Vec2Math t where
    vec2 :: t -> t -> Vec 2 t
    mat2x2 :: Vec 2 t -> Vec 2 t -> Mat 2 t
    unpackV2 :: Vec 2 t -> (t,t)
    rowsOfM2 :: Mat 2 t -> (Vec 2 t,Vec 2 t)
    colsOfM2 :: Mat 2 t -> (Vec 2 t,Vec 2 t)

class VectorMath n t => MatProduct a n t where
    prod :: Mat n t -> a n t -> a n t

type Vec2 = Vec 2
type Vec3 = Vec 3
type Vec4 = Vec 4
type Mat2 = Mat 2
type Mat3 = Mat 3
type Mat4 = Mat 4
