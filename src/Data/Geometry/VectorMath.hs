{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeFamilies, DataKinds #-}
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
    -- | Vector data types are wrappers for SIMD vectors
    data Vector (n :: Nat) t
    data Matrix (n :: Nat) t
    -- | Matrix with 1 on diagonal and 0 elsewhere
    eye :: Matrix n t
    -- | Put the same value on the matrix diagonal, 0 otherwise
    diag :: t -> Matrix n t
    -- | Transpose matrix
    transpose :: Matrix n t -> Matrix n t
    -- | Determinant of  matrix
    det :: Matrix n t -> t
    -- | Sum of diagonal elements
    trace :: Matrix n t -> t
    -- | Get the diagonal elements from matrix into vector
    fromDiag :: Matrix n t -> Vector n t
    -- | Set vector values into the diagonal elements of matrix
    toDiag :: Vector n t -> Matrix n t
    -- | Scalar product -- sum of vectors' components products, propagated into whole vector
    infixl 7 .*.
    (.*.) :: Vector n t -> Vector n t -> Vector n t
    -- | Scalar product -- sum of vectors' components products -- a scalar
    dot :: Vector n t -> Vector n t -> t

class VectorMath n t => VectorFracMath n t where
    inverse :: Matrix n t -> Matrix n t


class Vector4Math t where
    vector4 :: t -> t -> t -> t -> Vector 4 t
    matrix4x4 :: Vector 4 t -> Vector 4 t -> Vector 4 t -> Vector 4 t -> Matrix 4 t

class Vector3Math t where
    vector3 :: t -> t -> t -> Vector 3 t
    matrix3x3 :: Vector 3 t -> Vector 3 t -> Vector 3 t -> Matrix 3 t

class Vector2Math t where
    vector2 :: t -> t -> Vector 2 t
    matrix2x2 :: Vector 2 t -> Vector 2 t -> Matrix 2 t

class VectorMath n t => MatrixProduct a n t where
    prod :: Matrix n t -> a n t -> a n t

