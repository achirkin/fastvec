{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, DataKinds, KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
#if defined(ghcjs_HOST_OS)
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}
#else
{-# LANGUAGE TypeFamilies #-}
#endif

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

module Data.Geometry.VectorMath
    ( VectorMath (..)
    , Vector2Math (..)
    , Vector3Math (..)
    , Vector4Math (..)
    , MatrixProduct (..)
    , VectorFracMath (..)
#if defined(ghcjs_HOST_OS)
    , Vector (..), Matrix (..)
    , Dimensional (..)
#endif
    , Vector2, Vector3, Vector4
    , Matrix2, Matrix3, Matrix4
    ) where

import GHC.TypeLits

#if defined(ghcjs_HOST_OS)

import Data.Typeable
import JsHs.Types
import JsHs.LikeJS.Class
-- import GHCJS.Marshal.Pure

newtype Vector (n :: Nat) t = JSVector JSVal deriving Typeable
instance IsJSVal (Vector n t)
newtype Matrix (n :: Nat) t = JSMatrix JSVal deriving Typeable
instance IsJSVal (Matrix n t)

instance LikeJS "Array" (Vector n t) where
    asJSVal (JSVector v) = v
    asLikeJS = JSVector

instance LikeJS "Array" (Matrix n t) where
    asJSVal (JSMatrix v) = v
    asLikeJS = JSMatrix

class Dimensional a where
    dim :: a -> Int

instance KnownNat n => Dimensional (Vector n a) where
    {-# INLINE dim #-}
    dim _ = fromInteger $ natVal (Proxy :: Proxy n)


instance KnownNat n => Dimensional (Matrix n a) where
    {-# INLINE dim #-}
    dim _ = fromInteger $ natVal (Proxy :: Proxy n)

#else
#endif

class VectorMath (n :: Nat) t where
#if defined(ghcjs_HOST_OS)
#else
    data Vector (n :: Nat) t
    data Matrix (n :: Nat) t
#endif
    -- | Fill vector with the same value
    broadcastVector :: t -> Vector n t
    -- | Fill matrix with single value
    broadcastMatrix :: t -> Matrix n t
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
    -- | Get element by its index
    indexVector :: Int -> Vector n t -> t
    -- | Get element by its index
    indexMatrix :: Int -> Int -> Matrix n t -> t
    -- | Sum of absolute values
    normL1 :: Vector n t -> t
    -- | hypot function (square root of squares)
    normL2 :: Vector n t -> t
    -- | Maximum of absolute values
    normLPInf :: Vector n t -> t
    -- | Minimum of absolute values
    normLNInf :: Vector n t -> t
    -- | Norm in Lp space
    normLP :: Int -> Vector n t -> t

{-# RULES
"normLP/L1" normLP 1 = normL1
"normLP/L2" normLP 2 = normL2
    #-}

class VectorMath n t => VectorFracMath n t where
    inverse :: Matrix n t -> Matrix n t


class Vector4Math t where
    vector4 :: t -> t -> t -> t -> Vector 4 t
    matrix4x4 :: Vector 4 t -> Vector 4 t -> Vector 4 t -> Vector 4 t -> Matrix 4 t
    unpackV4 :: Vector 4 t -> (t,t,t,t)
    rowsOfM4 :: Matrix 4 t -> (Vector 4 t,Vector 4 t,Vector 4 t,Vector 4 t)
    colsOfM4 :: Matrix 4 t -> (Vector 4 t,Vector 4 t,Vector 4 t,Vector 4 t)

class Vector3Math t where
    vector3 :: t -> t -> t -> Vector 3 t
    matrix3x3 :: Vector 3 t -> Vector 3 t -> Vector 3 t -> Matrix 3 t
    unpackV3 :: Vector 3 t -> (t,t,t)
    rowsOfM3 :: Matrix 3 t -> (Vector 3 t,Vector 3 t,Vector 3 t)
    colsOfM3 :: Matrix 3 t -> (Vector 3 t,Vector 3 t,Vector 3 t)


class Vector2Math t where
    vector2 :: t -> t -> Vector 2 t
    matrix2x2 :: Vector 2 t -> Vector 2 t -> Matrix 2 t
    unpackV2 :: Vector 2 t -> (t,t)
    rowsOfM2 :: Matrix 2 t -> (Vector 2 t,Vector 2 t)
    colsOfM2 :: Matrix 2 t -> (Vector 2 t,Vector 2 t)

class VectorMath n t => MatrixProduct a n t where
    prod :: Matrix n t -> a n t -> a n t

type Vector2 = Vector 2
type Vector3 = Vector 3
type Vector4 = Vector 4
type Matrix2 = Matrix 2
type Matrix3 = Matrix 3
type Matrix4 = Matrix 4
