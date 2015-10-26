{-# LANGUAGE JavaScriptFFI, GHCForeignImportPrim #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash, UnboxedTuples, PolyKinds #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE DataKinds, StandaloneDeriving #-}
{-# LANGUAGE TypeOperators, FlexibleContexts, UndecidableInstances, FlexibleInstances, MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Geometry.Transform.Matrix4
-- Copyright   :  Copyright (C) 2015 Artem M. Chirkin <chirkin@arch.ethz.ch>
-- License     :  BSD3
--
-- Maintainer  :  Artem M. Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  Experimental
-- Portability :
--
--
-----------------------------------------------------------------------------

module Data.Geometry.Transform.Matrix4
    ( translateM
    , rotateXM, rotateYM, rotateZM, rotateM
    , rotateEulerM
    , lookAtMatrix, perspectiveM, orthogonalM
    , MTransform (..)
    ) where

import GHC.TypeLits

import GHCJS.Types

import Data.Geometry.Quaternion
import Data.Geometry.Types
import Data.Geometry.VectorMath
import Data.Geometry.Prim.JSNum
import Data.Geometry.Transform.SpaceTransform



-- | STransform via standard transformation matrices (homogeneous coordinates)
data MTransform n t x = MTransform (Matrix (n+1) t) x

instance Functor (MTransform n t) where
    fmap f (MTransform m x) = MTransform m (f x)

instance ( Floating t, Eq t
         , KnownNat (n+1)
         , JSNum t
         ) => Applicative (MTransform n t) where
    pure = MTransform eye
    MTransform mf f <*> MTransform mx x = MTransform (mf `prod` mx) (f x)

instance ( Floating t, Eq t
         , KnownNat (n+1)
         , JSNum t
         ) => Monad (MTransform n t) where
    return = MTransform eye
    (MTransform m1 _) >> (MTransform m2 x) = MTransform (m1 `prod` m2) x
    (MTransform m x) >>= f = MTransform (m `prod` m') y
        where MTransform m' y = f x

instance ( Floating t, Eq t
         , JSNum t
         ) => SpaceTransform (MTransform 3 t) 3 t where
    rotate v = MTransform . rotateM v
    scale c = MTransform $ matrix4x4
        (vector4 c 0 0 0)
        (vector4 0 c 0 0)
        (vector4 0 0 c 0)
        (vector4 0 0 0 1)
    translate = MTransform . translateM
    unwrap (MTransform _ v) = v
    wrap x (MTransform m _) = MTransform m x
    liftTransform (MTransform m t) = fmap (MTransform m) t
    mergeSecond tr (MTransform m t) = fmap (\f -> f t) tr >>= transformMH m
    mergeFirst (MTransform m f) = (<*>) $ transformMH m f
    inverseTransform (MTransform m x) = MTransform (inverse m) x
    applyV (MTransform m v) = vector3 (x'/c) (y'/c) (z'/c)
        where (x', y', z', c) = unpackV4 $ m `prod` vector4 x y z 1
              (x,y,z) = unpackV3 v
    applyVH (MTransform m v) = m `prod` v
    transformM m = MTransform $
                   matrix4x4 (resizeVector v1)
                             (resizeVector v2)
                             (resizeVector v3)
                             (vector4 0 0 0 1)
        where (v1,v2,v3) = colsOfM3 m
    transformMH = MTransform

instance Space3DTransform (MTransform 3 Float) Float QFloat where
    rotateX = MTransform . rotateXM
    rotateY = MTransform . rotateYM
    rotateZ = MTransform . rotateZM
    rotateScale = MTransform . js_fromQFloat

instance Space3DTransform (MTransform 3 Double) Double QDouble where
    rotateX = MTransform . rotateXM
    rotateY = MTransform . rotateYM
    rotateZ = MTransform . rotateZM
    rotateScale = MTransform . js_fromQDouble

deriving instance (Eq x, Eq t, Eq (Matrix (n+1) t)) => Eq (MTransform n t x)
deriving instance (Show x, Show t, Show (Matrix (n+1) t)) => Show (MTransform n t x)

-- | translation matrix
{-# INLINE translateM #-}
foreign import javascript unsafe "translateM($1)"
    translateM :: Vector 3 a -> Matrix 4 a

-- | Rotation matrix for a rotation around the X axis
rotateXM :: (JSNum a, Floating a)
         => a -- ^ The angle in radians
         -> Matrix 4 a
rotateXM = rotateXM' . fromNum
foreign import javascript unsafe "rotateXM($1)"
    rotateXM' :: JSVal -> Matrix 4 a
{-# INLINE rotateXM #-}
{-# INLINE rotateXM' #-}

-- | Rotation matrix for a rotation around the Y axis
rotateYM :: (JSNum a, Floating a)
         => a -- ^ The angle in radians
         -> Matrix 4 a
rotateYM = rotateYM' . fromNum
foreign import javascript unsafe "rotateYM($1)"
    rotateYM' :: JSVal -> Matrix 4 a
{-# INLINE rotateYM #-}
{-# INLINE rotateYM' #-}

-- | Rotation matrix for a rotation around the Z axis
rotateZM :: (JSNum a, Floating a)
         => a -- ^ The angle in radians
         -> Matrix 4 a
rotateZM = rotateZM' . fromNum
foreign import javascript unsafe "rotateZM($1)"
    rotateZM' :: JSVal -> Matrix 4 a
{-# INLINE rotateZM #-}
{-# INLINE rotateZM' #-}

-- | Rotation matrix for a rotation around an arbitrary normalized vector

rotateM :: (JSNum a, Floating a)
        => Vector 3 a  -- ^ The normalized vector around which the rotation goes
        -> a -- ^ The angle in radians
        -> Matrix 4 a
rotateM v = rotateM' v . fromNum
foreign import javascript unsafe "rotateM($1,$2)"
    rotateM' :: Vector 3 a -> JSVal -> Matrix 4 a
{-# INLINE rotateM #-}
{-# INLINE rotateM' #-}

-- | Rotation matrix from the Euler angles yaw pitch and roll
rotateEulerM :: (JSNum a, Floating a) => a -> a -> a -> Matrix 4 a
rotateEulerM a b c = rotateEulerM' (fromNum a) (fromNum b) (fromNum c)
foreign import javascript unsafe "rotateEulerM($1,$2,$3)"
    rotateEulerM' :: JSVal -> JSVal -> JSVal -> Matrix 4 a
{-# INLINE rotateEulerM #-}
{-# INLINE rotateEulerM' #-}

-- | 4x4 rotation matrix from a quaternion (Not necessarily normalized; this is also scaling transformation).
--   NB: if the quesaternion is not a unit quaternion, matrix will have scaling |q|^2
foreign import javascript unsafe "fromQuaternion($1)"
    js_fromQFloat :: QFloat  -- ^ Quaternion of r
                  -> Matrix 4 a
{-# INLINE js_fromQFloat #-}

-- | 4x4 rotation matrix from a quaternion (Not necessarily normalized; this is also scaling transformation).
--   NB: if the quesaternion is not a unit quaternion, matrix will have scaling |q|^2
foreign import javascript unsafe "fromQuaternion($1)"
    js_fromQDouble :: QDouble -- ^ Quaternion of r
                   -> Matrix 4 a
{-# INLINE js_fromQDouble #-}


-- | Create a transform matrix so that applying it at camera on @Q 0 0 -1 0@ and @Q 0 0 0 1@  will make it looking at specified direction.
--   Just the same as GluLookAt.
foreign import javascript unsafe "lookAtMatrix($1,$2,$3)"
    lookAtMatrix :: Vector 3 a -- ^ The up direction, not necessary unit length or perpendicular to the view vector
                 -> Vector 3 a -- ^ The viewers position
                 -> Vector 3 a -- ^ The point to look at
                 -> Matrix 4 a
{-# INLINE lookAtMatrix #-}

-- | A perspective symmetric projection matrix. Right-handed coordinate system. (@x@ - right, @y@ - top)
-- | http://en.wikibooks.org/wiki/GLSL_Programming/Vertex_Transformations
perspectiveM :: (JSNum a, Floating a)
            => a -- ^ Near plane clipping distance (always positive)
            -> a -- ^ Far plane clipping distance (always positive)
            -> a -- ^ Field of view of the y axis, in radians
            -> a -- ^ Aspect ratio, i.e. screen's width\/height
            -> Matrix 4 a
perspectiveM a b c d = perspectiveM' (fromNum a) (fromNum b) (fromNum c) (fromNum d)
foreign import javascript unsafe "perspective($1,$2,$3,$4)"
    perspectiveM' :: JSVal -> JSVal -> JSVal -> JSVal -> Matrix 4 a
{-# INLINE perspectiveM #-}
{-# INLINE perspectiveM' #-}

-- | An orthogonal symmetric projection matrix. Right-handed coordinate system. (@x@ - right, @y@ - top)
-- | http://en.wikibooks.org/wiki/GLSL_Programming/Vertex_Transformations
orthogonalM :: (JSNum a, Fractional a)
            => a -- ^ Near plane clipping distance
            -> a -- ^ Far plane clipping distance
            -> a -- ^ width
            -> a -- ^ height
            -> Matrix 4 a
orthogonalM a b c d = orthogonalM' (fromNum a) (fromNum b) (fromNum c) (fromNum d)
foreign import javascript unsafe "orthogonal($1,$2,$3,$4)"
    orthogonalM' :: JSVal -> JSVal -> JSVal -> JSVal -> Matrix 4 a
{-# INLINE orthogonalM #-}
{-# INLINE orthogonalM' #-}
