{-# LANGUAGE DataKinds, StandaloneDeriving #-}
{-# LANGUAGE TypeOperators, FlexibleContexts, UndecidableInstances, FlexibleInstances, MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Geometry.Transform.Quaternion
-- Copyright   :  Copyright (C) 2015 Artem M. Chirkin <chirkin@arch.ethz.ch>
-- License     :  BSD3
--
-- Maintainer  :  Artem M. Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  Experimental
-- Portability :
--
--
-----------------------------------------------------------------------------

module Data.Geometry.Transform.Quaternion
    ( QFTransform (..)
    , QDTransform (..)
    ) where

import Data.Geometry.Quaternion
import Data.Geometry.Types
import Data.Geometry.Instances.Eq ()
import Data.Geometry.Instances.Show ()
import Data.Geometry.Instances.Num ()
import Data.Geometry.VectorMath
import Data.Geometry.Transform.SpaceTransform

-- | SpaceTransform based on quaternion (rotation+scale) and vector (translate)
data QFTransform x = QFTransform QFloat (Vector 3 Float) x deriving (Eq, Show)
-- | SpaceTransform based on quaternion (rotation+scale) and vector (translate)
data QDTransform x = QDTransform QDouble (Vector 3 Double) x deriving (Eq, Show)

instance Functor QFTransform where
    fmap f (QFTransform q v x) = QFTransform q v (f x)

instance Applicative QFTransform where
    pure = QFTransform 1 0
    QFTransform qf vf f <*> QFTransform qx vx x = QFTransform (qf * qx) (rotScale qf vx + vf) (f x)

instance Monad QFTransform where
    return = QFTransform 1 0
    (QFTransform q1 v1 _) >> (QFTransform q2 v2 x) = QFTransform (q1 * q2) (rotScale q1 v2 + v1) x
    (QFTransform q v x) >>= f = QFTransform (q * q') (rotScale q v' + v) y
        where QFTransform q' v' y = f x

instance SpaceTransform QFTransform 3 Float where
    rotate v a = QFTransform (axisRotation v a) 0
    scale c = QFTransform (setQ 0 0 0 c) 0
    translate = QFTransform 1
    unwrap (QFTransform _ _ x) = x
    wrap x (QFTransform a b _) = QFTransform a b x
    liftTransform (QFTransform q v t) = fmap (QFTransform q v) t
    mergeSecond tr (QFTransform q v t) = fmap (\f -> f t) tr
            >>= translate v
            >>= scale c
            >>= rotate axis angle
        where c = square q
              axis = unit $ imVec q
              angle = atan2 (normL2 $ imVec q) (taker q)
    mergeFirst (QFTransform q v f) = (<*>) $ translate v f
            >>= scale c
            >>= rotate axis angle
        where c = square q
              axis = unit $ imVec q
              angle = atan2 (normL2 $ imVec q) (taker q)
    inverseTransform (QFTransform q v x) | q == 0    = QFTransform 0 (-v) x
                                         | otherwise = QFTransform p (- rotScale p v) x
        where p = recip q
    applyV (QFTransform q v x) = rotScale q x + v
    applyVH (QFTransform q v vec) = vector4 x' y' z' w'
        where ((x',y',z'),w') = case unpackV4 vec of
                                 (x,y,z,0) ->
                                    ( unpackV3 $ rotScale q (vector3 x y z)
                                    , 0)
                                 (x,y,z,w) ->
                                    ( unpackV3 $ rotScale q (vector3 (x/w) (y/w) (z/w)) + v
                                    , 1)
    transformM m = QFTransform (js_fromMatrix3x3F m) 0
    transformMH m = QFTransform (js_fromMatrix4x4F m)
        $ case colsOfM4 m of (_,_,_, v) -> resizeVector v

instance Space3DTransform (QFTransform) Float QFloat where
    rotateX a = QFTransform (setQ (sin a) 0 0 (cos a)) 0
    rotateY a = QFTransform (setQ 0 (sin a) 0 (cos a)) 0
    rotateZ a = QFTransform (setQ 0 0 (sin a) (cos a)) 0
    rotateScale q = QFTransform q 0


instance Functor QDTransform where
    fmap f (QDTransform q v x) = QDTransform q v (f x)

instance Applicative QDTransform where
    pure = QDTransform 1 0
    QDTransform qf vf f <*> QDTransform qx vx x = QDTransform (qf * qx) (rotScale qf vx + vf) (f x)

instance Monad QDTransform where
    return = QDTransform 1 0
    (QDTransform q1 v1 _) >> (QDTransform q2 v2 x) = QDTransform (q1 * q2) (rotScale q1 v2 + v1) x
    (QDTransform q v x) >>= f = QDTransform (q * q') (rotScale q v' + v) y
        where QDTransform q' v' y = f x

instance SpaceTransform QDTransform 3 Double where
    rotate v a = QDTransform (axisRotation v a) 0
    scale c = QDTransform (setQ 0 0 0 c) 0
    translate = QDTransform 1
    unwrap (QDTransform _ _ x) = x
    wrap x (QDTransform a b _) = QDTransform a b x
    liftTransform (QDTransform q v t) = fmap (QDTransform q v) t
    mergeSecond tr (QDTransform q v t) = fmap (\f -> f t) tr
            >>= translate v
            >>= scale c
            >>= rotate axis angle
        where c = square q
              axis = unit $ imVec q
              angle = atan2 (normL2 $ imVec q) (taker q)
    mergeFirst (QDTransform q v f) = (<*>) $ translate v f
            >>= scale c
            >>= rotate axis angle
        where c = square q
              axis = unit $ imVec q
              angle = atan2 (normL2 $ imVec q) (taker q)
    inverseTransform (QDTransform q v x) | q == 0    = QDTransform 0 (-v) x
                                         | otherwise = QDTransform p (- rotScale p v) x
        where p = recip q
    applyV (QDTransform q v x) = rotScale q x + v
    applyVH (QDTransform q v vec) = vector4 x' y' z' w'
        where ((x',y',z'),w') = case unpackV4 vec of
                                 (x,y,z,0) ->
                                    ( unpackV3 $ rotScale q (vector3 x y z)
                                    , 0)
                                 (x,y,z,w) ->
                                    ( unpackV3 $ rotScale q (vector3 (x/w) (y/w) (z/w)) + v
                                    , 1)
    transformM m = QDTransform (js_fromMatrix3x3D m) 0
    transformMH m = QDTransform (js_fromMatrix4x4D m)
        $ case colsOfM4 m of (_,_,_, v) -> resizeVector v

instance Space3DTransform (QDTransform) Double QDouble where
    rotateX a = QDTransform (setQ (sin a) 0 0 (cos a)) 0
    rotateY a = QDTransform (setQ 0 (sin a) 0 (cos a)) 0
    rotateZ a = QDTransform (setQ 0 0 (sin a) (cos a)) 0
    rotateScale q = QDTransform q 0



{-# INLINE js_fromMatrix3x3F #-}
foreign import javascript unsafe "fromMatrix3x3($1)"
    js_fromMatrix3x3F :: Matrix 3 Float -> QFloat
{-# INLINE js_fromMatrix4x4F #-}
foreign import javascript unsafe "fromMatrix4x4($1)"
    js_fromMatrix4x4F :: Matrix 4 Float -> QFloat

{-# INLINE js_fromMatrix3x3D #-}
foreign import javascript unsafe "fromMatrix3x3($1)"
    js_fromMatrix3x3D :: Matrix 3 Double -> QDouble
{-# INLINE js_fromMatrix4x4D #-}
foreign import javascript unsafe "fromMatrix4x4($1)"
    js_fromMatrix4x4D :: Matrix 4 Double -> QDouble

