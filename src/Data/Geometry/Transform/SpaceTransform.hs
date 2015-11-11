{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, DataKinds #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Geometry.Transform.SpaceTransform
-- Copyright   :  Copyright (C) 2015 Artem M. Chirkin <chirkin@arch.ethz.ch>
-- License     :  BSD3
--
-- Maintainer  :  Artem M. Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  Experimental
--
-- Space transform definition
--
-----------------------------------------------------------------------------

module Data.Geometry.Transform.SpaceTransform where


import GHC.TypeLits

import Data.Geometry.VectorMath
import Data.Geometry.Quaternion





-- | SpaceTransform separates space transformations (such as rotation, scaling, and others) from actual points.
--   This means objects inside SpaceTransform Monad normally stay untouched until transformations are applied.
--   This is useful, for instance, when working with OpenGL: one can submit into GPU transform and coordinates separately.
--   Final behavior is similar to OpenGL's push and pop matrices:
-- > translate (Vector3 1 0 0) x >>= scale 2 >>= rotateX pi
--   The code above means: first translate, then scale, then rotate; if transforms were just matrices, @>>=@ would be matrix multiplication.
--   Important: these transforms are applied inside, not outside - i.e. translate in the example above is outermost matrix.
class Monad s => SpaceTransform s (n :: Nat) t | s -> n, s -> t where
    -- | Create rotation transform
    rotate :: (Eq t, Floating t, Real t) => Vector n t -> t -> x -> s x
    -- | Create transform by uniform scaling
    scale :: (Num t) => t -> x -> s x
    -- | Create transform by translating
    translate :: (Num t) => Vector n t -> x -> s x
    -- | Get bare data without applying transform
    unwrap :: s x -> x
    -- | Wrap data into existing transform discarding transform content
    wrap :: (Num t) => x -> s y -> s x
    -- | Lift transform into Functor's inside
    liftTransform :: (Functor f) => s (f x) -> f (s x)
    -- | Transform another STransform using this one. Multitype analogue of `<*>`
    mergeSecond :: (SpaceTransform z n t) => z (x -> y) -> s x -> z y
    -- | Transform this STransform using another one. Multitype analogue of `<*>`
    mergeFirst :: (SpaceTransform z n t) => s (x -> y) -> z x -> z y
    -- | Inverse of the transform that should satisfy
    -- >>> return x == inverseTransform s >> s
    inverseTransform :: s x -> s x
    -- | Apply transform to nD vector
    applyV :: (Eq t, Floating t) => s (Vector n t) -> Vector n t
    -- | Apply transform to homogeneous point in (n+1)D (corresponding nD Euclidian space)
    applyVH :: (Eq t, Floating t) => s (Vector (n+1) t) -> Vector (n+1) t
    -- | Create transform from transformation matrix
    transformM :: (Eq t, Floating t) => Matrix n t -> x -> s x
    -- | Create transform from transformation matrix -- homogeneous coordinates
    transformMH :: (Eq t, Floating t) => Matrix (n+1) t -> x -> s x

{-# RULES
"mergeSecond/any" mergeSecond = (<*>) :: (SpaceTransform z n t) => z (x -> y) -> z x -> z y
"mergeFirst/any" mergeFirst = (<*>) :: (SpaceTransform z n t) => z (x -> y) -> z x -> z y
    #-}

class ( Monad s
      , SpaceTransform s 3 t
      , Quaternion q t
      , Floating t
      ) => Space3DTransform s t q | s -> t, q -> t, t -> q where
    -- | Create rotation transform by rotating w.r.t. X axis
    rotateX :: t -> x -> s x
    -- | Create rotation transform by rotating w.r.t. Y axis
    rotateY :: t -> x -> s x
    -- | Create rotation transform by rotating w.r.t. Z axis
    rotateZ :: t -> x -> s x
    -- | Create transform from quaternion
    rotateScale :: q -> x -> s x


-- | Kind of object that can be transformed
class Transformable x n t | x -> t where
    -- | Apply wrapping transform on the object inside
    transform :: (SpaceTransform s n t, Floating t, Eq t) => s x -> x

instance Transformable (Vector 2 t) 2 t where
    transform = applyV
instance Transformable (Vector 3 t) 3 t where
    transform = applyV
instance Transformable (Vector 4 t) 4 t where
    transform = applyV
instance Transformable (Vector 5 t) 5 t where
    transform = applyV
instance Transformable (Vector 6 t) 6 t where
    transform = applyV
instance Transformable (Vector 7 t) 7 t where
    transform = applyV
instance Transformable (Vector 8 t) 8 t where
    transform = applyV

instance Transformable (Vector 3 t) 2 t where
    transform = applyVH
instance Transformable (Vector 4 t) 3 t where
    transform = applyVH
instance Transformable (Vector 5 t) 4 t where
    transform = applyVH
instance Transformable (Vector 6 t) 5 t where
    transform = applyVH
instance Transformable (Vector 7 t) 6 t where
    transform = applyVH
instance Transformable (Vector 8 t) 7 t where
    transform = applyVH


--instance ( Functor f
--         , Transformable b n t
--         ) => Transformable (f b) n t where
--    transform = fmap transform . liftTransform

-- | Apply transform on each point within Functor
ftransform :: ( SpaceTransform s n t
              , Functor f
              , Transformable b n t
              , Floating t, Eq t)
           => s (f b) -> f b
ftransform = fmap transform . liftTransform

--    -- | return the overall rotation and scale
--    getRotationScale :: s x -> Quaternion t
--    -- | return the overall translation of the transform
--    getTranslation :: s x -> Vector 3 t

