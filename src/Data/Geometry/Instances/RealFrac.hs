{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DataKinds #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Geometry.Instances.RealFrac
-- Copyright   :  Copyright (C) 2015 Artem M. Chirkin <chirkin@arch.ethz.ch>
-- License     :  BSD3
--
-- Maintainer  :  Artem M. Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  Experimental
-- Portability :
--
--
-----------------------------------------------------------------------------

module Data.Geometry.Instances.RealFrac () where

import GHC.TypeLits (KnownNat)
import Data.Coerce (coerce)

import Data.Geometry.Types ()
import Data.Geometry.Prim.JSNum
import Data.Geometry.VectorMath
import Data.Geometry.Instances.Num ()
import Data.Geometry.Instances.Ord ()
import Data.Geometry.Instances.Enum ()
import Data.Geometry.Instances.Real ()
import Data.Geometry.Instances.Integral ()
import Data.Geometry.Instances.Fractional ()

instance (RealFrac t, JSNum t, KnownNat n) => RealFrac (Vector n t) where
    {-# INLINE properFraction #-}
    properFraction x = case properFractionVector x of (a,b) -> (fromIntegral a,b)
    {-# INLINE truncate #-}
    truncate = fromIntegral . truncateVector
    {-# INLINE round #-}
    round = fromIntegral . roundVector
    {-# INLINE ceiling #-}
    ceiling = fromIntegral . ceilingVector
    {-# INLINE floor #-}
    floor = fromIntegral . floorVector

instance (RealFrac t, JSNum t, KnownNat n) => RealFrac (Matrix n t) where
    {-# INLINE properFraction #-}
    properFraction x = case properFractionMatrix x of (a,b) -> (fromIntegral a,b)
    {-# INLINE truncate #-}
    truncate = fromIntegral . truncateMatrix
    {-# INLINE round #-}
    round = fromIntegral . roundMatrix
    {-# INLINE ceiling #-}
    ceiling = fromIntegral . ceilingMatrix
    {-# INLINE floor #-}
    floor = fromIntegral . floorMatrix

{-# INLINE properFractionVector #-}
properFractionVector :: Vector n t -> (Vector n Int,Vector n t)
properFractionVector = coerce . js_properFraction . coerce
{-# INLINE properFractionMatrix #-}
properFractionMatrix :: Matrix n t -> (Matrix n Int,Matrix n t)
properFractionMatrix = coerce . js_properFraction . coerce

{-# INLINE truncateVector #-}
truncateVector :: Vector n t -> Vector n Int
truncateVector = coerce . js_truncate . coerce
{-# INLINE truncateMatrix #-}
truncateMatrix :: Matrix n t -> Matrix n Int
truncateMatrix = coerce . js_truncate . coerce

{-# INLINE roundVector #-}
roundVector :: Vector n t -> Vector n Int
roundVector = coerce . js_round . coerce
{-# INLINE roundMatrix #-}
roundMatrix :: Matrix n t -> Matrix n Int
roundMatrix = coerce . js_round . coerce

{-# INLINE ceilingVector #-}
ceilingVector :: Vector n t -> Vector n Int
ceilingVector = coerce . js_ceiling . coerce
{-# INLINE ceilingMatrix #-}
ceilingMatrix :: Matrix n t -> Matrix n Int
ceilingMatrix = coerce . js_ceiling . coerce

{-# INLINE floorVector #-}
floorVector :: Vector n t -> Vector n Int
floorVector = coerce . js_floor . coerce
{-# INLINE floorMatrix #-}
floorMatrix :: Matrix n t -> Matrix n Int
floorMatrix = coerce . js_floor . coerce
