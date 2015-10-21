{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Geometry.Instances.Enum
-- Copyright   :  Copyright (C) 2015 Artem M. Chirkin <chirkin@arch.ethz.ch>
-- License     :  BSD3
--
-- Maintainer  :  Artem M. Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  Experimental
-- Portability :
--
--
-----------------------------------------------------------------------------

module Data.Geometry.Instances.Enum () where

import GHC.TypeLits (KnownNat)
import Data.Coerce (coerce)

import Data.Geometry.Types ()
import Data.Geometry.Prim.JSNum
import Data.Geometry.VectorMath
import Data.Geometry.Instances.Num ()
import Data.Geometry.Instances.Ord ()

instance (Num t, Enum t, JSNum t, KnownNat n) => Enum (Vector n t) where
    {-# INLINE succ #-}
    succ v = v + 1
    {-# INLINE pred #-}
    pred v = v - 1
    {-# INLINE toEnum #-}
    toEnum i = v
        where v = coerce . broadcastJSVec (fromNum i) $ dim v
    {-# INLINE fromEnum #-}
    fromEnum = fromEnum . indexVector 0
    {-# INLINE enumFrom #-}
    enumFrom = iterate (+1)
    {-# INLINE enumFromThen #-}
    enumFromThen x y = x : iterate (+d) y
        where d = y-x
    {-# INLINE enumFromTo #-}
    enumFromTo x y = if y >= x then iterateWhile (<= y) succ x else iterateWhile (>= y) pred x
        where iterateWhile c f t = if c t then t : iterateWhile c f (f t) else []
    {-# INLINE enumFromThenTo #-}
    enumFromThenTo x y z | z >= x && z >= y && not (y <= x) = iterateWhile (<= z) x
                         | z <= x && z <= y && not (y >= x) = iterateWhile (>= z) x
                         | otherwise                        = []
        where iterateWhile c t = if c t then t : iterateWhile c (t+d) else []
              d = y-x

instance (Num t, Enum t, JSNum t, KnownNat n) => Enum (Matrix n t) where
    {-# INLINE succ #-}
    succ v = v + 1
    {-# INLINE pred #-}
    pred v = v - 1
    {-# INLINE toEnum #-}
    toEnum i = v
        where v = coerce $ broadcastJSVec (fromNum i) (n*n)
              n = dim v
    {-# INLINE fromEnum #-}
    fromEnum = fromEnum . indexMatrix 0 0
    {-# INLINE enumFrom #-}
    enumFrom = iterate (+1)
    {-# INLINE enumFromThen #-}
    enumFromThen x y = x : iterate (+d) y
        where d = y-x
    {-# INLINE enumFromTo #-}
    enumFromTo x y = if y >= x then iterateWhile (<= y) succ x else iterateWhile (>= y) pred x
        where iterateWhile c f t = if c t then t : iterateWhile c f (f t) else []
    {-# INLINE enumFromThenTo #-}
    enumFromThenTo x y z | z >= x && z >= y && not (y <= x) = iterateWhile (<= z) x
                         | z <= x && z <= y && not (y >= x) = iterateWhile (>= z) x
                         | otherwise                        = []
        where iterateWhile c t = if c t then t : iterateWhile c (t+d) else []
              d = y-x
