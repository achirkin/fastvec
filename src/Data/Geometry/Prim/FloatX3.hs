{-# LANGUAGE MagicHash, UnboxedTuples #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Geometry.Prim.FloatX3
-- Copyright   :  Copyright (C) 2015 Artem M. Chirkin <chirkin@arch.ethz.ch>
-- License     :  BSD3
--
-- Maintainer  :  Artem M. Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  Experimental
-- Portability :
--
-- Internal package containing only vector primops
--
-----------------------------------------------------------------------------

module Data.Geometry.Prim.FloatX3
    ( FloatX3#
      -- GHC.Prim
    , packFloatX3#
    , unpackFloatX3#
    , broadcastFloatX3#
    , plusFloatX3#
    , minusFloatX3#
    , timesFloatX3#
    , divideFloatX3#
    , negateFloatX3#
      -- Eq
    ,  eqFloatX3#
      -- Ord
    , ltFloatX3#
    , gtFloatX3#
    , leFloatX3#
    , geFloatX3#
    , maxFloatX3#
    , minFloatX3#
      -- Num
    , absFloatX3#
    , signumFloatX3#
      -- Fractional
    , recipFloatX3#
    , inverseMFloatX3#
      -- Floating
    , sqrtFloatX3#
      -- Misc
    , dotFloatX3#
    , transposeMFloatX3#
    , detMFloatX3#
    , prodMMFloatX3#
    , prodMVFloatX3#
    ) where

import GHC.Exts

type FloatX3# = FloatX4#

-- GHC.Prim

{-# INLINE packFloatX3# #-}
packFloatX3# :: (#Float#, Float#, Float#, Float# #) -> FloatX3#
packFloatX3# = packFloatX4#

{-# INLINE unpackFloatX3# #-}
unpackFloatX3# :: FloatX3# -> (#Float#, Float#, Float#, Float# #)
unpackFloatX3# = unpackFloatX4#

{-# INLINE broadcastFloatX3# #-}
broadcastFloatX3# :: Float# -> FloatX3#
broadcastFloatX3# = broadcastFloatX4#

{-# INLINE plusFloatX3# #-}
plusFloatX3# :: FloatX3# -> FloatX3# -> FloatX3#
plusFloatX3# = plusFloatX4#

{-# INLINE minusFloatX3# #-}
minusFloatX3# :: FloatX3# -> FloatX3# -> FloatX3#
minusFloatX3# = minusFloatX4#


{-# INLINE timesFloatX3# #-}
timesFloatX3# :: FloatX3# -> FloatX3# -> FloatX3#
timesFloatX3# = timesFloatX4#


{-# INLINE divideFloatX3# #-}
divideFloatX3# :: FloatX3# -> FloatX3# -> FloatX3#
divideFloatX3# = divideFloatX4#


{-# INLINE negateFloatX3# #-}
negateFloatX3# :: FloatX3# -> FloatX3#
negateFloatX3# = negateFloatX4#

-- Eq
{-# INLINE eqFloatX3# #-}
foreign import prim "eqFloatX3"
    eqFloatX3# :: FloatX3# -> FloatX3# -> Int#

-- Ord
{-# INLINE ltFloatX3# #-}
foreign import prim "ltFloatX3"
    ltFloatX3# :: FloatX3# -> FloatX3# -> Int#
{-# INLINE gtFloatX3# #-}
foreign import prim "gtFloatX3"
    gtFloatX3# :: FloatX3# -> FloatX3# -> Int#
{-# INLINE leFloatX3# #-}
foreign import prim "leFloatX3"
    leFloatX3# :: FloatX3# -> FloatX3# -> Int#
{-# INLINE geFloatX3# #-}
foreign import prim "geFloatX3"
    geFloatX3# :: FloatX3# -> FloatX3# -> Int#

{-# INLINE maxFloatX3# #-}
foreign import prim "maxFloatX3"
    maxFloatX3# :: FloatX3# -> FloatX3# -> FloatX3#
{-# INLINE minFloatX3# #-}
foreign import prim "minFloatX3"
    minFloatX3# :: FloatX3# -> FloatX3# -> FloatX3#

-- Num

{-# INLINE absFloatX3# #-}
foreign import prim "absFloatX3"
    absFloatX3# :: FloatX3# -> FloatX3#
{-# INLINE signumFloatX3# #-}
foreign import prim "signumFloatX3"
    signumFloatX3# :: FloatX3# -> FloatX3#

-- Fractional

{-# INLINE recipFloatX3# #-}
foreign import prim "recipFloatX3"
    recipFloatX3# :: FloatX3# -> FloatX3#
{-# INLINE inverseMFloatX3# #-}
foreign import prim "inverseMFloatX3"
    inverseMFloatX3# :: FloatX3# -> FloatX3# -> FloatX3#
                     -> (# FloatX3#, FloatX3#, FloatX3# #)

-- Floating

{-# INLINE sqrtFloatX3# #-}
foreign import prim "sqrtFloatX3"
    sqrtFloatX3# :: FloatX3# -> FloatX3#

-- Misc


{-# INLINE dotFloatX3# #-}
foreign import prim "dotFloatX3"
    dotFloatX3# :: FloatX3# -> FloatX3# -> FloatX3#

{-# INLINE transposeMFloatX3# #-}
foreign import prim "transposeMFloatX3"
    transposeMFloatX3# :: FloatX3# -> FloatX3# -> FloatX3#
                       -> (# FloatX3#, FloatX3#, FloatX3# #)

{-# INLINE detMFloatX3# #-}
foreign import prim "detMFloatX3"
    detMFloatX3# :: FloatX3# -> FloatX3# -> FloatX3# -> FloatX3#

{-# INLINE prodMMFloatX3# #-}
foreign import prim "prodMMFloatX3"
    prodMMFloatX3# :: FloatX3# -> FloatX3# -> FloatX3#
                   -> FloatX3# -> FloatX3# -> FloatX3#
                   -> (# FloatX3#, FloatX3#, FloatX3# #)

{-# INLINE prodMVFloatX3# #-}
foreign import prim "prodMVFloatX3"
    prodMVFloatX3# :: FloatX3# -> FloatX3# -> FloatX3#
                   -> FloatX3#
                   -> FloatX3#

