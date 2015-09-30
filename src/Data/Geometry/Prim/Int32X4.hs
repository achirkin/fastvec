{-# LANGUAGE MagicHash, UnboxedTuples #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Geometry.Prim.Int32X4
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

module Data.Geometry.Prim.Int32X4
    ( -- Eq
      eqInt32X4#
      -- Ord
    , ltInt32X4#
    , gtInt32X4#
    , leInt32X4#
    , geInt32X4#
    , maxInt32X4#
    , minInt32X4#
      -- Num
    , absInt32X4#
    , signumInt32X4#
      -- Misc
    , dotInt32X4#
    , transposeMInt32X4#
    , detMInt32X4#
    , prodMMInt32X4#
    , prodMVInt32X4#
    ) where

import GHC.Exts

-- Eq
{-# INLINE eqInt32X4# #-}
foreign import prim "eqInt32X4"
    eqInt32X4# :: Int32X4# -> Int32X4# -> Int#

-- Ord
{-# INLINE ltInt32X4# #-}
foreign import prim "ltInt32X4"
    ltInt32X4# :: Int32X4# -> Int32X4# -> Int#
{-# INLINE gtInt32X4# #-}
foreign import prim "gtInt32X4"
    gtInt32X4# :: Int32X4# -> Int32X4# -> Int#

{-# INLINE leInt32X4# #-}
leInt32X4# :: Int32X4# -> Int32X4# -> Int#
leInt32X4# x y = ltInt32X4# x y `orI#` eqInt32X4# x y

{-# INLINE geInt32X4# #-}
geInt32X4# :: Int32X4# -> Int32X4# -> Int#
geInt32X4# x y = gtInt32X4# x y `orI#` eqInt32X4# x y

{-# INLINE maxInt32X4# #-}
foreign import prim "maxInt32X4"
    maxInt32X4# :: Int32X4# -> Int32X4# -> Int32X4#
{-# INLINE minInt32X4# #-}
foreign import prim "minInt32X4"
    minInt32X4# :: Int32X4# -> Int32X4# -> Int32X4#

-- Num

{-# INLINE absInt32X4# #-}
foreign import prim "absInt32X4"
    absInt32X4# :: Int32X4# -> Int32X4#
{-# INLINE signumInt32X4# #-}
foreign import prim "signumInt32X4"
    signumInt32X4# :: Int32X4# -> Int32X4#

-- Misc


{-# INLINE dotInt32X4# #-}
foreign import prim "dotInt32X4"
    dotInt32X4# :: Int32X4# -> Int32X4# -> Int32X4#

{-# INLINE transposeMInt32X4# #-}
foreign import prim "transposeMInt32X4"
    transposeMInt32X4# :: Int32X4# -> Int32X4# -> Int32X4# -> Int32X4#
                       -> (# Int32X4#, Int32X4#, Int32X4#, Int32X4# #)

{-# INLINE detMInt32X4# #-}
foreign import prim "detMInt32X4"
    detMInt32X4# :: Int32X4# -> Int32X4# -> Int32X4# -> Int32X4# -> Int32X4#

{-# INLINE prodMMInt32X4# #-}
foreign import prim "prodMMInt32X4"
    prodMMInt32X4# :: Int32X4# -> Int32X4# -> Int32X4# -> Int32X4#
                   -> Int32X4# -> Int32X4# -> Int32X4# -> Int32X4#
                   -> (# Int32X4#, Int32X4#, Int32X4#, Int32X4# #)

{-# INLINE prodMVInt32X4# #-}
foreign import prim "prodMVInt32X4"
    prodMVInt32X4# :: Int32X4# -> Int32X4# -> Int32X4# -> Int32X4#
                   -> Int32X4#
                   -> Int32X4#
