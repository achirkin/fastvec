{-# LANGUAGE MagicHash, UnboxedTuples #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Geometry.Prim.FloatX4
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

module Data.Geometry.Prim.FloatX4 where
--    ( -- Eq
--      eqFloatX4#
--      -- Ord
--    , ltFloatX4#
--    , gtFloatX4#
--    , leFloatX4#
--    , geFloatX4#
--    , maxFloatX4#
--    , minFloatX4#
--      -- Num
--    , absFloatX4#
--    , signumFloatX4#
--      -- Fractional
--    , recipFloatX4#
--    , inverseMFloatX4#
--      -- Floating
--    , sqrtFloatX4#
--      -- Misc
--    , dotFloatX4#
--    , transposeMFloatX4#
--    , detMFloatX4#
--    , prodMMFloatX4#
--    , prodMVFloatX4#
--    ) where

import GHC.Exts

---- Eq
--{-# INLINE eqFloatX4# #-}
--foreign import prim "eqFloatX4"
--    eqFloatX4# :: FloatX4# -> FloatX4# -> Int#
--
---- Ord
--{-# INLINE ltFloatX4# #-}
--foreign import prim "ltFloatX4"
--    ltFloatX4# :: FloatX4# -> FloatX4# -> Int#
--{-# INLINE gtFloatX4# #-}
--foreign import prim "gtFloatX4"
--    gtFloatX4# :: FloatX4# -> FloatX4# -> Int#
--{-# INLINE leFloatX4# #-}
--foreign import prim "leFloatX4"
--    leFloatX4# :: FloatX4# -> FloatX4# -> Int#
--{-# INLINE geFloatX4# #-}
--foreign import prim "geFloatX4"
--    geFloatX4# :: FloatX4# -> FloatX4# -> Int#
--
--{-# INLINE maxFloatX4# #-}
--foreign import prim "maxFloatX4"
--    maxFloatX4# :: FloatX4# -> FloatX4# -> FloatX4#
--{-# INLINE minFloatX4# #-}
--foreign import prim "minFloatX4"
--    minFloatX4# :: FloatX4# -> FloatX4# -> FloatX4#

-- Num

foreign import prim "absFloatX4" absFloatX4# :: FloatX4# -> FloatX4#
foreign import prim "signumFloatX4" signumFloatX4# :: FloatX4# -> FloatX4#

-- Fractional

--{-# INLINE recipFloatX4# #-}
--foreign import prim "recipFloatX4"
--    recipFloatX4# :: FloatX4# -> FloatX4#
--{-# INLINE inverseMFloatX4# #-}
--foreign import prim "inverseMFloatX4"
--    inverseMFloatX4# :: FloatX4# -> FloatX4# -> FloatX4# -> FloatX4#
--                     -> (# FloatX4#, FloatX4#, FloatX4#, FloatX4# #)
--
---- Floating
--
--{-# INLINE sqrtFloatX4# #-}
--foreign import prim "sqrtFloatX4"
--    sqrtFloatX4# :: FloatX4# -> FloatX4#
--
---- Misc
--
--
--{-# INLINE dotFloatX4# #-}
--foreign import prim "dotFloatX4"
--    dotFloatX4# :: FloatX4# -> FloatX4# -> FloatX4#
--
--{-# INLINE transposeMFloatX4# #-}
--foreign import prim "transposeMFloatX4"
--    transposeMFloatX4# :: FloatX4# -> FloatX4# -> FloatX4# -> FloatX4#
--                       -> (# FloatX4#, FloatX4#, FloatX4#, FloatX4# #)
--
--{-# INLINE detMFloatX4# #-}
--foreign import prim "detMFloatX4"
--    detMFloatX4# :: FloatX4# -> FloatX4# -> FloatX4# -> FloatX4# -> FloatX4#
--
--{-# INLINE prodMMFloatX4# #-}
--foreign import prim "prodMMFloatX4"
--    prodMMFloatX4# :: FloatX4# -> FloatX4# -> FloatX4# -> FloatX4#
--                   -> FloatX4# -> FloatX4# -> FloatX4# -> FloatX4#
--                   -> (# FloatX4#, FloatX4#, FloatX4#, FloatX4# #)
--
--{-# INLINE prodMVFloatX4# #-}
--foreign import prim "prodMVFloatX4"
--    prodMVFloatX4# :: FloatX4# -> FloatX4# -> FloatX4# -> FloatX4#
--                   -> FloatX4#
--                   -> FloatX4#

