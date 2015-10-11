{-# LANGUAGE DataKinds #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  Copyright (C) 2015 Artem M. Chirkin <chirkin@arch.ethz.ch>
-- License     :  BSD3
--
-- Maintainer  :  Artem M. Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  Experimental
-- Portability :
--
--
-----------------------------------------------------------------------------

module Main
    ( main
    ) where

import Foreign.Marshal
import Foreign.Storable
import Foreign.Ptr

--import GHCJS.Types
--import GHCJS.Prim
--import GHCJS.Marshal
--import JavaScript.Array

--import Unsafe.Coerce
--import Data.Geometry.Prim.JSNum

import Data.Geometry

main :: IO ()
main = do
    print a
    print b
    print $ a .*. b
    print $ dot a b
    putStrLn "hello world!"
--    printRef $ jsref m
    print m
    print $ trace m
    print $ det m
    print $ transpose m
    print $ toDiag b
    print $ fromDiag m
    print l
    print $ a * 5
    print $ abs c
    print $ negate a
    print $ c * a
    print $ inverse m
    print $ inverse m `prod` m
    print $ m `prod` inverse m
    print $ inverse l
    print (inverse eye :: Matrix 3 Float)
    print $ m `prod` diag 3
    print $ a > b
    print $ compare a b
    print $ compare c c
    print $ compare c d
    print $ 2 >= a
    print $ 2 > a
    print $ a < 3
    print $ 2 > a / 1.4
    marr <- mallocArray 5 :: IO (Ptr (Matrix 4 Float))
    poke marr 3
    pokeElemOff marr 1 m
    pokeElemOff marr 4 eye
    pokeElemOff marr 2 pi
    pokeByteOff marr (3*16*4) l
    peek marr >>= print
    peekElemOff marr 1 >>= print
    peekElemOff marr 2 >>= print
    peekElemOff marr 3 >>= print
    peekElemOff marr 4 >>= print
    where a = vector4 2 0 0 2 :: Vector 4 Float
          b = vector4 0 1 0 0 :: Vector 4 Float
          c = vector4 0 0 4 0 :: Vector 4 Float
          d = vector4 0 2 0 1
          m = matrix4x4 a b c d
          l = diag 6 :: Matrix 3 Float
