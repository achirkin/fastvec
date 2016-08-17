{-# LANGUAGE DataKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE JavaScriptFFI, GHCForeignImportPrim #-}
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

--import JsHs.Types
--import GHCJS.Prim
--import GHCJS.Marshal
--import JavaScript.Array

--import Unsafe.Coerce
--import Data.Geometry.Prim.JSNum

#if defined(ghcjs_HOST_OS)
import JsHs.TypedArray
import JsHs.TypedArray.IO
import qualified Control.Monad.ST as ST
import qualified JsHs.TypedArray.ST as ST
import Data.Int

import qualified GHC.Exts as Exts
import Unsafe.Coerce
--import JsHs.JSString
import JsHs.Types
#else
#endif
import Data.Geometry


main :: IO ()
main = do
    print a
    print b
    print $ a .*. b
    print $ dot a b
    putStrLn "hello world!"
--    printRef $ coerce m
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
#if defined(ghcjs_HOST_OS)
    let barr = typedArray 5 :: TypedArray (Vector 6 Float)
        arr = fromList [a,b,c,d]
        sarr = fillNewTypedArray 3 d
        e = vector4 1 (-2.0124) 9.72 0.23
        iv1 = vector3 113 63 (-135) :: Vector3 Int
        iv2 = vector3 7 (-36) 15 :: Vector3 Int
    printAny barr
    printAnyVal sarr
    printAnyVal arr
    print (arr ! 2 == c)
    print (elemSize barr)
    print (elemSize arr)
    print (elemSize sarr)
    printAnyVal (fromArray arr :: TypedArray (Vector 2 Double))
    printAnyVal (fromArray (fromList [1,2,3 :: Float]) :: TypedArray Double)
    print $ (arrayView $ arrayBuffer barr) ! 2 - d
    print $ (arrayView . arrayBuffer $ fillNewTypedArray 8 (4::Float)) ! 1 * d
    barr2 <- thaw barr
    setIndex 2 1 barr2
    setList 1 [0.3,23,12.3262] barr2
    let barr3 = ST.runST $ do
            barr35 <- ST.unsafeThaw barr
            ST.setIndex 0 2.34 barr35
            ST.freeze barr35
    print barr3
    print barr
    print barr2
    putStrLn "Decomposing matrices"
    print m
    print $ colsOfM4 m
    print $ rowsOfM4 m
    putStrLn "Enum tests"
    print ([vector2 0 3, 3.1 .. 9] :: [Vector 2 Double])
    putStrLn "Resizing"
    print (resizeMatrix m :: Matrix 2 Float)
    print (resizeMatrix m :: Matrix 7 Float)
    print (resizeMatrix m :: Matrix 4 Float)
    print (resizeVector e :: Vector 2 Double)
    print (resizeVector e :: Vector 7 Double)
    print (resizeVector e :: Vector 5 Double)
    putStrLn "Real tests"
    print (realToFrac e :: Vector 3 Float)
    print (realToFrac e :: Vector 4 Float)
    print (realToFrac e :: Vector 5 Double)
    print (realToFrac d :: Vector 6 Double)
    print (realToFrac e :: Vector 2 Double)
    putStrLn "Integral tests"
    print (fromIntegral iv1 * e)
    print (fromIntegral iv2 * vector2 1 (2::Double))
    print (iv1 `div` iv2)
    putStrLn "lcm+gcd"
    print (lcm iv1 iv2)
    print (gcd iv1 iv2)
    putStrLn "integral power"
    print (iv2 ^ vector3 0 1 (2::Int))
    putStrLn "RealFrac tests"
    print e
    print (round e :: Vector 2 Int)
    print (floor e :: Vector 3 Int16)
    print (truncate e :: Vector 6 Int)
    print (ceiling e :: Vector 5 Int)
    print (4.3 :: QFloat)
    print (7.6 :: QDouble)
#else
#endif
    where a = vector4 2 0 0 2 :: Vector 4 Float
          b = vector4 0 1 0 0 :: Vector 4 Float
          c = vector4 0 0 4 0 :: Vector 4 Float
          d = vector4 0 2 0 1 :: Vector 4 Float
          m = matrix4x4 a b c d
          l = diag 6 :: Matrix 3 Float



#if defined(ghcjs_HOST_OS)
-- | Printing anything without conversion of types
printAny :: a -> IO ()
printAny = printAny' . unsafeCoerce

foreign import javascript safe "console.log($1)"
    printAny' :: Exts.Any -> IO ()

-- | Printing anything without conversion of types, attempting to get value from the heap object
printAnyVal :: a -> IO ()
printAnyVal = printVal' . unsafeCoerce

foreign import javascript safe "console.log($1)"
    printVal' :: JSVal -> IO ()
#else
#endif
