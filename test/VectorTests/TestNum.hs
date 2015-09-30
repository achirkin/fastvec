{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
module VectorTests.TestNum where


import Test.Framework

import Data.Word
import Data.Int
import Data.Geometry
--import Debug.Trace

import VectorTests.VectorGenerators ()
import VectorTests.TH

$(writeTests VecMat [''Num]
    (\t ->
        [d| prop_negate :: $(t) -> Bool
            prop_negate x = negate x == x - 2*x

            prop_abs :: $(t) -> Bool
            prop_abs x = abs x >= 0 && abs x >= x && abs x >= (-x)

            prop_signum :: $(t) -> Bool
            prop_signum x = abs x * signum x == x

            prop_linear :: $(t) -> Word8 -> Bool
            prop_linear x n' = abs ((x * n) - (times (n+1) 0 - x)) < max 1 (abs x)
             where times 0 v = v
                   times m v = v `seq` times (m-1) (v+x)
                   n = fromIntegral n'

            prop_absNorm1 :: $(t) -> Bool
            prop_absNorm1 x = abs (x*0) == 0

            prop_absNorm2 :: $(t) -> $(t) -> Bool
            prop_absNorm2 x y = abs (abs x + abs y)  >= abs (x+y)

            prop_absNorm3 :: $(t) -> Int8 -> Bool
            prop_absNorm3 x n' = abs (n*x) == abs (abs x * abs n)
                where n = fromIntegral n'
        |]
    )
 )



$(writeTests VecMat [''Num]
    (\t ->
        [d| prop_prodEye :: $(t) -> Bool
            prop_prodEye v = eye `prod` v == v

            prop_prodZero :: $(t) -> Bool
            prop_prodZero v = 0 `prod` v == 0

            prop_prodDiag :: $(t) -> Int8 -> Bool
            prop_prodDiag v n' = diag n `prod` v == fromIntegral n' * v
                where n = fromIntegral n'
        |]
    )
 )

