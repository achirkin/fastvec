{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

-- import Language.Haskell.TH
--import System.CPUTime
--import System.Environment

--import Data.Geometry

--import qualified Geometry.Space as S


import Test.Framework

import VectorTests.TH
import {-@ HTF_TESTS @-} VectorTests.TestNum

main :: IO ()
main = htfMain $ htf_importedTests ++ $(aggregateTests)



-- htfMain $ makeTestSuite "generated tests" [property prop_negateIntX4]
--    : htf_importedTests


--main :: IO ()
--main = do
--    args <- getArgs
--    let n = case args of
--             amount : _ -> read amount
--             []         -> 100000
--    vectorAddSpeed (n*100)
--    matrixInverseSpeed n
--    testVecOps (vector4 1 (-2) 0 3 :: Vector 4 Int32)
--    testVecOps (vector3 1 (-3) 0 :: Vector 3 Float)
--    print m
--    print v
--    print $ m `prod` v
--    print m1
--    print $ inverse m1
--    print $ m1 `prod` inverse m1
--    print $ det m
--    print $ det m1
--    print $ vector4 1 2 3 4 .*. vector4 0.2 10 100 (1000 :: Float)
--    print $ vector4 1 2 3 4 .*. vector4 1 10 100 (1000 :: Int32)
--    putStrLn "Multiply matrix*vector"
--    print mi4
--    print vi
--    putStrLn $ "result: " ++ show (mi4 `prod` vi)
--    putStrLn $ "det M: " ++ show (det mi4)
----    liftM ((\(o,x) -> (o/10,x/10)) . foldl1 (\(o,x) (oa,xa) -> (oa+o,xa+x)))
----        (forM (replicate 10 (n*100)) vectorIntDotSpeed)
----        >>= \(x,y) -> putStrLn $ "Average times are " ++ show (x,y) ++ " ms. Speedup is " ++ show (x/y)
--    where m = matrix4x4 (vector4 2   0   3   5)
--                        (vector4 0   0.2 0   0)
--                        (vector4 0   0   2   7)
--                        (vector4 1   0.1 0   4) :: Matrix 4 Float
--          v = vector4 1 (-1) 4 (-0.3) :: Vector 4 Float
--          m1 = matrix3x3 (vector3 2 0   1)
--                         (vector3 0 0.5 0)
--                         (vector3 0 1   4) :: Matrix 3 Float
--          mi4 = matrix4x4 (vector4 2   0   3   5)
--                          (vector4 0   1   0   0)
--                          (vector4 0   0   2   7)
--                          (vector4 1  (-1) 0   4) :: Matrix 4 Int32
--          vi = vector4 1 (-1) 4 (-2) :: Vector 4 Int32
----          mi3 = matrix3x3 (vector3 2 0   1)
----                          (vector3 0 (-1) 0)
----                          (vector3 0 1   4) :: Matrix 3 Int32


--vectorIntDotSpeed :: Int -> IO (Double, Double)
--vectorIntDotSpeed n = do
--    t0 <- getCPUTime
--    rn <- t0 `seq` return (rep n ((+ vec3) . (.**. vec2)) vec1)
--    t1 <- rn `seq` getCPUTime
--    ro <- t1 `seq`
--        return (rep n ((+ vec3) . (.*. vec2)) vec1)
--    t2 <- ro `seq` getCPUTime
--    let dto = fromInteger (t2-t1) / 1000000000
--        dtn = fromInteger (t1-t0) / 1000000000
--    putStrLn $ "Dot product of " ++ show n ++ " vectors of 4 int in the new regime "
--        ++ show dtn ++ " ms; "
--        ++ "result is " ++ show rn ++ "."
--    putStrLn $ "Dot product of  " ++ show n ++ " vectors of 4 int in the old regime "
--        ++ show dto ++ " ms; "
--        ++ "result is " ++ show ro ++ "."
--    putStrLn $ "Speedup is " ++ show (fromInteger (t2-t1) / fromInteger (t1-t0) :: Double)
--    return (dto,dtn)
--    where vec1 = vector4 1 2 3 4 :: Vector 4 Int32
--          vec2 = vector4 1 10 100 1000 :: Vector 4 Int32
--          vec3 = vector4 (-4320) (-4319) (-4318) (-4317) :: Vector 4 Int32

--testVecOps :: (Num a, Show a, Ord a) => a -> IO ()
--testVecOps v = do
--    print v
--    putStrLn $ "sum  1 : " ++ show (v + 1)
--    putStrLn $ "mult   : " ++ show (v*v)
--    putStrLn $ "diff 1 : " ++ show (v - 1)
--    putStrLn $ "abs    : " ++ show (abs v)
--    putStrLn $ "negate : " ++ show (negate v)
--    putStrLn $ "signum : " ++ show (signum v)
--    putStrLn $ "signum 0 : " ++ show (signum (v-v))
--    putStrLn $ "signum neg : " ++ show (signum $ negate v)
--    putStrLn $ "lt -3 : " ++ show (v < -3)
--    putStrLn $ "lt 1  : " ++ show (v < 1)
--    putStrLn $ "lt 3  : " ++ show (v < 3)
--    putStrLn $ "lt 5  : " ++ show (v < 5)
--    putStrLn $ "le -3 : " ++ show (v <= -3)
--    putStrLn $ "le 1  : " ++ show (v <= 1)
--    putStrLn $ "le 3  : " ++ show (v <= 3)
--    putStrLn $ "le 5  : " ++ show (v <= 5)
--    putStrLn $ "gt -3 : " ++ show (v > -3)
--    putStrLn $ "gt 1  : " ++ show (v > 1)
--    putStrLn $ "gt 3  : " ++ show (v > 3)
--    putStrLn $ "gt 5  : " ++ show (v > 5)
--    putStrLn $ "ge -3 : " ++ show (v >= -3)
--    putStrLn $ "ge 1  : " ++ show (v >= 1)
--    putStrLn $ "ge 3  : " ++ show (v >= 3)
--    putStrLn $ "ge 5  : " ++ show (v >= 5)
--    putStrLn $ "max 1 : " ++ show (max 1 v)
--    putStrLn $ "min 1 : " ++ show (min 1 v)


--matrixInverseSpeed :: Int -> IO ()
--matrixInverseSpeed n = do
--    t0 <- getCPUTime
--    rn <- t0 `seq` return (rep (n*100) inverse l)
--    t1 <- rn `seq` getCPUTime
--    ro <- l' `seq` t1 `seq` return (rep n S.invert l')
--    t2 <- ro `seq` getCPUTime
--    putStrLn $ "Inverting " ++ show (n*100) ++ " SIMD matrices of 4x4 float took "
--        ++ show (fromInteger (t1-t0) / 1000000000 :: Double) ++ " ms; "
--        ++ "result is " ++ show rn ++ "."
--    putStrLn $ "Inverting " ++ show n ++ " old matrices of 4x4 float took "
--        ++ show (fromInteger (t2-t1) / 1000000000 :: Double) ++ " ms; "
--        ++ "result is " ++ show ro ++ "."
--    putStrLn $ "Speedup is " ++ show (100*fromInteger (t2-t1) / fromInteger (t1-t0) :: Double)
--    where l = matrix4x4 (vector4 2   0   0   5)
--                        (vector4 0   0.2 0   0)
--                        (vector4 0   0   2   0)
--                        (vector4 1   0.1 0   4) :: Matrix 4 Float
--          l' = S.Matrix4x4 2 0   0 1
--                           0 0.2 0 0.1
--                           0 0   2 0
--                           5 0   0 4 :: S.Matrix4x4 Float
--
--vectorAddSpeed :: Int -> IO ()
--vectorAddSpeed n = do
--    t0 <- getCPUTime
--    rv <- t0 `seq` return (rep n (+ vec) vec)
--    t1 <- rv `seq` getCPUTime
--    rn@(r1,r2,r3,r4) <- t1 `seq`
--        return (rep n (+ nu1) nu1, rep n (+ nu2) nu2,rep n (+ nu3) nu3,rep n (+ nu4) nu4)
--    t2 <- r1 `seq` r2 `seq` r3 `seq` r4 `seq` getCPUTime
--    putStrLn $ "Adding " ++ show n ++ " vectors of 4 float took "
--        ++ show (fromInteger (t1-t0) / 1000000000 :: Double) ++ " ms; "
--        ++ "result is " ++ show rv ++ "."
--    putStrLn $ "Adding " ++ show (n*4) ++ " floats took "
--        ++ show (fromInteger (t2-t1) / 1000000000 :: Double) ++ " ms; "
--        ++ "result is " ++ show rn ++ "."
--    putStrLn $ "Speedup is " ++ show (fromInteger (t2-t1) / fromInteger (t1-t0) :: Double)
--    where vec = vector4 1 0.4 (-0.2) 0.000001 :: Vector 4 Float
--          nu1 = 1 :: Float
--          nu2 = 0.4 :: Float
--          nu3 = -0.2 :: Float
--          nu4 = 0.000001 :: Float
--
--rep :: Int -> (a -> a) -> a -> a
--rep 0 _ a = a
--rep n f a = case f a of r -> r `seq` rep (n-1) f r
