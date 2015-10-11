{-# LANGUAGE CPP #-}
#if defined(ghcjs_HOST_OS)
module Main where
main :: IO ()
main = putStrLn "Sorry, testing is not supported in GHCJS yet."
#else

{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where

import Test.Framework

import VectorTests.TH
import {-@ HTF_TESTS @-} VectorTests.TestNum

main :: IO ()
main = htfMain $ htf_importedTests ++ $(aggregateTests)

#endif
