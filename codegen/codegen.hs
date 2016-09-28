{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash, UnboxedTuples #-}
{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
module Main
    ( main
    ) where


--import GHC.Exts as Exts
import Llvm
--import Llvm.Numbers
import Data.Monoid
import System.FilePath

-- $(runTH "myfun2" ((\(a, b) -> a * signum b + 1000) :: (E (Float32X 4), E (Float32X 4)) -> E (Float32X 4)))

--code2 :: (E (Float32X 4), E (Float32X 4)) -> E (Float32X 4)
--code2 (a, b) = a * signum b + 1000

main :: IO ()
main =
  putStrLn "Hello World!"
--    writeFile (joinPath ["src", "Data", "Geometry", "Prim", "codegen_FloatX4.ll"])
--      $ unlines
--            [ targetDataLayout
--            , targetTriple
--            , ""
--            ]
--      <> sigCode <> absCode
--      <> "\n" <> combineDeps [sigDeps, absDeps]
  where
--    (sigCode, sigDeps) = genCode "signumFloatX4" (signum:: E (Float32X 4)-> E (Float32X 4))
--    (absCode, absDeps) = genCode "absFloatX4"    (abs:: E (Float32X 4)-> E (Float32X 4))
--  main2
--  where
----    x = broadcast (NConst 5) :: Numbers 'NT_FP 32 4
----    x = (NConst 5 :$ NConst 3 :$ NConst 6 :$ NConst 7) :: Numbers 'NT_FP 32 4
----    code
----TIO.writeFile "cmmtry/x64.ll"
----    $  moduleTop
----    <> genCode "plusFloatX4" code
----    <> moduleBottom
----  where
----    code :: (E (Numbers 'NT_INT 32 4), E (Numbers 'NT_INT 32 4)) -> E (Numbers 'NT_INT 32 4)
----    code (a, b) = a * signum b + 1000
--    code :: (E (Numbers 'NT_INT 32 4), E (Numbers 'NT_INT 32 4)) -> E (Numbers 'NT_INT 32 4)
--    code (a, b) = a * signum b + 1000
--    code :: E (Numbers 'NT_FP 32 4) -> E (Numbers 'NT_FP 32 4)
--    code = signum
--    code :: E (Numbers 'NT_INT 32 4) -> E (Numbers 'NT_INT 32 4)
--    code = signum


--foreign import prim "myFunc" myFunc# :: Int32X4# -> Int32X4# -> Int32X4#
--
--
--main2 :: IO ()
--main2 = do
--    print $ I# x1
--    print $ I# x2
--    print $ I# x3
--    print $ I# x4
--    putStrLn "Hello World"
--  where
--    a = packInt32X4# (# 7#, 2#, 3#, -4# #)
--    b = packInt32X4# (# -5#, 2#, -3#, 5# #)
--    r = myFunc# a b
--    (# x1, x2, x3, x4 #) = unpackInt32X4# r

