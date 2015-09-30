{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances, DataKinds #-}
module VectorTests.VectorGenerators where

import Test.Framework

import Control.Monad as M
import Data.Int
import Data.Word
import Foreign.C.Types



import Data.Geometry


-- | C types
instance Arbitrary CFloat where
    arbitrary = M.liftM realToFrac (arbitrary :: Gen Float)
instance Arbitrary CDouble where
    arbitrary = M.liftM realToFrac (arbitrary :: Gen Double)
instance Arbitrary CInt where
    arbitrary = M.liftM CInt (arbitrary :: Gen Int32)
instance Arbitrary CUInt where
    arbitrary = M.liftM CUInt (arbitrary :: Gen Word32)


-- All vector types

instance (Arbitrary t, Vector2Math t) => Arbitrary (Vector 2 t) where
    arbitrary = pure vector2 <*> arbitrary <*> arbitrary
instance (Arbitrary t, Vector2Math t) => Arbitrary (Matrix 2 t) where
    arbitrary = pure matrix2x2 <*> arbitrary <*> arbitrary


instance (Arbitrary t, Vector3Math t) => Arbitrary (Vector 3 t) where
    arbitrary = pure vector3 <*> arbitrary <*> arbitrary <*> arbitrary
instance (Arbitrary t, Vector3Math t) => Arbitrary (Matrix 3 t) where
    arbitrary = pure matrix3x3 <*> arbitrary <*> arbitrary <*> arbitrary


instance (Arbitrary t, Vector4Math t) => Arbitrary (Vector 4 t) where
    arbitrary = pure vector4 <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
instance (Arbitrary t, Vector4Math t) => Arbitrary (Matrix 4 t) where
    arbitrary = pure matrix4x4 <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
