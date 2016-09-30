{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
module Data.Geometry.Gen.FloatX4 () where
import Llvm

instance DefineFunction "firstFun" (FloatX4', FloatX4') (FloatX4', FloatX4') where
  code _ (a, b) = (a + b, a - b)

instance DefineFunction "secondFun" (FloatX4', FloatX4') FloatX4' where
  code _ (a, b) = signum a * abs b

instance DefineFunction "secondFun2" (FloatX4', FloatX4') FloatX4' where
  code _ (a, b) = signum a * abs b

$(genDefs)
