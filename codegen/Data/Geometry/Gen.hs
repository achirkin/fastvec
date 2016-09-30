-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Geometry.Gen
-- Copyright   :  Copyright (C) 2015 Artem M. Chirkin <chirkin@arch.ethz.ch>
-- License     :  BSD3
--
-- Maintainer  :  Artem M. Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  Experimental
--
-- Module exports only one function writeAllFiles, which aggregates code generation
-- from all modules it depends on.
--
-----------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
module Data.Geometry.Gen (writeAllFiles) where


import Llvm

-- Import here all modules containing $(genDefs) for proper code generation
import Data.Geometry.Gen.FloatX4 ()



$(writeAllFilesFun)
