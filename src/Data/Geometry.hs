{-# LANGUAGE CPP, DataKinds #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Geometry
-- Copyright   :  Copyright (C) 2015 Artem M. Chirkin <chirkin@arch.ethz.ch>
-- License     :  BSD3
--
-- Maintainer  :  Artem M. Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  Experimental
--
--
-----------------------------------------------------------------------------

module Data.Geometry
    ( module X
    ) where


#if defined(ghcjs_HOST_OS)
import Data.Geometry.Instances.FloatingJS as X ()
import Data.Geometry.Instances.TypedArray as X ()
import Data.Geometry.Instances.Enum       as X ()
import Data.Geometry.Instances.Integral   as X ()
import Data.Geometry.Instances.Real       as X ()
import Data.Geometry.Instances.RealFrac   as X ()
import Data.Geometry.Instances.RealFloat  as X ()
import Data.Geometry.Quaternion           as X
import Data.Geometry.Prim.JSNum           as X (JSNum (..))
#else
import Data.Geometry.Instances.Floating3  as X ()
import Data.Geometry.Instances.Floating4  as X ()
#endif
import Data.Geometry.Types                as X
import Data.Geometry.VectorMath           as X
import Data.Geometry.Instances.Eq         as X ()
import Data.Geometry.Instances.Ord        as X ()
import Data.Geometry.Instances.Num        as X ()
import Data.Geometry.Instances.Fractional as X ()
import Data.Geometry.Instances.Show       as X ()
import Data.Geometry.Instances.Storable   as X ()





