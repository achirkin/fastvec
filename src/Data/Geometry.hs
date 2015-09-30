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


import Data.Geometry.Types                as X ()
import Data.Geometry.Instances.Num        as X ()
import Data.Geometry.Instances.Eq         as X ()
import Data.Geometry.Instances.Ord        as X ()
import Data.Geometry.Instances.Show       as X ()
import Data.Geometry.Instances.Fractional as X ()
import Data.Geometry.Instances.Floating3  as X ()
import Data.Geometry.Instances.Floating4  as X ()
import Data.Geometry.Instances.Storable   as X ()
import Data.Geometry.VectorMath           as X



