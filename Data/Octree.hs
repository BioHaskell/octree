module Data.Octree(Octree,
                   Vector3(..),
                   dist, 
                   fromList, toList,
                   lookup,
                   insert,
                   nearest,
                   withinRange)
where

import Prelude hiding(lookup)
import Data.Octree.Internal

