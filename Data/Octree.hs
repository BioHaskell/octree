module Data.Octree(Octree,
                   Vector3(..),
                   dist, 
                   fromList, toList,
                   lookup,
                   insert,
                   nearest,
                   depth,
                   size,
                   withinRange)
where

import Prelude hiding(lookup)
import Data.Octree.Internal

