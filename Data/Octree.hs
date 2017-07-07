module Data.Octree(Octree,
                   Vector3(..),
                   dist,
                   fromList, toList,
                   lookup,
                   insert,
                   delete,
                   deleteBy,
                   nearest,
                   depth,
                   size,
                   withinRange)
where

import Prelude hiding(lookup)
import Data.Octree.Internal

