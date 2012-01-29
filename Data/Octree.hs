{-# LANGUAGE MagicHash, ScopedTypeVariables, RecordWildCards, TemplateHaskell #-}
module Data.Octree(Octree,
                   Coord(..),
                   dist, 
                   fromList, toList,
                   lookup,
                   insert,
                   neighbors,
                   nearest,
                   withinRange)
where

import Prelude hiding(lookup)
import Data.Octree.Internal

