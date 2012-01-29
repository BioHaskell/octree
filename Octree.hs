{-# LANGUAGE MagicHash, ScopedTypeVariables, RecordWildCards, TemplateHaskell #-}
module Octree(Octree,
              Coord(..),
              dist, 
              fromList, toList,
              lookup,
              insert,
              neighbors,
              nearest,
              withinRange,
              boundingBox)
where

import Prelude hiding(lookup)
import Octree.Internal

