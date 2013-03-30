module Data.Octree.Instances where

import Data.Octree.Internal

-- Additional instances for convenience

import Data.Functor
import Data.Foldable hiding(foldr1)
import Data.Traversable

import Data.Monoid

instance Functor Octree where
  fmap f (Leaf l) = Leaf . fmap (\(c, a) -> (c, f a)) $  l
  fmap f (Node { split = sp,
                 nwu   = anwu,
                 nwd   = anwd,
                 neu   = aneu,
                 ned   = aned,
                 swu   = aswu,
                 swd   = aswd,
                 seu   = aseu,
                 sed   = ased }) = Node { split = sp,
                                          nwu   = fmap f anwu,
                                          nwd   = fmap f anwd,
                                          neu   = fmap f aneu,
                                          ned   = fmap f aned,
                                          swu   = fmap f aswu,
                                          swd   = fmap f aswd,
                                          seu   = fmap f aseu,
                                          sed   = fmap f ased }

-- TODO: test Foldable instance
instance Foldable Octree where
  foldMap f (Leaf l)                = foldMap (f . snd) l
  foldMap f n@(Node { split = sp }) = Prelude.foldr1 mappend . map (foldMap f) . subnodes $ n

-- TODO: test Traversable instance
--instance Traversable where

