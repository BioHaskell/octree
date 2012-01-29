{-# LANGUAGE TemplateHaskell #-}
module TestOctree(main) where

import Octree.Internal
import Prelude hiding(lookup)
import Data.List(sort, sortBy)

import Test.QuickCheck.All(quickCheckAll)
import Test.QuickCheck.Arbitrary

-- | These are tests for internal helper functions:

prop_cmp1 a b = cmp a b == joinStep (dx >= 0, dy >= 0, dz >= 0)
  where Coord dx dy dz = a - b

prop_cmp2 a = cmp a origin == joinStep (dx >= 0, dy >= 0, dz >= 0)
  where Coord dx dy dz = a

prop_stepDescription a b = splitStep (cmp a b) == (x a >= x b, y a >= y b, z a >= z b)

prop_octantDistanceNoGreaterThanInterpointDistance0 ptA ptB = triangleInequality 
  where triangleInequality = (octantDistance' aptA (cmp ptB origin)) <= (dist aptA ptB)
        aptA               = abs ptA

prop_octantDistanceNoGreaterThanInterpointDistance ptA ptB vp = triangleInequality || sameOctant
  where triangleInequality = (octantDistance (ptA - vp) (cmp ptB vp)) <= (dist ptA ptB)
        sameOctant         = (cmp ptA vp) == (cmp ptB vp)

prop_octantDistanceNoGreaterThanInterpointDistanceZero ptA ptB = triangleInequality || sameOctant
  where triangleInequality = (octantDistance ptA (cmp ptB origin)) <= (dist ptA ptB)
        sameOctant         = (cmp ptA origin) == (cmp ptB origin)

prop_octantDistanceNoGreaterThanCentroidDistance pt vp = all testFun allOctants
  where testFun odir = (octantDistance (pt - vp) odir) <= dist pt vp

prop_splitByPrime splitPt pt = (unLeaf . octreeStep ot . cmp pt $ splitPt) == [arg]
  where ot   = splitBy' Leaf splitPt [arg] 
        arg  = (pt, dist pt splitPt)

prop_pickClosest        l pt = pickClosest pt l == naiveNearest pt l

-- | These are tests for exposed functions:

prop_lookup l = all isIn l
  where ot = fromList l
        isIn x = lookup ot (fst x) == Just x

prop_fromToList         l = sort l == (sort . toList . fromList $ l)
prop_insertionPreserved l = sort l == (sort . toList . foldr insert (Leaf []) $ l)
prop_nearest            l pt = nearest pt (fromList l) == naiveNearest pt l
prop_naiveWithinRange   r l pt = naiveWithinRange r pt l == (sort . map fst . withinRange r pt . fromList . tuplify pt $ l)

tuplify pt = map (\a -> (a, dist pt a))

compareDistance pt a b = compare (dist pt (fst a)) (dist pt (fst b))

naiveNearest pt l = if byDist == [] then Nothing else Just . head $ byDist
  where byDist = sortBy (compareDistance pt) l

naiveWithinRange r pt l = sort . filter (\p -> dist pt p <= r) $ l


main = do $quickCheckAll
