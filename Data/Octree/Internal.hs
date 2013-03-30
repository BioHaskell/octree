{-# LANGUAGE ScopedTypeVariables, DisambiguateRecordFields #-}
module Data.Octree.Internal(Vector3(..), dist,
                            Octree(..), lookup, nearest, withinRange, fromList, toList, insert,
                            -- internal
                            ODir(..),
                            octreeStep, octantDistance, splitBy', joinStep, splitStep, allOctants, octantDistance',
                            cmp,
                            pickClosest,
                            depth, size,
                            subnodes
                            ) where

import Data.Vector.V3
import Data.Vector.Class

--import Text.Show
import Prelude hiding(lookup)
import Data.List(sort, sortBy)
import Data.Maybe(maybeToList, listToMaybe)
import Data.Bits((.&.))
import Control.Arrow(second)
import Test.QuickCheck.All(quickCheckAll)
import Test.QuickCheck.Arbitrary

-- | norm of a vector
norm ::  Vector3 -> Double
norm a = sqrt (a `vdot` a)

-- | distance between two vectors
dist ::  Vector3 -> Vector3 -> Double
dist u v = norm (u - v) 

-- | Datatype for nodes within Octree.
data Octree a = Node { split :: Vector3,
                       nwu, nwd, neu, ned, swu, swd, seu, sed :: Octree a } |
                Leaf { unLeaf :: [(Vector3, a)] }  deriving (Show)

-- | Enumerated type to indicate octants in 3D-space relative to given center.
data ODir = SWD | SED | NWD | NED | SWU | SEU | NWU | NEU deriving (Eq, Ord, Enum, Show, Bounded)

-- | Internal method that gives octant of a first vector relative to the second vector as a center.
cmp :: Vector3 -> Vector3 -> ODir
cmp ca cb = joinStep (cx, cy, cz)
  where cx = v3x ca >= v3x cb
        cy = v3y ca >= v3y cb
        cz = v3z ca >= v3z cb

-- | Internal method that joins result of three coordinate comparisons and makes an octant name `ODir`
joinStep :: (Enum a1, Enum a3, Enum a2, Enum a) => (a1, a2, a3) -> a
joinStep (cx, cy, cz) = toEnum (fromEnum cx + 2 * fromEnum cy + 4 * fromEnum cz)

-- | This function converts octant name to a function that steps down in an Octree towards this octant
octreeStep ::  Octree a -> ODir -> Octree a
octreeStep ot NWU = nwu ot
octreeStep ot NWD = nwd ot
octreeStep ot NEU = neu ot
octreeStep ot NED = ned ot 
octreeStep ot SWU = swu ot 
octreeStep ot SWD = swd ot 
octreeStep ot SEU = seu ot 
octreeStep ot SED = sed ot

-- | Function that splits octant name into three boolean values, depending of sign of a relative coordinate in that octant.
-- | (Coordinate is relative to a split point within Octree.)
splitStep :: ODir -> (Bool, Bool, Bool)
splitStep step = ((val .&. 1) == 1, (val .&. 2) == 2, (val .&. 4) == 4)
  where val = fromEnum step

-- | Internal function that finds a lower bound for a distance between a point of relative coordinates,
-- | and octant of given name. It works only when coordinates of a given point are all positive.
-- | It is used only by `octantDistance`, which respectively changes octant name depending of signs of
-- | relative coordinates.
-- here we assume that a, b, c > 0 (otherwise we will take abs, and correspondingly invert results)
-- same octant
-- dp = difference between given point and the center of Octree node
octantDistance' ::  Vector3 -> ODir -> Scalar
octantDistance' dp NEU = 0.0
-- adjacent by plane
octantDistance' dp NWU = v3x dp
octantDistance' dp SEU = v3y dp
octantDistance' dp NED = v3z dp
-- adjacent by edge
octantDistance' dp SWU = sqrt ( v3x dp * v3x dp + v3y dp * v3y dp)
octantDistance' dp SED = sqrt ( v3y dp * v3y dp + v3z dp * v3z dp)
octantDistance' dp NWD = sqrt ( v3x dp * v3x dp + v3z dp * v3z dp)
-- adjacent by point
octantDistance' dp SWD = norm dp

-- | List of all octant names.
allOctants :: [ODir]
allOctants = [minBound..maxBound]

-- | Internal function that makes code clearer.
xor :: Bool -> Bool -> Bool
xor = (/=)

-- | Finds a minimum bounds for a distance between a given point
-- | in relative coordinates and a given octant.
octantDistance :: Vector3 -> ODir -> Double
octantDistance dp odir = octantDistance' (abs dp) (toggle dp odir)

-- | Toggles octant names depending on a signs of vector coordinates
-- | for use in octantDistance.
toggle :: Vector3 -> ODir -> ODir
toggle dp odir = 
  joinStep ((v3x dp >= 0) `xor` not u,
            (v3y dp >= 0) `xor` not v,
            (v3z dp >= 0) `xor` not w)
  where (u, v, w) = splitStep odir

-- | Given a point in relative coordinates, gives list of all octants and minimum distances from this point.
octantDistances ::  Vector3 -> [(ODir, Double)]
octantDistances dp = [(o, octantDistance dp o) | o <- allOctants]

-- | splits a list of vectors and "payload" tuples
-- | into a tuple with elements destined for different octants.
-- FIXME: VERY IMPORTANT - add prop_splitBy vs cmp
splitBy :: Vector3 -> [(Vector3, a)] -> ([(Vector3, a)],
                                         [(Vector3, a)],
                                         [(Vector3, a)],
                                         [(Vector3, a)],
                                         [(Vector3, a)],
                                         [(Vector3, a)],
                                         [(Vector3, a)],
                                         [(Vector3, a)])
splitBy _splitPoint [] = ([], [], [], [], [], [], [], [])
splitBy  splitPoint ((pt@(coord, a)):aList) =
   case i of
     SWD -> (pt:swd,    sed,    nwd,    ned,    swu,    seu,    nwu,    neu)
     SED -> (   swd, pt:sed,    nwd,    ned,    swu,    seu,    nwu,    neu)
     NWD -> (   swd,    sed, pt:nwd,    ned,    swu,    seu,    nwu,    neu)
     NED -> (   swd,    sed,    nwd, pt:ned,    swu,    seu,    nwu,    neu)
     SWU -> (   swd,    sed,    nwd,    ned, pt:swu,    seu,    nwu,    neu)
     SEU -> (   swd,    sed,    nwd,    ned,    swu, pt:seu,    nwu,    neu)
     NWU -> (   swd,    sed,    nwd,    ned,    swu,    seu, pt:nwu,    neu)
     NEU -> (   swd,    sed,    nwd,    ned,    swu,    seu,    nwu, pt:neu)
  where i                                        = cmp coord splitPoint
        (swd, sed, nwd, ned, swu, seu, nwu, neu) = splitBy splitPoint aList

-- | Computes a center of mass for a given list of vectors - used to find a splitPoint.
massCenter ::  Fractional a => [(a, b)] -> a
massCenter aList = sum (map fst aList) / count
  where
    count = fromInteger . toInteger . length $ aList

-- | Helper function to map over an 8-element tuple
tmap :: (t -> t1)-> (t, t, t, t, t, t, t, t)-> (t1, t1, t1, t1, t1, t1, t1, t1)
tmap t (a, b, c, d, e, f, g, h) = (t a, t b, t c, t d, t e, t f, t g, t h)

-- | Maximum number of elements before Octree leaf is split.
leafLimit :: Int
leafLimit = 16

-- | Creates an Octree from a list of (index, payload) tuples.
fromList :: [(Vector3, a)] -> Octree a
fromList aList = if length aList <= leafLimit
                   then Leaf aList
                   else let splitPoint :: Vector3 = massCenter aList
                        in splitBy' fromList splitPoint aList

-- | Internal method, that splits a list into octants depending on coordinates,
-- | and then applies a specified function to each of these sublists,
-- | in order to create subnodes of the Octree
splitBy' :: ([(Vector3, a)] -> Octree a1)-> Vector3-> [(Vector3, a)]-> Octree a1
splitBy' f splitPoint aList = Node { split = splitPoint,
                                     nwu   = tnwu,
                                     nwd   = tnwd,
                                     neu   = tneu,
                                     ned   = tned,
                                     swu   = tswu,
                                     swd   = tswd,
                                     seu   = tseu,
                                     sed   = tsed }
  where
    (tswd, tsed, tnwd, tned, tswu, tseu, tnwu, tneu) = tmap f $ splitBy splitPoint aList
-- TODO: use arrays for memory savings

-- | Internal method that prepends contents of the given subtree to a list
-- | given as argument.
toList' ::  Octree t -> [(Vector3, t)] -> [(Vector3, t)]
toList' (Leaf l            ) tmp = l ++ tmp
toList' (Node { nwu   = a,
                nwd   = b,
                neu   = c,
                ned   = d,
                swu   = e,
                swd   = f,
                seu   = g,
                sed   = h }) tmp = foldr toList' tmp [a, b, c, d, e, f, g, h]
-- | Creates an Octree from list, trying to keep split points near centers
-- | of mass for each subtree.
toList ::  Octree t -> [(Vector3, t)]
toList t = toList' t []

-- | Finds a path to a Leaf where a given point should be,
-- | and returns a list of octant names.
pathTo ::  Vector3 -> Octree a -> [ODir]
pathTo pt (Leaf _) = []
pathTo pt node     = aStep : pathTo pt (octreeStep node aStep)
  where aStep = cmp pt (split node)

-- | Applies a given function to a node specified by a path (list of octant names),
-- | and then returns a modified Octree.
applyByPath :: (Octree a -> Octree a) -> [ODir] -> Octree a -> Octree a
applyByPath f []          ot   = f ot
applyByPath f (step:path) node = case step of
                                   NWU -> node{ nwu = applyByPath f path (nwu node) }
                                   NWD -> node{ nwd = applyByPath f path (nwd node) }
                                   NEU -> node{ neu = applyByPath f path (neu node) }
                                   NED -> node{ ned = applyByPath f path (ned node) }
                                   SWU -> node{ swu = applyByPath f path (swu node) }
                                   SWD -> node{ swd = applyByPath f path (swd node) }
                                   SEU -> node{ seu = applyByPath f path (seu node) }
                                   SED -> node{ sed = applyByPath f path (sed node) }

-- | Inserts a point into an Octree.
-- | NOTE: insert accepts duplicate points, but lookup would not find them - use withinRange in such case.
insert :: (Vector3, a) -> Octree a -> Octree a
insert (pt, dat) ot = applyByPath insert' path ot
  where path             = pathTo pt ot
        insert' (Leaf l) = fromList ((pt, dat) : l)
        insert' _        = error "Impossible in insert'"


-- | Internal: finds candidates for nearest neighbour lazily for each octant;
-- | they are returned in a list of (octant, min. bound for distance, Maybe candidate) tuples.
candidates' :: Vector3 -> Octree a -> [(ODir, Double, [(Vector3, a)])]
candidates' pt (Leaf l) = []
candidates' pt node     = map findCandidates . sortBy compareDistance . octantDistances $ pt - split node
  where
    findCandidates (octant, d) = (octant, d, maybeToList . pickClosest pt . toList . octreeStep node $ octant)
    compareDistance a b  = compare (snd a) (snd b)

-- | Finds a given point, if it is in the tree.
lookup :: Octree a -> Vector3 -> Maybe (Vector3, a)
lookup (Leaf l) pt = listToMaybe . filter ((==pt) . fst) $ l
lookup node     pt = flip lookup pt . octreeStep node . cmp pt . split $ node

-- | Finds nearest neighbour for a given point.
nearest :: Octree a -> Vector3 -> Maybe (Vector3, a)
nearest (Leaf l) pt = pickClosest pt l
nearest node     pt = selectFrom candidates
  where candidates                 = map findCandidate . sortBy compareDistance . octantDistances $ pt - split node
        compareDistance a b  = compare (snd a) (snd b)
        findCandidate (octant, d) = (maybeToList . nearest' . octreeStep node $ octant, d)
        selectFrom (([],     _d) : cs) = selectFrom       cs
        selectFrom (([best], _d) : cs) = selectFrom' best cs
        selectFrom []                  = Nothing
        nearest'   n                   = nearest n pt

        
        selectFrom' best (([],     d) : cs)                          = selectFrom' best     cs
        -- TODO: FAILS: shortcut guard to avoid recursion over whole structure (since d is bound for distance within octant):
        selectFrom' best ((c,      d) : cs) | d > dist pt (fst best) = Just best
        selectFrom' best (([next], d) : cs)                          = selectFrom' nextBest cs
          where nextBest = if dist pt (fst best) <= dist pt (fst next)
                             then best
                             else next
        selectFrom' best []                                          = Just best

-- | Internal method that picks from a given list a point closest to argument, 
pickClosest ::  Vector3 -> [(Vector3, t)] -> Maybe (Vector3, t)
pickClosest pt []     = Nothing
pickClosest pt (a:as) = Just $ foldr (pickCloser pt) a as
pickCloser pt va@(a, _a) vb@(b, _b) = if dist pt a <= dist pt b
                                        then va
                                        else vb

-- | Returns all points within Octree that are within a given distance from argument.
withinRange :: Octree a -> Scalar -> Vector3 -> [(Vector3, a)]
withinRange (Leaf l) r pt = filter (\(lpt, _) -> dist pt lpt <= r) l
withinRange node     r pt = concatMap recurseOctant           . -- recurse over remaining octants, and merge results
                            filter ((<=r) . snd)              . -- discard octants that are out of range
                            octantDistances $ pt - split node   -- find octant distances
  where
    recurseOctant (octant, _d) = (\o -> withinRange o r pt) . octreeStep node $ octant

subnodes :: Octree a -> [Octree a]
subnodes (Leaf _) = []
subnodes node     = map (octreeStep node) allOctants

depth :: Octree a -> Int
depth (Leaf _) = 0
depth node     = foldr max 0 . map (+1) . map depth . subnodes $ node

size :: Octree a -> Int
size =  length . toList

