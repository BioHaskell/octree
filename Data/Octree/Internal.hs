{-# LANGUAGE ScopedTypeVariables, RecordWildCards #-}
module Data.Octree.Internal(Vector3(..), dist,
                            Octree(..), lookup, neighbors, nearest, withinRange, fromList, toList, insert,
                            -- internal
                            ODir,
                            octreeStep, octantDistance, splitBy', joinStep, splitStep, allOctants, octantDistance',
                            cmp, origin,
                            pickClosest
                            ) where

import Data.Vector.V3
import Data.Vector.Class

import Text.Show
import Prelude hiding(lookup)
import Data.List(sort, sortBy)
import Data.Maybe(maybeToList, listToMaybe)
import Data.Bits((.&.))
import Test.QuickCheck.All(quickCheckAll)
import Test.QuickCheck.Arbitrary

norm a = a `vdot` a

dist u v = norm (u - v) 

data Octree a = Node { split :: Vector3,
                       nwu, nwd, neu, ned, swu, swd, seu, sed :: Octree a } |
                Leaf { unLeaf :: [(Vector3, a)] }  deriving (Show)

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

-- TODO: assure that enum numbers are assigned in order
data ODir = SWD | SED | NWD | NED | SWU | SEU | NWU | NEU deriving (Eq, Ord, Enum, Show, Bounded)

cmp :: Vector3 -> Vector3 -> ODir
cmp ca cb = joinStep (cx, cy, cz)
  where cx = v3x ca >= v3x cb
        cy = v3y ca >= v3y cb
        cz = v3z ca >= v3z cb

prop_cmp1 a b = cmp a b == joinStep (dx >= 0, dy >= 0, dz >= 0)
  where Vector3 dx dy dz = a - b

prop_cmp2 a = cmp a origin == joinStep (dx >= 0, dy >= 0, dz >= 0)
  where Vector3 dx dy dz = a

joinStep (cx, cy, cz) = toEnum (fromEnum cx + 2 * fromEnum cy + 4 * fromEnum cz)

octreeStep ot NWU = nwu ot
octreeStep ot NWD = nwd ot
octreeStep ot NEU = neu ot
octreeStep ot NED = ned ot 
octreeStep ot SWU = swu ot 
octreeStep ot SWD = swd ot 
octreeStep ot SEU = seu ot 
octreeStep ot SED = sed ot

splitStep :: ODir -> (Bool, Bool, Bool)
splitStep step = ((val .&. 1) == 1, (val .&. 2) == 2, (val .&. 4) == 4)
  where val = fromEnum step

-- | here we assume that a, b, c > 0 (otherwise we will take abs, and correspondingly invert results)
-- same octant
-- dp = difference between given point and the center of Octree node
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

allOctants :: [ODir]
allOctants = [minBound..maxBound]

xor :: Bool -> Bool -> Bool
xor = (/=)

octantDistance :: Vector3 -> ODir -> Double
octantDistance dp odir = octantDistance' (abs dp) (toggle dp odir)

toggle :: Vector3 -> ODir -> ODir
toggle dp odir = 
  joinStep ((v3x dp >= 0) `xor` not u,
            (v3y dp >= 0) `xor` not v,
            (v3z dp >= 0) `xor` not w)
  where (u, v, w) = splitStep odir


prop_octantDistanceNoGreaterThanInterpointDistance0 ptA ptB = triangleInequality 
  where triangleInequality = (octantDistance' aptA (cmp ptB origin)) <= (dist aptA ptB)
        aptA               = abs ptA

origin :: Vector3
origin = fromInteger 0

octantDistances dp = [(o, octantDistance dp o) | o <- allOctants]

prop_octantDistanceNoGreaterThanInterpointDistance ptA ptB vp = triangleInequality || sameOctant
  where triangleInequality = (octantDistance (ptA - vp) (cmp ptB vp)) <= (dist ptA ptB)
        sameOctant         = (cmp ptA vp) == (cmp ptB vp)

prop_octantDistanceNoGreaterThanInterpointDistanceZero ptA ptB = triangleInequality || sameOctant
  where triangleInequality = (octantDistance ptA (cmp ptB origin)) <= (dist ptA ptB)
        sameOctant         = (cmp ptA origin) == (cmp ptB origin)

prop_octantDistanceNoGreaterThanCentroidDistance pt vp = all testFun allOctants
  where testFun odir = (octantDistance (pt - vp) odir) <= dist pt vp


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
splitBy splitPoint  ((pt@(coord, a)):aList) =
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

sumVector3s [(coord, _)]       = coord
sumVector3s ((coord, _):aList) = coord + sumVector3s aList

massCenter aList = sumVector3s aList / (fromInteger . toInteger . length $ aList)

-- TODO: should be pre-defined in Data.Tuple or derived?
-- UTILITY
tmap t (a, b, c, d, e, f, g, h) = (t a, t b, t c, t d, t e, t f, t g, t h)

leafLimit :: Int
leafLimit = 16

fromList :: [(Vector3, a)] -> Octree a
fromList aList = if length aList <= leafLimit
                   then Leaf aList
                   else let splitPoint :: Vector3 = massCenter aList
                        in splitBy' fromList splitPoint aList
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

toList' (Leaf l            ) tmp = l ++ tmp
toList' (Node { nwu   = a,
                nwd   = b,
                neu   = c,
                ned   = d,
                swu   = e,
                swd   = f,
                seu   = g,
                sed   = h }) tmp = foldr toList' tmp [a, b, c, d, e, f, g, h]
toList t = toList' t []

pathTo pt (Leaf _) = []
pathTo pt node     = aStep : pathTo pt (octreeStep node aStep)
  where aStep = cmp pt (split node)

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
insert (pt, dat) ot = applyByPath insert' path ot
  where path             = pathTo pt ot
        insert' (Leaf l) = fromList ((pt, dat) : l)
        insert' _        = error "Impossible in insert'"
neighbors   = undefined


-- | Internal: finds candidates for nearest neighbour lazily for each octant;
-- | they are returned in a list of (octant, min. bound for distance, Maybe candidate) tuples.
candidates' pt (Leaf l) = []
candidates' pt node     = map findCandidates . sortBy compareDistance . octantDistances $ pt - split node
  where
    findCandidates (octant, d) = (octant, d, maybeToList . pickClosest pt . toList . octreeStep node $ octant)
    compareDistance a b  = compare (snd a) (snd b)

-- | Finds a given point, if it is in the tree.
lookup (Leaf l) pt = listToMaybe . filter ((==pt) . fst) $ l
lookup node     pt = flip lookup pt . octreeStep node . cmp pt . split $ node

-- | Finds nearest neighbour for a given point.
nearest pt (Leaf l) = pickClosest pt l
nearest pt node     = selectFrom candidates
  where candidates                 = map findCandidate . sortBy compareDistance . octantDistances $ pt - split node
        compareDistance a b  = compare (snd a) (snd b)
        findCandidate (octant, d) = (maybeToList . nearest pt $ octreeStep node $ octant, d)
        selectFrom (([],     _d) : cs) = selectFrom       cs
        selectFrom (([best], _d) : cs) = selectFrom' best cs
        selectFrom []                  = Nothing

        
        selectFrom' best (([],     d) : cs)                          = selectFrom' best     cs
        -- TODO: FAILS: shortcut guard to avoid recursion over whole structure (since d is bound for distance within octant):
        selectFrom' best ((c,      d) : cs) | d > dist pt (fst best) = Just best
        selectFrom' best (([next], d) : cs)                          = selectFrom' nextBest cs
          where nextBest = if dist pt (fst best) <= dist pt (fst next)
                             then best
                             else next
        selectFrom' best []                                          = Just best

pickClosest pt []     = Nothing
pickClosest pt (a:as) = Just $ foldr (pickCloser pt) a as
pickCloser pt va@(a, _a) vb@(b, _b) = if dist pt a <= dist pt b
                                        then va
                                        else vb

withinRange r pt (Leaf l) = filter (\(lpt, _) -> dist pt lpt <= r) l
withinRange r pt node     = (concat               .             -- merge results
                             map recurseOctant    .             -- recurse over remaining octants
                             filter ((<=r) . snd) .             -- discard octants that are out of range
                             octantDistances $ pt - split node) -- find octant distances
  where
    recurseOctant (octant, _d) = withinRange r pt . octreeStep node $ octant

