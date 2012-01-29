{-# LANGUAGE MagicHash, ScopedTypeVariables, RecordWildCards, TemplateHaskell #-}
module Octree.Internal(Coord(..), dist,
                       Octree(..), lookup, neighbors, nearest, withinRange, fromList, toList, insert,
                       -- internal
                       ODir,
                       octreeStep, octantDistance, splitBy', joinStep, splitStep, allOctants, octantDistance',
                       cmp, origin,
                       unLeaf,
                       pickClosest
                       ) where

import Text.Show
import GHC.Real
import Prelude hiding(lookup)
-- testing
import Data.List(sort, sortBy)
import Data.Maybe(maybeToList)
import Data.Bits((.&.))
--import Data.Traversable
--import Data.Foldable
import Test.QuickCheck.All(quickCheckAll)
import Test.QuickCheck.Arbitrary
-- TODO:
-- {-# LANGUAGE OverloadedStrings #-}
-- import Text.Show.ByteString

data Coord = Coord { x, y, z :: !Double } deriving (Show, Eq, Ord)

instance Arbitrary Coord
  where
    arbitrary = do (a, b, c) <- arbitrary
                   return Coord {x = a, y = b, z = c}


--{-# OPTIONS_GHC -F -pgmFderive -optF-F #-}
--data Coord = Coord { x, y, z :: !Double } deriving (Show, Eq, Ord {-! Arbitrary !-} )
-- Using Neil Mitchell's 'derive'

instance Num Coord
  where
    a + b = Coord { x = x a + x b,
                    y = y a + y b,
                    z = z a + z b }
    a - b = Coord { x = x a - x b,
                    y = y a - y b,
                    z = z a - z b }
    a * b = Coord { x = x a * x b,
                    y = y a * y b,
                    z = z a * z b }
    negate a = Coord { x = negate $ x a,
                       y = negate $ y a,
                       z = negate $ z a }
    fromInteger i = Coord { x = f,
                            y = f,
                            z = f }
      where f = fromInteger i
    abs a = Coord { x = abs $ x a,
                    y = abs $ y a,
                    z = abs $ z a }
    signum = undefined

norm (Coord { x = a, y = b, z = c }) = sqrt (a*a + b*b + c*c)

dist u v = norm (u - v) 

instance Fractional Coord
  where
    a / b = Coord { x = x a / x b,
                    y = y a / y b,
                    z = z a / z b }
    recip a = fromInteger 1 / a
    fromRational (a :% b) = fromInteger a / fromInteger b


data Octree a = Node { split :: Coord,
                       nwu, nwd, neu, ned, swu, swd, seu, sed :: Octree a } |
                Leaf { unLeaf :: [(Coord, a)] }  deriving (Show)

-- TODO: assure that enum numbers are assigned in order
data ODir = SWD | SED | NWD | NED | SWU | SEU | NWU | NEU deriving (Eq, Ord, Enum, Show, Bounded)

cmp :: Coord -> Coord -> ODir
cmp ca cb = joinStep (cx, cy, cz)
  where cx = x ca >= x cb
        cy = y ca >= y cb
        cz = z ca >= z cb

prop_cmp1 a b = cmp a b == joinStep (dx >= 0, dy >= 0, dz >= 0)
  where Coord dx dy dz = a - b

prop_cmp2 a = cmp a origin == joinStep (dx >= 0, dy >= 0, dz >= 0)
  where Coord dx dy dz = a

joinStep (cx, cy, cz) = toEnum (fromEnum cx + 2 * fromEnum cy + 4 * fromEnum cz)

octreeStep NWU = nwu
octreeStep NWD = nwd
octreeStep NEU = neu
octreeStep NED = ned
octreeStep SWU = swu
octreeStep SWD = swd
octreeStep SEU = seu
octreeStep SED = sed

splitStep :: ODir -> (Bool, Bool, Bool)
splitStep step = ((val .&. 1) == 1, (val .&. 2) == 2, (val .&. 4) == 4)
  where val = fromEnum step

prop_stepDescription a b = splitStep (cmp a b) == (x a >= x b, y a >= y b, z a >= z b)

-- here we assume that a, b, c > 0 (otherwise we will take abs, and correspondingly invert results)
-- same octant
-- dp = difference between given point and the center of Octree node
octantDistance' dp NEU = 0.0
-- adjacent by plane
octantDistance' dp NWU = x dp
octantDistance' dp SEU = y dp
octantDistance' dp NED = z dp
-- adjacent by edge
octantDistance' dp SWU = sqrt ( x dp * x dp + y dp * y dp)
octantDistance' dp SED = sqrt ( y dp * y dp + z dp * z dp)
octantDistance' dp NWD = sqrt ( x dp * x dp + z dp * z dp)
-- adjacent by point
octantDistance' dp SWD = norm dp

allOctants :: [ODir]
allOctants = [minBound..maxBound]

xor :: Bool -> Bool -> Bool
xor = (/=)

octantDistance :: Coord -> ODir -> Double
octantDistance dp odir = octantDistance' (abs dp) (toggle dp odir)

toggle :: Coord -> ODir -> ODir
toggle dp odir | (u, v, w) <- splitStep odir = 
  joinStep ((x dp >= 0) `xor` not u,
            (y dp >= 0) `xor` not v,
            (z dp >= 0) `xor` not w)


prop_octantDistanceNoGreaterThanInterpointDistance0 ptA ptB = triangleInequality 
  where triangleInequality = (octantDistance' aptA (cmp ptB origin)) <= (dist aptA ptB)
        aptA               = abs ptA

origin :: Coord
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
splitBy :: Coord -> [(Coord, a)] -> ([(Coord, a)],
                                     [(Coord, a)],
                                     [(Coord, a)],
                                     [(Coord, a)],
                                     [(Coord, a)],
                                     [(Coord, a)],
                                     [(Coord, a)],
                                     [(Coord, a)])
splitBy splitPoint [] = ([], [], [], [], [], [], [], [])
splitBy splitPoint ((pt@(coord, a)):aList) =
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

sumCoords [(coord, _)]       = coord
sumCoords ((coord, _):aList) = coord + sumCoords aList

massCenter aList = sumCoords aList / (fromInteger . toInteger . length $ aList)

-- TODO: should be pre-defined in Data.Tuple or derived?
-- UTILITY
tmap t (a, b, c, d, e, f, g, h) = (t a, t b, t c, t d, t e, t f, t g, t h)

leafLimit :: Int
leafLimit = 16

fromList :: [(Coord, a)] -> Octree a
fromList aList = if length aList <= leafLimit
                   then Leaf aList
                   else let splitPoint :: Coord = massCenter aList
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

toList' (Leaf l     ) tmp = l ++ tmp
toList' (n@Node {..}) tmp = a
  where
    a = toList' nwu b
    b = toList' nwd c
    c = toList' neu d
    d = toList' ned e
    e = toList' swu f
    f = toList' swd g
    g = toList' seu h
    h = toList' sed tmp
toList t = toList' t []

pathTo pt (Leaf _) = []
pathTo pt node     = aStep : pathTo pt (octreeStep aStep node)
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

lookup      = undefined
-- TODO: think about re-balancing
insert (pt, dat) ot = applyByPath insert' path ot
  where path = pathTo pt ot
        insert' (Leaf l) = fromList ((pt, dat) : l)
neighbors   = undefined

candidates' pt (Leaf l) = []
candidates' pt node     = map findCandidates . sortBy compareDistance . octantDistances $ pt - split node
  where
    findCandidates (octant, d) = (octant, d, maybeToList . pickClosest pt . toList . octreeStep octant $ node)
    compareDistance a b  = compare (snd a) (snd b)

-- TODO: nearest
nearest pt (Leaf l) = pickClosest pt l
nearest pt node     = selectFrom candidates
  where candidates                 = map findCandidate . sortBy compareDistance . octantDistances $ pt - split node
        compareDistance a b  = compare (snd a) (snd b)
        findCandidate ( octant, d) = (maybeToList . nearest pt $ octreeStep octant $ node, d)
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
    recurseOctant ( octant, d) = withinRange r pt $ octreeStep octant node

{-
-- QuickCheck
prop_fromToList         l = sort l == (sort . toList . fromList $ l)
prop_insertionPreserved l = sort l == (sort . toList . foldr insert (Leaf []) $ l)
prop_nearest            l pt = nearest pt (fromList l) == naiveNearest pt l
prop_pickClosest        l pt = pickClosest pt l == naiveNearest pt l
prop_naiveWithinRange   r l pt = naiveWithinRange r pt l == (sort . map fst . withinRange r pt . fromList . tuplify pt $ l)

tuplify pt = map (\a -> (a, dist pt a))

compareDistance pt a b = compare (dist pt (fst a)) (dist pt (fst b))

naiveNearest pt l = if byDist == [] then Nothing else Just . head $ byDist
  where byDist = sortBy (compareDistance pt) l

naiveWithinRange r pt l = sort . filter (\p -> dist pt p <= r) $ l


runTests = $quickCheckAll

-- here testing...
main = do putStrLn "OK!"
          $quickCheckAll
-}
