{-# LANGUAGE MagicHash, ScopedTypeVariables, RecordWildCards, TemplateHaskell, DeriveTraversable #-}
module Octree(Octree,
              fromList, toList,
              lookup,
              insert,
              neighbors,
              withinRange)
where

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
                Leaf [(Coord, a)]  deriving (Show)

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

octantDistances dp = [(o, octantDistance' dp o) | o <- allOctants]

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
splitBy aPoint [] = ([], [], [], [], [], [], [], [])
splitBy aPoint ((pt@(coord, a)):aList) =
   case i of
     SWD -> (pt:swd,    sed,    nwd,    ned,    swu,    seu,    nwu,    neu)
     SED -> (   swd, pt:sed,    nwd,    ned,    swu,    seu,    nwu,    neu)
     NWD -> (   swd,    sed, pt:nwd,    ned,    swu,    seu,    nwu,    neu)
     NED -> (   swd,    sed,    nwd, pt:ned,    swu,    seu,    nwu,    neu)
     SWU -> (   swd,    sed,    nwd,    ned, pt:swu,    seu,    nwu,    neu)
     SEU -> (   swd,    sed,    nwd,    ned,    swu, pt:seu,    nwu,    neu)
     NWU -> (   swd,    sed,    nwd,    ned,    swu,    seu, pt:nwu,    neu)
     NEU -> (   swd,    sed,    nwd,    ned,    swu,    seu,    nwu, pt:neu)
  where i                                        = cmp aPoint coord 
        (swd, sed, nwd, ned, swu, seu, nwu, neu) = splitBy aPoint aList

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
                            (tswd, tsed, tnwd, tned, tswu, tseu, tnwu, tneu) = tmap fromList $ splitBy splitPoint aList
                        in Node { split = splitPoint,
                                  nwu   = tnwu,
                                  nwd   = tnwd,
                                  neu   = tneu,
                                  ned   = tned,
                                  swu   = tswu,
                                  swd   = tswd,
                                  seu   = tseu,
                                  sed   = tsed }
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
        --selectFrom' best ((c,      d) : cs) | d > dist pt (fst best) = Just best
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
withinRange = undefined

-- QuickCheck
prop_fromToList         l = sort l == (sort . toList . fromList $ l)
prop_insertionPreserved l = sort l == (sort . toList . foldr insert (Leaf []) $ l)
prop_nearest            l pt = nearest pt (fromList l) == naiveNearest l pt
prop_pickClosest        l pt = pickClosest pt l == naiveNearest l pt

compareDistance pt a b = compare (dist pt (fst a)) (dist pt (fst b))

naiveNearest l pt = if byDist == [] then Nothing else Just . head $ byDist
  where byDist = sortBy (compareDistance pt) l

testList = [(Coord {x = -91.04901811601876, y = 11.426013200436955, z = 16.647031370019647},0),(Coord {x = -12.935215796029725, y = 273.06637813843406, z = -55.05942073820639},0),(Coord {x = -24.70608961609365, y = 9.768334667913571, z = -591.735874813091},0),(Coord {x = 26.673862637209673, y = 14.861111850434305, z = -0.5439453260021949},0),(Coord {x = 57.400581024453736, y = -144.91120044321454, z = 24.03491739822036},0),(Coord {x = -11.43201309196567, y = 3.7804836420166694, z = -24.717463780970363},0),(Coord {x = 10.651535005098076, y = 10.6173986252353, z = -15.759345133357492},0),(Coord {x = -77.72525450628606, y = -15.977408392409531, z = -267.34076708407065},0),(Coord {x = -41.756366021666814, y = 17.893349521294244, z = -13.249495155771942},0),(Coord {x = -37.70403327605951, y = -23.28563337728081, z = 10.722245287874406},0),(Coord {x = 42.27238607587282, y = 15.570709306523009, z = -26.807306838698615},0),(Coord {x = -36.19853573880914, y = -553.9078556075585, z = 0.6473478811917106},0),(Coord {x = -21.27163250648984, y = -27.708729303970568, z = -8.556708721822678},0),(Coord {x = 20.15432607521134, y = 1.1542390859566, z = -126.74544260690091},0),(Coord {x = -12.166463863760978, y = 13.126246517336506, z = -61.762520268470375},0),(Coord {x = 1.2054792198915618, y = -82.74805704521371, z = 6.020451056976829},0),(Coord {x = 40.951653683373564, y = 1.2099796994804364, z = -55.73588538475787},0)]
testList2 = map (\(c, _a) -> (c, dist c testPt)) testList
testPt = Coord {x = 4.871607516633762, y = 40.71735564629869, z = 18.77074208349906} 

runTests = $quickCheckAll

-- here testing...
main = do putStrLn "OK!"
          $quickCheckAll
