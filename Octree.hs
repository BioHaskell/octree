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
import Data.List(sort)
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
    fromInteger i = Coord { x = fromInteger i,
                            y = fromInteger i,
                            z = fromInteger i }
    abs a = Coord { x = abs $ x a,
                    y = abs $ y a,
                    z = abs $ z a }
    signum = undefined

dist u v = sqrt (a*a + b*b + c*c)
  where Coord { x = a, y = b, z = c } = u - v
        

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

data ODir = NWU | NWD | NEU | NED | SWU | SWD | SEU | SED deriving (Eq, Ord, Enum)

cmp ca cb = toEnum comparisonIndex
  where cx = x ca >= x cb
        cy = y ca >= y cb
        cz = z ca >= z cb
        comparisonIndex = fromEnum cx + 2 * fromEnum cy + 4 * fromEnum cz

octreeStep NWU = nwu
octreeStep NWD = nwd
octreeStep NEU = neu
octreeStep NED = ned
octreeStep SWU = swu
octreeStep SWD = swd
octreeStep SEU = seu
octreeStep SED = sed

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
     NWU -> (pt:nwu,    nwd,    neu,    ned,    swu,    swd,    seu,    sed)
     NWD -> (   nwu, pt:nwd,    neu,    ned,    swu,    swd,    seu,    sed)
     NEU -> (   nwu,    nwd, pt:neu,    ned,    swu,    swd,    seu,    sed)
     NED -> (   nwu,    nwd,    neu, pt:ned,    swu,    swd,    seu,    sed)
     SWU -> (   nwu,    nwd,    neu,    ned, pt:swu,    swd,    seu,    sed)
     SWD -> (   nwu,    nwd,    neu,    ned,    swu, pt:swd,    seu,    sed)
     SEU -> (   nwu,    nwd,    neu,    ned,    swu,    swd, pt:seu,    sed)
     SED -> (   nwu,    nwd,    neu,    ned,    swu,    swd,    seu, pt:sed)
  where i                                        = cmp aPoint coord 
        (nwu, nwd, neu, ned, swu, swd, seu, sed) = splitBy aPoint aList

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
                            (tnwu, tnwd, tneu, tned, tswu, tswd, tseu, tsed) = tmap fromList $ splitBy splitPoint aList
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

applyByPath f []          ot       = f ot
applyByPath f (step:path) node     = case step of
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

-- TODO: nearest
nearest pt (Leaf l) = pickClosest pt l
nearest pt node     = undefined
    
pickClosest pt []     = Nothing
pickClosest pt (a:as) = Just $ foldr (pickCloser pt) a as
pickCloser pt va@(a, _a) vb@(b, _b) = if dist pt a <= dist pt b
                                  then va
                                  else vb
withinRange = undefined

-- QuickCheck
prop_fromToList         l = sort l == (sort . toList . fromList $ l)
prop_insertionPreserved l = sort l == (sort . toList . foldr insert (Leaf []) $ l)

runTests = $quickCheckAll

-- here testing...
main = do putStrLn "OK!"
          $quickCheckAll
