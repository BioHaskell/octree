{-# LANGUAGE FlexibleInstances #-}
module Main(main) where

import Criterion
import Criterion.Main
import Test.QuickCheck(generate, vectorOf, Arbitrary(..))

import Linear.V3
import Data.Octree.Internal

sizes = [100, 1000, 10000]

instance {-# OVERLAPPING #-} Arbitrary (Vector3, Int) where
  arbitrary = ((,) <$> (V3 <$> arbitrary <*> arbitrary <*> arbitrary)
                   <*>         arbitrary)

testData :: Int -> IO [(Vector3, Int)]
testData i = generate $ vectorOf i arbitrary

main = defaultMain [
    bgroup "insert" [
      let i=10^p
      in bench (show i) $ nfIO (foldr insert (Leaf [])
                                 <$> testData i)
      | p <- [1..4]
    ]
  ]
