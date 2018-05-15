octree
======
This is a simple Octree implementation in Haskell.

[![Build Status](https://api.travis-ci.org/BioHaskell/octree.png?branch=master)](https://www.travis-ci.org/BioHaskell/octree)

To use simply:

~~~ {.haskell}
module Main where

import Data.Octree as O

import Linear

main = do let oct = fromList [(V3 1 2 3, "a"),
                              (V3 3 4 5, "b"),
                              (V3 8 8 8, "c")]
              report msg elt = putStrLn $ msg ++ show elt
          report "Nearest     :" $ O.nearest     oct     $ V3 2 2 3
          report "Within range:" $ O.withinRange oct 5.0 $ V3 2 2 3
          return ()
~~~

*For now it uses linear package for vectors, but I may change it to use Tensor package used by OpenGL package, if there is interest.*
*So far I still wait for package with vector operations (like dot, cross producton, vector projection and rejection) on Tensor types.*

Official releases are on [Hackage](http://hackage.haskell.org/package/Octree).

This package is also a part of [Stackage](http://daniel-diaz.github.io/stackagelist/) - a stable subset of Hackage.
