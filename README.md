This is a simple Octree implementation in Haskell.

To use simply:

    import Data.Octree as O

    import Data.Vector.V3

    main = do let oct = fromList [(Vector3 1 2 3, "a"),
                                  (Vector3 3 4 5, "b"),
                                  (Vector3 8 8 8, "c")]
                  report msg elt = putStrLn (msg ++ show elt)
              report "Nearest     :" $ O.nearest     oct     (Vector3 2 2 3)
              report "Within range:" $ O.withinRange oct 5.0 (Vector3 2 2 3)
              return ()

*For now it uses AC-Vector package for vectors, but I may change it to use Tensor package used by OpenGL package, if there is interest.*
*So far I still wait for package with vector operations (like dot, cross producton, vector projection and rejection) on Tensor types.*

