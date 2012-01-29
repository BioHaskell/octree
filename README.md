This is a simple Octree implementation in Haskell.

To use simply:

    import Data.Octree as O

    main = do let oct = fromList [(O.Coord 1 2 3, "a"),
                                  (O.Coord 3 4 5, "b"),
                                  (O.Coord 8 8 8, "c")]
                  report msg elt = putStrLn (msg ++ show elt)
              report "Nearest     :" $ O.nearest         (Coord 2 2 3) oct
              report "Within range:" $ O.withinRange 5.0 (Coord 2 2 3) oct
              return ()

*I want to release it on Hackage as soon as I make my mind as of which of numerous 3D vector datatypes to use here.*

