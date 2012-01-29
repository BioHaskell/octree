This is a simple Octree implementation in Haskell.

To use simply:

    import Data.Octree as O

    main = do oct <- fromList [(O.Coord 1 2 3, "a"), (O.Coord 3 4 5, "b")]
	      print ("Nearest" ++ show . snd (O.nearest (Coord 2 2 3)))

*I want to release it on Hackage as soon as I make my mind as of which of numerous 3D vector datatypes to use here.*

