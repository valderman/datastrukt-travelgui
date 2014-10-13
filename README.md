This is the Haskell GUI for the travel planner lab of the Chalmers course
DAT037 Data structures.

Installation
------------

Save the files Lab3GUI.hs and Lab3Help.hs into your source code directory.
Then install the threepenny-gui package by executing the following commands
in a terminal. This should work on Mac, Windows and Linux, as long as you have
the Haskell Platform installed.

    cabal update
    cabal install threepenny-gui


Usage
-----

Import Lab3GUI and Lab3Help in your main source file. Create a data type
`Graph` to represent your graph, then write a function to find the shortest
path between two points. Your function should have the type
`Graph -> Name -> Name -> Maybe ([Name], Cost)`, where the `Name`s are strings
representing node names and the `Cost` is an `Int` representing the total
travel time for the path. Of course, you can (and probably should) use as many
helper functions as you like.

Use the provided functions `readStops` and `readLines` to read your graph
files, then build your graph and pass it, along with your stops and lines and
your `shortestPath` function, to `runGUI`.

Feel free to use this skeleton for your code:

    import Lab3Help
    import Lab3GUI
    
    data Graph = ... -- Use a sensible graph representation
    
    shortestPath :: Graph -> Name -> Name -> Maybe ([Name], Cost)
    shortestPath g from to = undefined -- TODO: implement Dijkstra's algorithm
    
    main = do
      Right bstops <- readStops "your-stops.txt"
      Right blines <- readLines "your-lines.txt"
      let graph = undefined -- Build your graph here using bstops and blines
      runGUI bstops blines graph shortestPath
