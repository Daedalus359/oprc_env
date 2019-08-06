module GraphOPRC where

import Env

--allows for efficiently building a Path as a list from back to front
data PathStep = PathStep Position (Maybe ParentPos)

type ParentPos = Position

--need memoization for efficiency, not an immediate priority
--should probably use Manhattan distance as a heuristic
--use inBoundsNeighborsOf
--open set represents the search frontier
--ties should be broken by searching the most recently discovered node first, so that multiple equally viable paths aren't drawn out in parallel

--f(x) = c(x) + h(x)
----c(x) : the lowest cost path from start to x currently known
------implement as a map with all initial values equal to the number of nodes in the Footprint, as this is an upper bound on path cost
----h(x) : the heuristic estimated cost from x to end

--f(x) values should also be implemented as a map with initialized upper bound values for all nodes

testCompilation = "!"