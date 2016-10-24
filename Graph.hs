module Graph (
   Graph,
   graph,
   addVertice,
   addVertices,
   addEdge,
   addEdges
    ) where

import Set

type Graph a = (Set a, Set (a, a))

graph::Graph a
graph = (eset, eset)

addVertice::(Eq a, Ord a)=>a->Graph a->Graph a
addVertice x (n, y) = (add x n, y)

addEdge::(Eq a, Ord a)=>(a,a)->Graph a->Graph a
addEdge (x, v) (n, y) = if exists x n && exists v n then (n, add (x,v) y) else (n,y)

addVertices::(Eq a, Ord a)=>[a]->Graph a->Graph a
addVertices [] (x,y) = (x,y)
addVertices (x:xs) (n, y) = addVertices xs (addVertice x (n,y))

addEdges::(Eq a, Ord a)=>[(a,a)]->Graph a->Graph a
addEdges [] (x,y) = (x,y)
addEdges (x:xs) (n, v) = addEdges xs (addEdge x (n,v))

sEntrada2::(Eq a, Ord a)=>Graph a->Set a
sEntrada2 (Set x, Set []) = Set []
sEntrada2 (Set [], Set x) = Set []
sEntrada2 (Set (x:xs), Set (y:ys)) = if exists (snd y) (Set (x:xs)) then add (snd y) (sEntrada2 (Set (x:xs), Set ys)) else sEntrada2 (Set (x:xs), Set ys)

sEntrada::(Eq a, Ord a)=>Graph a->Set a
sEntrada x = dif (fst x) (sEntrada2 x)


