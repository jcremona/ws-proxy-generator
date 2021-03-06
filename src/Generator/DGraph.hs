module Generator.DGraph (cyclicGraph, makeGraph, Generator.DGraph.topSort) where

import Data.Graph (Graph, reachable, topSort, Vertex)
import Data.Array

type Node = Vertex

-----------------------------------------------------------------------------
-- Una implementación de grafos que utiliza Data.Graph. 
-- Exporta una función que permite realizar un ordenamiento topológico.
-- https://stackoverflow.com/a/8936096
-----------------------------------------------------------------------------


makeGraph :: [(Node, [Node])] -> Graph
makeGraph list =
  array (minimum nodes, maximum nodes) list
  where
    nodes = map fst list

-- | Calculates all the nodes that are part of cycles in a graph.
cyclicNodes :: Graph -> [Node]
cyclicNodes graph =
  map fst . filter isCyclicAssoc . assocs $ graph
  where
    isCyclicAssoc = uncurry $ reachableFromAny graph

topSort = Data.Graph.topSort

cyclicGraph :: Graph -> Bool
cyclicGraph graph = foldr (\ k r  -> isCyclicAssoc k || r) False . assocs $ graph
                    where
                      isCyclicAssoc = uncurry $ reachableFromAny graph 

-- | In the specified graph, can the specified node be reached, starting out
-- from any of the specified vertices?
reachableFromAny :: Graph -> Node -> [Node] -> Bool
reachableFromAny graph node =
  elem node . concatMap (reachable graph)

-- reachable Graph node retorna los nodos a los que puede se llegar desde node mediante un camino (incluyendo el nodo node)
-- para cada nodo v, se calcula reachable sobre todos sus vecinos, y se concatenan los resultados en una lista. Si v aparece en esa lista, tenemos un ciclo 

