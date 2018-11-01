module Generator.DGraph where

import Data.Graph (Graph)
import qualified Data.Graph as Graph
import Data.Array

type Node = Graph.Vertex

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

topSort = Graph.topSort

-- en lugar de un filter, nos gustaria recorrer la lista y al encontrar un ciclo retornar con error.
cyclicGraph :: Graph -> Bool
cyclicGraph graph = foldr (\ k r  -> isCyclicAssoc k || r) False . assocs $ graph
                    where
                      isCyclicAssoc = uncurry $ reachableFromAny graph 

-- | In the specified graph, can the specified node be reached, starting out
-- from any of the specified vertices?
reachableFromAny :: Graph -> Node -> [Node] -> Bool
reachableFromAny graph node =
  elem node . concatMap (Graph.reachable graph)

-- reachable Graph node retorna los nodos a los que puede se llegar desde node mediante un camino (incluyendo el nodo node)
-- para cada nodo v, se calcula reachable sobre todos sus vecinos, y se concatenan los resultados en una lista. Si v aparece en esa lista, tenemos un ciclo 

(myGraph,vertexToNode,keyToVertex) = Graph.graphFromEdges [
      ("node4",4,[8]),     -- the first component can be of any type
      ("node8",8,[4]),
      ("node7",7,[4]),
      ("node5",5,[7,1]),
      ("node1",1,[4])
   ]


-- primero hay que chequear sintaxis, luego armar el grafo
-- mientras chequeo sintaxis podría ver las llamadas que hace una función para desp ver las aristas
-- ver funciones y parametros en min, Tipos en Mayus, nombre de func que no se repitan, nombre de param que no se repitan ni coincidan con nombre de su func

inff :: [Int] -> Bool
inff = foldr (\ k r  -> (k == 3) || r) False -- para que corte si encuentro un ciclo




