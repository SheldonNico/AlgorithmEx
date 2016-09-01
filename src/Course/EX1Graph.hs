module Course.EX1Graph where

import Data.Array
import Data.List (sort)

-- Algorithm Graphs
type Vertex = Int
type Table a = Array Vertex a
type Graph e = Table [(e, Vertex)]
type Bounds = (Vertex, Vertex)
type Edge e = (Vertex, e, Vertex)
type Labeling a = Vertex -> a
data LabGraph n e = LabGraph (Graph e) (Labeling n)
vertices :: LabGraph n e -> [Vertex]
vertices (LabGraph gr _) = indices gr
labels :: LabGraph n e -> [n]
labels (LabGraph gr lab) = map lab (indices gr)

-- Build Graph
buildG :: Bounds -> [Edge e] -> Graph e
buildG bounds0 edges0 =
  accumArray (flip (:)) [] bounds0 [(v, (l,w)) | (v,l,w) <- edges0]

testGraph = buildG (1,3) [(1, "a", 2), (2, "b", 3), (1, "c", 3)]
-- The graph obtained by reversing all edges
transposeG :: Graph e -> Graph e
transposeG g = buildG (bounds g) (reverseE g)
reverseE :: Graph e -> [Edge e]
reverseE g = [(w, l, v) | (v, l, w) <- edges g]
edges :: Graph e -> [Edge e]
edges g = [(v, l, w) | v <- indices g, (l,w) <- g ! v]

showGraphViz (LabGraph gr lab)  =
    "digraph name {\n" ++ "rankdir=LR;\n" ++ concatMap showNode (indices gr) ++ concatMap showEdge (edges gr) ++"}\n"
    where showEdge (from, t, to) = show from ++ " -> " ++ show to ++ " [label = \"" ++ show t ++ "\"];\n"
          showNode v = show v ++ " [label = " ++ show (lab v) ++ "];\n"
