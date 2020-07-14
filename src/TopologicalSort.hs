module TopologicalSort where

import           Data.List  ((\\))
import           Data.Maybe (listToMaybe)

type Node = Int

data Edge = (:<=)
    { input  :: Node
    , output :: Node
    }

type Graph = ([Node], [Edge])

headMaybe :: [a] -> Maybe a
headMaybe = listToMaybe

{- |Does topological sort.
    Returns Just only when the given graph is DAG.

>>> let nodes = [1, 5, 3, 4, 2]
>>>     edges = [1 :<= 2 , 1 :<= 3 , 2 :<= 3 , 3 :<= 5 , 4 :<= 5]
>>> topologicalSort (nodes, edges)
Just [1,4,2,3,5]
-}
topologicalSort :: Graph -> Maybe [Node]
topologicalSort ([], _) = Just []
topologicalSort (nodes, edges) = do
    r <- headMaybe $ nodes \\ (output <$> edges)
    let nodes' = filter (/= r) nodes
        edges' = filter ((/= r) . input) edges
    rs <- topologicalSort (nodes', edges')
    return $ r : rs
