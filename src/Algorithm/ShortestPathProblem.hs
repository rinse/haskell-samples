{-# LANGUAGE RecordWildCards #-}
module Algorithm.ShortestPathProblem where

import Data.Bits
import Data.Maybe

{-# ANN module "HLint: ignore Avoid restricted function" #-}


inf :: Int
inf = shift 1 29

type Node = Int
type Cost = Int

data Edge = (:<=)
    { input  :: Node
    , output :: Node
    , cost:: Cost
    }

type Graph = ([Node], [Edge])

graph :: Graph
graph = ([0, 1, 2, 3, 4, 5, 6],
    [ (0 :<= 1) 10
    , (0 :<= 2) 12
    , (0 :<= 3) 16
    , (1 :<= 3) 18
    , (1 :<= 4) 4
    , (2 :<= 3) 3
    , (2 :<= 5) 5
    , (3 :<= 5) 1
    , (4 :<= 6) 21
    , (5 :<= 6) 9
    ])

{-| Normal dp. Input must be REVERSED topological-sorted.
>>> :{
    let dp0 = [0, inf, inf, inf, inf, inf, inf]
     in foldr shortestPathProblem dp0 (reverse $ snd graph)
    :}
[0,10,12,15,14,16,25]
-}
shortestPathProblem :: Edge -> [Node] -> [Node]
shortestPathProblem (:<=) {..} dp =
    let smallerCost = min ((dp !! input) + cost) (dp !! output)
     in fromMaybe [] $ replace output smallerCost dp

{-| More dp-ish edition. Input must be REVERSED topological-sorted.
>>> :{
    let dp0 = [0, inf, inf, inf, inf, inf, inf]
     in foldr shortestPathProblem dp0 (reverse $ snd graph)
    :}
[0,10,12,15,14,16,25]
-}
shortestPathProblem' :: Edge -> [Node] -> [Node]
shortestPathProblem' (:<=) {..} dp = do
    (j, dpj) <- zip [0..] dp
    return $
        if j == output
        then min ((dp !! input) + cost) dpj
        else dpj

{-|
>>> replace 0 100 [0, 1, 2, 3]
Just [100,1,2,3]
>>> replace 2 100 [0, 1, 2, 3]
Just [0,1,100,3]
>>> replace 5 100 [0, 1, 2, 3]
Nothing
-}
replace :: Int -> a -> [a] -> Maybe [a]
replace _ _ [] = Nothing
replace 0 a (_:xs) = Just $ a:xs
replace n a (x:xs) = (x :) <$> replace (n - 1) a xs
