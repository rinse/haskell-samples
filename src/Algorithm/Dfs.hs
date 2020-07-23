module Algorithm.Dfs where

{- $setup
>>> :set -XLambdaCase
-}

{-|Depth-first search
>>> data Tree2 = Leaf Char | Branch Char Tree2 Tree2 deriving Show
>>> :{
    let branch1 = Branch 'B' (Leaf 'D') (Leaf 'E')
        branch2 = Branch 'C' (Leaf 'F') (Leaf 'G')
        tree2 = Branch 'A' branch1 branch2
        f = \case (Branch _ t1 t2) -> [t1, t2]; _ -> []
     in dfs f tree2
:}
[Branch 'A' (Branch 'B' (Leaf 'D') (Leaf 'E')) (Branch 'C' (Leaf 'F') (Leaf 'G')),Branch 'B' (Leaf 'D') (Leaf 'E'),Leaf 'D',Leaf 'E',Branch 'C' (Leaf 'F') (Leaf 'G'),Leaf 'F',Leaf 'G']
-}
dfs :: (a -> [a]) -> a -> [a]
dfs f x = x : (f x >>= dfs f)
