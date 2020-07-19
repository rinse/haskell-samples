module Algorithm.Dfs where

dfs :: (a -> [a]) -> a -> [a]
dfs f x = x : (f x >>= dfs f)
