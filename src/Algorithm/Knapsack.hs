module Algorithm.Knapsack where

{-# ANN module "HLint: ignore Avoid restricted function" #-}


{-|
>>> :{
    let w = 9
        weights = [2,1,3,2,1,5]
        values =  [3,2,6,1,3,85]
     in foldr knapsack (replicate (w + 1) 0) (zip weights values)
     :}
[0,3,5,6,9,85,88,90,91,94]
-}
knapsack :: (Int, Int) -> [Int] -> [Int]
knapsack (wi, vi) dp = do
    (j, dpj) <- zip [0..] dp
    return $
        if j >= wi
        then max ((dp !! (j - wi)) + vi) dpj
        else dpj
