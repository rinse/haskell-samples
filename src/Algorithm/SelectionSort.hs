module Algorithm.SelectionSort where

import Data.List (unfoldr)


{- |Do insertion sort on a list
>>> selectionSort []
[]

>>> selectionSort [1]
[1]

>>> selectionSort [4,1,2,5,3,8,6,9,7]
[1,2,3,4,5,6,7,8,9]
-}
selectionSort :: Ord a => [a] -> [a]
selectionSort = unfoldr select

{- |Select a minimum element in a list.
>>> select [5,1,3,2,4]
Just (1,[5,2,3,4])

>>> select []
Nothing

>>> select [1]
Just (1,[])

>>> select [3,2,1]
Just (1,[3,2])
-}
select :: Ord a => [a] -> Maybe (a, [a])
select = foldr select' Nothing

select' :: Ord a => a -> Maybe (a, [a]) -> Maybe (a, [a])
select' a Nothing = Just (a, [])
select' a (Just (x, xs))
    | a < x = Just (a, x:xs)
    | otherwise = Just (x, a:xs)
