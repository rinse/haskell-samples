module Sudoku where

import           Control.Monad (guard)
import           Data.Foldable (fold)
import           Data.Function (on)
import           Data.List     (minimumBy)
import           Data.Maybe    (catMaybes, maybeToList)
import           Dfs           (dfs)


type Element = Int
type Table = [[Element]]

{- | Solves sudoku.
>>> :{
solveSudoku [ [8,0,0,0,0,0,0,0,0]
            , [0,0,3,6,0,0,0,0,0]
            , [0,7,0,0,9,0,2,0,0]
            , [0,5,0,0,0,7,0,0,0]
            , [0,0,0,0,4,5,7,0,0]
            , [0,0,0,1,0,0,0,3,0]
            , [0,0,1,0,0,0,0,6,8]
            , [0,0,8,5,0,0,0,1,0]
            , [0,9,0,0,0,0,4,0,0]
            ]
:}
[[[8,1,2,7,5,3,6,4,9],[9,4,3,6,8,2,1,7,5],[6,7,5,4,9,1,2,8,3],[1,5,4,2,3,7,8,9,6],[3,6,9,8,4,5,7,2,1],[2,8,7,1,6,9,5,3,4],[5,2,1,9,7,4,3,6,8],[4,3,8,5,2,6,9,1,7],[7,9,6,3,1,8,4,5,2]]]
-}
solveSudoku :: Table -> [Table]
solveSudoku = filter isCompleted . dfs tentatives

isCompleted :: Table -> Bool
isCompleted = all $ notElem 0

tentatives :: Table -> [Table]
tentatives table = catMaybes $ do
    ((x, y), cands) <- maybeToList $ minimumCandidates table
    cand <- cands
    return $ replaceWith2' x y cand table

minimumCandidates :: Table -> Maybe ((Int, Int), [Element])
minimumCandidates table = do
    let getPosWithCands t p@(x, y) = (p, candidates x y t)
        posWithCands = getPosWithCands table <$> posBlank table
        comp = compare `on` length . snd
    guard $ (not . null) posWithCands
    return $ minimumBy comp posWithCands

-- safe !!
{-# ANN (!?) "HLint: ignore Avoid restricted function" #-}
(!?) :: [a] -> Int -> Maybe a
xs !? n
    | n < length xs = Just $ xs !! n
    | otherwise = Nothing

-- lists candidates of a coordinate.
candidates :: Int -> Int -> Table -> [Element]
candidates x y table = do
    existing <- maybeToList $ fold <$> sequenceA [row y table, col x table, box x y table]
    filter (`notElem` existing) [1 .. 9]

-- the nth row of a table.
row :: Int -> [[a]] -> Maybe [a]
row y = (!? y)

-- the nth column of a table.
col :: Int -> [[a]] -> Maybe [a]
col x = traverse (!? x)

-- the nth box of a table.
box :: Int -> Int -> [[a]] -> Maybe [a]
box x y t =
    let ret = f y t >>= f x
     in ret <$ guard (length ret == 9)
    where
    f n = take 3 . drop (n `div` 3 * 3)

-- lists blanks of a table.
posBlank :: Table -> [(Int, Int)]
posBlank t = do
    (y, r) <- zip [0..] t
    (x, c) <- zip [0..] r
    guard $ 0 `elem` r && c == 0
    return (x, y)

replaceWith2' :: Int -> Int -> a -> [[a]] -> Maybe [[a]]
replaceWith2' x y a t = row y t >>= replaceWith' x a >>= flip (replaceWith' y) t

safeTail :: [a] -> Maybe [a]
safeTail []     = Nothing
safeTail (_:xs) = Just xs

-- |replaces an nth element with a value.
-- >>> replaceWith' 1 'a' ['h', 'e', 'l', 'l', 'o']
-- Just "hallo"
-- >>> replaceWith' 6 'a' ['h', 'e', 'l', 'l', 'o']
-- Nothing
-- >>> replaceWith' (-1) 'a' ['h', 'e', 'l', 'l', 'o']
-- Nothing
replaceWith' :: Int -> a -> [a] -> Maybe [a]
replaceWith' n a l = do
    guard $ 0 <= n
    let (h, t) = splitAt n l
    t' <- safeTail t
    return $ putInBetween a h t'

-- |puts an element in between two lists and concats them.
-- prop> putInBetween x l1 l2 == fold [l1, [x], l2]
putInBetween :: a -> [a] -> [a] -> [a]
putInBetween a h t = h <> (a : t)