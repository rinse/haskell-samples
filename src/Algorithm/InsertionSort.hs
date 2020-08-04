module Algorithm.InsertionSort where


{- |Do insertion sort on a list
>>> insertionSort []
[]

>>> insertionSort [1]
[1]

>>> insertionSort [4,1,2,5,3,8,6,9,7]
[1,2,3,4,5,6,7,8,9]
-}
insertionSort :: Ord a => [a] -> [a]
insertionSort = foldr insert []

{- |Inserts a value into a sorted list.
>>> insert 2 [1,3,4]
[1,2,3,4]

>>> insert 1 []
[1]
-}
insert :: Ord a => a -> [a] -> [a]
insert a s =
    let (h, t) = binarySearch a s
     in h <> (a : t)

{- |Longest prefix of xs which is smaller than a.
    The given sorted list should be finite.

>>> binarySearch 1 ([])
([],[])

>>> binarySearch 1 ([1])
([],[1])

>>> binarySearch 4 ([1,2,3,4,5,6,7,8,9])
([1,2,3],[4,5,6,7,8,9])

>>> binarySearch 100 ([1,2,4,8,16,32,64,128,256])
([1,2,4,8,16,32,64],[128,256])
-}
binarySearch :: Ord a => a -> [a] -> ([a], [a])
binarySearch a x =
    case splitAt (length x `div` 2) x of
        (_, []) -> ([], [])   -- length of `x` should be 0
        (y, z'@(z:zs)) -> case compare z a of
            LT -> (y <> (z:le), gr)
                where (le, gr) = binarySearch a zs
            _ -> (le, gr <> z')
                where (le, gr) = binarySearch a y
