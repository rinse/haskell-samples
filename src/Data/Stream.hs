module Data.Stream where

import           Control.Comonad
import           Prelude         hiding (drop, dropWhile, head, init, iterate,
                                  last, repeat, tail, take, takeWhile)

{-| Infinite stream.
-}
data Stream a = a :< Stream a

instance Functor Stream where
    fmap f (x :< xs) = f x :< fmap f xs

instance Comonad Stream where
    extract (x :< _) = x
    duplicate st = st :< duplicate st

{-| List which is equivalent to a given stream.
    The list is always infinite.
-}
toList :: Stream a -> [a]
toList (x :< xs) = x : toList xs

{-| Stream which is equivalent to a given list.
    Returns nothing when it gets a finite list.
-}
fromList :: [a] -> Maybe (Stream a)
fromList = foldr ((<$>) . (:<)) Nothing

{- $setup
>>> import Control.Monad (guard)
>>> import Data.Maybe    (fromMaybe)
-}

{-| The head of a stream
>>> head $ iterate (+ 1) 0
0
-}
head :: Stream a -> a
head = extract

{-| Stream which the head of a given stream removed
>>> take 3 $ tail $ iterate (+ 1) 0
[1,2,3]
-}
tail :: Stream a -> Stream a
tail (_ :< xs) = xs

{-| Anamorphism of Stream
>>> let (~>) a b c = b <$ guard (c `mod` a == 0)
>>> let f = fromMaybe . show <*> 3 ~> "fizz" <> 5 ~> "buzz"
>>> let fb x = (f x, x + 1)
>>> take 15 $ unfoldr fb 1
["1","2","fizz","4","buzz","fizz","7","8","fizz","buzz","11","fizz","13","14","fizzbuzz"]
-}
unfoldr :: (a -> (b, a)) -> a -> Stream b
unfoldr f a = b :< unfoldr f a'
    where (b, a') = f a

{-| Stream from a value and repeated applications of a function to the value
>>> take 3 $ iterate (+ 1) 0
[0,1,2]
-}
iterate :: (a -> a) -> a -> Stream a
iterate f a = a :< b
    where b = f a :< fmap f b

{-| Stream from a value repeated
>>> take 3 $ repeat "Hello"
["Hello","Hello","Hello"]
-}
repeat :: a -> Stream a
repeat = iterate id

{-| List from the first n elements of a stream
>>> take 3 $ repeat "Hello"
["Hello","Hello","Hello"]
-}
take :: Int -> Stream a -> [a]
take 0 _         = []
take n (x :< xs) = x : take (n - 1) xs

{-| Stream which the first n elements of a given stream dropped
>>> take 3 $ repeat "Hello"
["Hello","Hello","Hello"]
-}
drop :: Int -> Stream a -> Stream a
drop 0 st        = st
drop n (_ :< xs) = drop (n - 1) xs

{-| List of the longest prefix of a stream which satisfy a predicate.
>>> takeWhile (< 10) $ iterate (+ 1) 0
[0,1,2,3,4,5,6,7,8,9]
-}
takeWhile :: (a -> Bool) -> Stream a -> [a]
takeWhile p (x :< xs)
    | p x = x : takeWhile p xs
    | otherwise = []

{-| Stream whose the longest prefix of a stream dropped.
>>> take 10 . dropWhile (< 10) $ iterate (+ 1) 0
[11,12,13,14,15,16,17,18,19,20]
-}
dropWhile :: (a -> Bool) -> Stream a -> Stream a
dropWhile p (x :< xs)
    | p x = dropWhile p xs
    | otherwise = xs
