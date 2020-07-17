module Data.Zipper where

import           Control.Comonad
import           Data.Stream     (Stream (..))
import qualified Data.Stream     as S


data Zipper a = Zipper (Stream a) a (Stream a)

right :: Zipper a -> Zipper a
right (Zipper ls x (r :< rs)) = Zipper (x :< ls) r rs

left :: Zipper a -> Zipper a
left (Zipper (l :<ls) x rs) = Zipper ls l (x :< rs)

instance Functor Zipper where
    fmap f (Zipper ls c rs) = Zipper (fmap f ls) (f c) (fmap f rs)

iterate1 :: (a -> a) -> a -> Stream a
iterate1 = fmap S.tail . S.iterate

instance Comonad Zipper where
    extract (Zipper _ a _) = a
    duplicate z = Zipper (iterate1 left z) z (iterate1 right z)
    extend f = fmap f . duplicate
