{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Functor2 where

import Control.Category
import Data.Functor.Contravariant (Op (..))
import Prelude hiding (id, (.), Functor (..))


type Hask = (->)

type (:<-) = Op

class (Category cat1, Category cat2) => Functor f cat1 cat2 where
    fmap :: cat1 a b -> cat2 (f a) (f b)

-- A sample of covariant functor
instance Functor [] Hask Hask where
    fmap :: (a -> b) -> [a] -> [b]
    fmap = map

-- A sample of contravariant
instance Functor (Op x) Op Hask where
    fmap :: (a :<- b) -> (x :<- a) -> (x :<- b)
    fmap = (.)

-- F : A*B -> A
type family Fst a
type instance Fst (a, b) = a
-- F : A*B -> B
type family Snd a
type instance Snd (a, b) = b

-- Product Category
newtype (:*) cat1 cat2 a b = Prod (cat1 (Fst a) (Fst b), cat2 (Snd a) (Snd b))

instance (Category cat1, Category cat2) => Category (cat1 :* cat2) where
    id :: (cat1 :* cat2) a a
    id = Prod (id, id)
    Prod (g1, g2) . Prod (f1, f2) = Prod (g1 . f1, g2 . f2)

-- Bifunctor
class (Category cat1, Category cat2, Category cat3) => Bifunctor f cat1 cat2 cat3 where
    bimap :: cat1 a b -> cat2 c d -> cat3 (f a c) (f b d)

-- A sample of covariant bifunctor
instance Bifunctor (,) Hask Hask Hask where
    bimap f g (a, b) = (f a, g b)

-- A sample of profunctor
instance Bifunctor (->) Op Hask Hask where
    bimap (Op f) g = (g .) . (. f)
