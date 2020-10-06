{-# LANGUAGE InstanceSigs #-}
module Control.Category.Lens where


import Prelude hiding (id, (.))
import Control.Category


data Lens a b = Lens (a -> b) (a -> b -> a)

instance Category Lens where
    id :: Lens a a
    id = Lens id const
    (.) :: Lens b c -> Lens a b -> Lens a c
    Lens f s . Lens g t = Lens (f . g) u
        where
        u a c = t a $ s (g a) c
