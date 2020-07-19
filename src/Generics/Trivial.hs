{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
module Generics.Trivial where

import           Data.Kind    (Type)
import           Data.Void
import           GHC.Generics


type a :+ b = Either a b
infixr 5 :+

type a :* b = (a, b)
infixr 6 :*

{- $setup
>>> :set -XDeriveGeneric
-}

{-| Converts an arbitrary algebraic data type to an isomorphic trivial type.
    Trivial types are algebraic data types which consist of only `(,)` and/or `Either`.

Does nothing for trivial types,
>>> :kind! TrivialType ()
TrivialType () :: *
= ()

Makes them trivial for algebraic types,
>>> :kind! TrivialType Bool
TrivialType Bool :: *
= () :+ ()

Example for recursive types.
>>> :kind! TrivialType [Int]
TrivialType [Int] :: *
= () :+ (Int :* [Int])

It cannot handle types which do not derive Generic.
>>> :kind! TrivialType Int
TrivialType Int :: *
= Trivial (Rep Int)
-}
type family Trivial (rep :: Type -> Type) :: Type
type instance Trivial V1 = Void
type instance Trivial U1 = ()
type instance Trivial (f :+: g) = Trivial f :+ Trivial g
type instance Trivial (f :*: g) = Trivial f :* Trivial g
type instance Trivial (K1 t c) = c
type instance Trivial (M1 t meta f) = Trivial f

type TrivialType a = Trivial (Rep a)
