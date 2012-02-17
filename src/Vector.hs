-- TODO: Remove unused language extensions
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Vector where
import Data.Vec.Nat
import Data.List (intercalate)
import Prelude hiding (map, reverse)

type Vec0 = Vector N0
type Vec1 = Vector N1
type Vec2 = Vector N2
type Vec3 = Vector N3
type Vec4 = Vector N4
type Vec5 = Vector N5
type Vec6 = Vector N6
type Vec7 = Vector N7
type Vec8 = Vector N8
type Vec9 = Vector N9
type Vec10 = Vector N10
type Vec11 = Vector N11
type Vec12 = Vector N12
type Vec13 = Vector N13
type Vec14 = Vector N14
type Vec15 = Vector N15
type Vec16 = Vector N16
type Vec17 = Vector N17
type Vec18 = Vector N18
type Vec19 = Vector N19

data Vector n a where
    Nil :: Vector N0 a
    (:.) :: a -> Vector n a -> Vector (Succ n) a
infixr :.


instance (Show a, List (Vector n)) => Show (Vector n a) where
    show v = "<" ++ intercalate "," (fmap show $ toList v) ++ ">"


instance Functor (Vector N0) where
    fmap _ _ = Nil
instance Functor (Vector n) => Functor (Vector (Succ n)) where
    fmap f (a:.v) = f a :. fmap f v


-- TODO: Instance monad


class Vec v where
    vec :: a -> v a
    getElem :: Int -> v a -> a
    setElem :: Int -> a -> v a -> v a
instance Vec (Vector N0) where
    vec _ = Nil
    getElem _ _ = error "List index out of bounds"
    setElem _ _ _ = error "List index out of bounds"
instance Vec (Vector n) => Vec (Vector (Succ n)) where
    vec a = a :. vec a
    getElem 0 (a:._) = a
    getElem n (_:.as) = getElem (n-1) as
    setElem 0 x (_:.as) = x :. as
    setElem n x (a:.as) = a :. setElem (n-1) x as

class List v where
    toList :: v a -> [a]
    fromList :: [a] -> v a
instance List (Vector N0) where
    toList Nil = []
    fromList [] = Nil
    fromList _ = error "Too many elements in list"
instance List (Vector n) => List (Vector (Succ n)) where
    toList (a:.as) = a : toList as
    fromList [] = error "Too few elements in list"
    fromList (x:xs) = x :. fromList xs


class Reduce v a | v -> a where
    reduce :: (a -> a -> a) -> a -> v -> a
instance Reduce (Vector n a) a where
    reduce _ a Nil = a
    reduce f x (a:.v) = reduce f (f x a) v


class Map a b u v
    | u -> a
    , v -> b
    , a v -> u
    , b u -> v
    where
    map :: (a -> b) -> u -> v
instance Map a b (Vector n a) (Vector n b) where
    map _ Nil = Nil
    map f (a:.u) = f a :. map f u


class Combineable a b c u v w
    | u -> a
    , v -> b
    , w -> c
    , a v w -> u
    , b u w -> v
    , c v u -> w
    where
    combine :: (a -> b -> c) -> u -> v -> w
instance Combineable a b c (Vector n a) (Vector n b) (Vector n c) where
    combine f (a:.u) (b:.v) = f a b :. combine f u v
    combine _ Nil Nil = Nil
    combine _ _ _ = error "Can't combine vectors of different length"


class (Reduce v a, Num a) => Multiplicands a v | v -> a where
    multiplicands :: v -> v
instance (Num a) => Multiplicands a (Vector n a) where
    multiplicands Nil = Nil
    multiplicands (_:.v) = reduce (*) 1 v :. multiplicands v


class (Integral a) => SplitUp a v | v -> a where
    splitUp :: a -> v -> v
instance (Integral a) => SplitUp a (Vector n a) where
    splitUp _ Nil = Nil
    splitUp n (a:.v) = mod n a :. splitUp (div n a) v


class Snoc a v w | v a -> w, w -> v a, v -> a where
    snoc :: a -> v -> w
instance Snoc a (Vector N0 a) (Vector N1 a) where
    snoc x Nil = x :. Nil
instance Snoc a (Vector n a) (Vector (Succ n) a) =>
         Snoc a (Vector (Succ n) a) (Vector (Succ (Succ n)) a) where
    snoc x (a:.v) = (a:.snoc x v)


class Reverse v where
    reverse :: v -> v
instance Reverse (Vector N0 a) where
    reverse _ = Nil
instance (Snoc a (Vector n a) (Vector (Succ n) a), Reverse (Vector n a)) => Reverse (Vector (Succ n) a) where
    reverse (a:.v) = snoc a (reverse v)
