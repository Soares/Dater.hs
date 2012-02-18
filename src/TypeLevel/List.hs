{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
module TypeLevel.List where
import Control.Applicative
import Data.List (intercalate)
import Prelude hiding (reverse, length, concat)
import TypeLevel.Naturals

type L0  = List N0
type L1  = List N1
type L2  = List N2
type L3  = List N3
type L4  = List N4
type L5  = List N5
type L6  = List N6
type L7  = List N7
type L8  = List N8
type L9  = List N9
type L10 = List N10
type L11 = List N11
type L12 = List N12
type L13 = List N13
type L14 = List N14
type L15 = List N15
type L16 = List N16
type L17 = List N17
type L18 = List N18
type L19 = List N19


data List n a where
    Nil :: List N0 a
    (:.) :: a -> List n a -> List (Succ n) a
infixr :.


instance Eq a => Eq (List N0 a) where (==) = const $ const True
instance (Eq a, Eq (List n a)) => Eq (List (Succ n) a) where
    (x:.v) == (y:.w) = x == y && v == w


instance Ord a => Ord (List N0 a) where (<=) = const $ const True
instance (Ord a, Ord (List n a)) => Ord (List (Succ n) a) where
    (x:.v) <= (y:.w) | x < y = True
                     | x == y = v <= w
                     | otherwise = False


instance ( Num a
         , Natural n
         , Applicative (List n)
         ) => Num (List n a) where
    x + y = (+) <$> x <*> y
    x - y = (-) <$> x <*> y
    x * y = (*) <$> x <*> y
    abs = fmap abs
    signum = fmap signum
    fromInteger = pure . fromInteger


instance ( Fractional a
         , Natural n
         , Applicative (List n)
         ) => Fractional (List n a) where
    u / v = (/) <$> u <*> v
    recip = fmap recip
    fromRational = pure . fromRational


instance (Show a, Listable (List n)) => Show (List n a) where
    show v = "<" ++ intercalate "," (fmap show $ toList v) ++ ">"


instance Functor L0 where
    fmap _ _ = Nil

instance Functor (List n) => Functor (List (Succ n)) where
    fmap f (a:.v) = f a :. fmap f v


instance Applicative L0 where
    pure _ = Nil
    _ <*> _ = Nil

instance Applicative (List n) => Applicative (List (Succ n)) where
    pure a = a :. pure a
    (f:.fs) <*> (a:.as) = f a :. (fs <*> as)


class Listable v where
    length :: v a -> Integer
    toList :: v a -> [a]
    fromList :: [a] -> v a
    getElem :: Int -> v a -> a
    setElem :: Int -> a -> v a -> v a

instance Listable L0 where
    length Nil = 0
    toList Nil = []
    fromList [] = Nil
    fromList _ = error "Too many elements in list"
    getElem _ _ = error "List index out of bounds"
    setElem _ _ _ = error "List index out of bounds"

instance Listable (List n) => Listable (List (Succ n)) where
    length (_:.as) = 1 + length as
    toList (a:.as) = a : toList as
    getElem 0 (a:._) = a
    getElem n (_:.as) = getElem (n-1) as
    setElem 0 x (_:.as) = x :. as
    setElem n x (a:.as) = a :. setElem (n-1) x as
    fromList xs = if null xs then error "Too few elements in list"
        else head xs :. fromList (tail xs)


class Reduce v a | v -> a where
    reduce :: (a -> a -> a) -> a -> v -> a

instance Reduce (List n a) a where
    reduce _ a Nil = a
    reduce f x (a:.v) = reduce f (f x a) v


class Concat u v w | u v -> w where
    concat :: u a -> v a -> w a

instance Concat L0 L0 L0 where
    concat _ _ = Nil

instance Concat L0 (List n) (List n) =>
         Concat L0 (List (Succ n)) (List (Succ n)) where
    concat _ v = v

instance Concat (List n) (List m) (List o) =>
         Concat (List (Succ n)) (List m) (List (Succ o)) where
    concat (a:.v) w = a :. concat v w


class Snoc a v w | v a -> w, w -> v a, v -> a where
    snoc :: a -> v -> w

instance Snoc a (L0 a) (L1 a) where
    snoc x Nil = x :. Nil

instance Snoc a (List n a) (List (Succ n) a) =>
         Snoc a (List (Succ n) a) (List (Succ (Succ n)) a) where
    snoc x (a:.v) = a :. snoc x v


class Reverse v where
    reverse :: v -> v

instance Reverse (List N0 a) where
    reverse _ = Nil

instance (Snoc a (List n a) (List (Succ n) a), Reverse (List n a)) => Reverse (List (Succ n) a) where
    reverse (a:.v) = snoc a (reverse v)
