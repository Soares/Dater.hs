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
    (:.) :: Natural n => a -> List n a -> List (Succ n) a
infixr :.



instance Eq a => Eq (L0 a) where (==) = const $ const True
instance (Eq a, Eq (List n a)) => Eq (List (Succ n) a) where
    (x:.v) == (y:.w) = x == y && v == w



instance Ord a => Ord (L0 a) where (<=) = const $ const True
instance (Ord a, Ord (List n a)) => Ord (List (Succ n) a) where
    (x:.v) <= (y:.w)
        | x < y = True
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



instance (Show a, Natural n) => Show (List n a) where
    show v = "<" ++ intercalate "," (fmap show $ toList v) ++ ">"



instance Functor L0 where
    fmap _ _ = Nil

instance Functor (List n) => Functor (List (Succ n)) where
    fmap f (a:.v) = f a :. fmap f v



instance Applicative L0 where
    pure _ = Nil
    _ <*> _ = Nil

instance (Natural n, Applicative (List n)) =>
    Applicative (List (Succ n)) where
    pure a = a :. pure a
    (f:.fs) <*> (a:.as) = f a :. (fs <*> as)


class Listlike v where
    length :: v a -> Integer
    toList :: v a -> [a]
    getElem :: Int -> v a -> a
    setElem :: Int -> a -> v a -> v a

instance Natural n => Listlike (List n) where
    length Nil = 0
    length (_:.as) = 1 + length as
    toList Nil = []
    toList (a:.as) = a : toList as
    getElem _ Nil = error "index out of bounds"
    getElem 0 (a:._) = a
    getElem n (_:.as) = getElem (n-1) as
    setElem _ _ Nil = error "index out of bounds"
    setElem 0 x (_:.as) = x :. as
    setElem n x (a:.as) = a :. setElem (n-1) x as


class FromList v where
    fromList :: [a] -> v a
instance FromList L0 where
    fromList [] = Nil
    fromList _ = error "too many elements for list"
instance (Natural n, FromList (List n)) => FromList (List (Succ n)) where
    fromList (x:xs) = x :. fromList xs
    fromList _ = error "too few elements for list"



class Reduce v a | v -> a where
    reduce :: (a -> a -> a) -> a -> v a -> a

instance Reduce (List n) a where
    reduce _ a Nil = a
    reduce f x (a:.v) = reduce f (f x a) v



class Concat u v w | u v -> w where
    concat :: u a -> v a -> w a

instance Concat L0 L0 L0 where
    concat _ _ = Nil

instance Concat L0 (List n) (List n) =>
         Concat L0 (List (Succ n)) (List (Succ n)) where
    concat _ v = v

instance (Natural n, Natural m, Natural o, Concat (List n) (List m) (List o)) =>
         Concat (List (Succ n)) (List m) (List (Succ o)) where
    concat (a:.v) w = a :. concat v w



class Snoc v w where
    snoc :: a -> v a -> w a

instance Snoc L0 L1 where
    snoc x Nil = x :. Nil

instance Snoc (List n) (List (Succ n)) =>
         Snoc (List (Succ n)) (List (Succ (Succ n))) where
    snoc x (a:.v) = a :. snoc x v



class Reverse v where
    reverse :: v a -> v a

instance Reverse (List N0) where
    reverse _ = Nil

instance (Snoc (List n) (List (Succ n)), Reverse (List n)) => Reverse (List (Succ n)) where
    reverse (a:.v) = snoc a (reverse v)
