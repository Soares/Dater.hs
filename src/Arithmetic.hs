{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Arithmetic where

data NotInRange = Overflow | Underflow

class Negable a where
    neg :: a -> a
class FiniteEnum a x where
    next :: x -> a -> Either NotInRange a
    prev :: x -> a -> Either NotInRange a
class (Negable a, Negable b, FiniteEnum a x) => Arithmetic a b x where
    plus :: x -> a -> b -> Either NotInRange a
    minus :: x -> a -> b -> Either NotInRange a
    minus x a = plus x a . neg
    lift :: x -> b -> Either NotInRange a

newtype Overflowable a = O { unwrapO :: a } deriving (Num, Eq, Ord, Enum, Bounded)
newtype Unbounded    a = U { unwrapU :: a } deriving (Enum, Num)

instance Num a => Negable (Overflowable a) where
    neg = negate
instance (Num a, Eq a, Ord a, Enum a, Bounded a) => FiniteEnum (Overflowable a) x where
    next _ a | a == maxBound = Left Overflow
             | otherwise = Right $ succ a
    prev _ a | a == minBound = Left Underflow
             | otherwise = Right $ pred a
instance (Num a, Eq a, Ord a, Enum a, Bounded a, Negable b, Integral b, FiniteEnum (Overflowable a) x) => Arithmetic (Overflowable a) b x where
    plus _ a i
        | x >= maxBound = Left Overflow
        | x < minBound = Left Underflow
        | otherwise = Right x
        where x = toEnum $ fromEnum a + (fromIntegral i)
    lift x = plus x (toEnum 0)

instance Num a => Negable (Unbounded a) where
    neg = negate
instance (Num a, Enum a) => FiniteEnum (Unbounded a) x where
    next _ = Right . succ
    prev _ = Right . pred
instance (Num a, Enum a, Negable b, Integral b, FiniteEnum (Unbounded a) x) => Arithmetic (Unbounded a) b x where
    plus _ a i = Right $ toEnum $ fromEnum a + (fromIntegral i)
    lift x = plus x (toEnum 0)


-- Orphans
instance Negable Integer where neg = negate
instance Negable Int where neg = negate
instance Negable Rational where neg = negate
instance Negable Float where neg = negate
instance Negable Double where neg = negate
instance Negable a => Negable (Maybe a) where neg = fmap neg
instance Negable a => Negable [a] where neg = fmap neg
