{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Range
    ( Zeroed(zero)
    , Ranged(range, start, end)
    , predecessors
    , elements
    , count
    ) where
import Control.Applicative
import Control.Arrow
import Gen

class (Ord a, Gen a) => Zeroed a where
    zero :: a

class Enum a => Ranged a x | a -> x, x -> a where
    range :: x -> (a, a)
    range = start &&& end
    start :: x -> a
    start = fst . range
    end :: x -> a
    end = snd . range

predecessors :: (Eq a, Ord a, Zeroed a) => a -> [a]
predecessors a
    | a == zero = []
    | a > zero = nextTo zero (prev a)
    | otherwise = prevTo (prev zero) a

elements :: Ranged a x => x -> [a]
elements = enumFromTo <$> start <*> end

count :: (Integral a, Ranged a x) => x -> Integer
count x = succ $ fromIntegral (end x) - fromIntegral (start x)

size2 :: Ranged a x => (x -> a -> Integer) -> x -> Integer
size2 subsize x = sum $ map (subsize x) (elements x)

instance Zeroed Integer where zero = 0
instance Zeroed Int where zero = 0
