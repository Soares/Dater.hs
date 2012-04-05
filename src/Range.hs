{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Range
    ( Zeroed(zero)
    , Ranged(range, start, end)
    , predecessors
    , elements
    , count
    , size
    , split
    , intify
    , elemify
    , isInRange
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

size :: (Zeroed a, Integral b, Ranged b a) => a -> b -> Integer
size a b = intify a b + sum (map count $ predecessors a)

split :: (Zeroed a, Integral b, Ranged b a) => Integer -> (a, Integer)
split n = choose 0 elems where
    elems = if n >= 0 then nexts zero else prevs (prev zero)
    choose _ [] = error "Reached the end of an infinite list"
    choose t (a:as) = let u = t + count a
        in if enough u then (a, leftover t u) else choose u as
    enough c = if n >= 0 then c > n else c >= (-n)
    leftover b c = if n >= 0 then n-b else n+c

intify :: forall a b. (Ranged b a, Integral b) => a -> b -> Integer
intify a b = fromIntegral b - fromIntegral (start a :: b)

elemify :: forall a b. (Ranged b a, Integral b) => a -> Integer -> b
elemify a i = fromIntegral (i + fromIntegral (start a :: b))

isInRange :: (Ord b, Ranged b a) => a -> b -> Bool
isInRange a b = b >= start a && b <= end a

instance Zeroed Integer where zero = 0
instance Zeroed Int where zero = 0
