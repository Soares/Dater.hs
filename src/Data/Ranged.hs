{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Ranged
    ( Ranged(range, start, end, size, split)
    , elems -- TODO: rename
    , signed
    , search
    , elements
    , count
    , intify
    , elemify
    , isInRange
    , maxMag
    ) where
import Control.Applicative
import Control.Arrow
import System.Random (Random)
import Test.QuickCheck (sized, choose, Gen)

-- TODO: We can do a lot of cleaning up with these integral constraints


-- | The counterpart to `Bounded`, Ranged elements `a` are bounded
-- | given a certain context `x`. For example, `Day` types are bounded in
-- | `Year:/:Month` types, where the maximum bound of the `Day` is dependant
-- | both upon the Month (September or October?) and the year (Leap year?)
class (Integral a, Integral x) => Ranged a x | a -> x, x -> a where
    range :: x -> (a, a)
    range = start &&& end
    start :: x -> a
    start = fst . range
    end :: x -> a
    end = snd . range
    size :: x -> a -> Integer
    size a b = signed a $ intify a b + (sum $ map count $ predecessors a)
    split :: Integer -> (x, a)
    split = search count =<< elems

signed :: forall a. Integral a => a -> Integer -> Integer
signed = (*) . toInteger . sign where
    sign a = if a >= 0 then 1 else -1

elems :: (Integral a, Integral b) => a -> [b]
elems n = if n >= 0 then [0..] else [-1, -2..]

search :: (Integral x, Integral a) => (x -> Integer) -> [x] -> Integer -> (x, a)
search cnt xs n = search' 0 xs where
    search' _ [] = error "Look, ma! I found the end of an infinite list!"
    search' t (a:as) = let u = t + cnt a
        in if enough u then (a, leftover t u) else search' u as
    enough c = if n >= 0 then c > n else c >= (-n)
    leftover b c = fromInteger $ if n >= 0 then n-b else n+c



-- == Operations on individual elements == --

-- | Whether or not an orderable Ranged element is in the given range
-- | (Useful for normalization purposes)
isInRange :: (Ord b, Ranged b a) => a -> b -> Bool
isInRange a b = b >= start a && b <= end a

-- | Encode a ranged value as an Integer
intify :: forall a x. Ranged a x => x -> a -> Integer
intify x a
    | x >= 0 = fromIntegral (a - start x)
    | otherwise = fromIntegral ((1 + (end (pred x))) - a)

-- | Decode a ranged value from an Integer
-- | TODO: This probably doesn't work.
-- | At the least, quickcheck it's indempotence against intify.
elemify :: forall a b. (Ranged b a, Integral b) => a -> Integer -> b
elemify a i = fromIntegral (i + fromIntegral (start a :: b))



-- == Operations on the set of elements == --

-- | A list of all the values in the range
elements :: Ranged a x => x -> [a]
elements = enumFromTo <$> start <*> end

-- | The count of all values in the range
count :: (Integral a, Ranged a x) => x -> Integer
count x = succ $ fromIntegral (end x) - fromIntegral (start x)


-- == Testing Utilities == --

maxMag :: (Random a, Integral a) => Int -> Gen a
maxMag n = sized $ \s -> choose $ from (negate $ max s n, max s n)
    where from = fromIntegral *** fromIntegral


-- TODO: Move to a better place
-- | All elements between the given element and the zero element.
-- | The positive direction contains the zero element, the negative
-- | direction does not.
-- | (Ex. predecessors 3 = [0,1,2,3]; predecessors (-2) = [-2, -1]
predecessors :: Integral a => a -> [a]
predecessors a
    | a == 0 = []
    | a > 0 = succTo 0 (pred a)
    | otherwise = predTo (pred 0) a

-- | The list of all values from one point to another, inclusive
succTo :: (Eq a, Enum a) => a -> a -> [a]
succTo a b
    | a == b = [a]
    | otherwise = a : succTo (succ a) b

-- | The list of all values from one point to backwards to another, inclusive
predTo :: (Eq a, Enum a) => a -> a -> [a]
predTo a b
    | a == b = [a]
    | otherwise = a : predTo (pred a) b
