{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- TODO: Rename to BoundedIn
module Data.Ranged
    ( Ranged(range, start, end, size, split)
    , signed
    , search
    , elements
    , count
    , intify
    , isInRange
    , maxMag
    , allBefore
    , nearerZero
    , predecessorsIn
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
    size a b = signed a $ intify a b + (sum $ map count $ allBefore a)
    split :: Integer -> (x, a)
    split = search count =<< elems
        where elems n = if n >= 0 then [0..] else [-1, -2..]

signed :: forall a. Integral a => a -> Integer -> Integer
signed = (*) . sign where
    sign a = if a >= 0 then 1 else -1

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


-- TODO: Not sure that these should be in Ranged.
allBefore :: Integral a => a -> [a]
allBefore a = if a >= 0 then [0..pred a] else [succ a..pred 0]

nearerZero :: (Integral x, Integral a, Bounded a) => x -> a -> [a]
nearerZero x a = if x >= 0 then [minBound..pred a] else [succ a..maxBound]

predecessorsIn :: Ranged a x => x -> a -> [a]
predecessorsIn x a = if x >= 0 then [start x..pred a] else [succ a..end x]
