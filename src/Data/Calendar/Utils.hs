module Data.Calendar.Utils
    ( count
    , force
    , signed
    , search
    , maxMag
    , flatten2
    , allBefore
    , nearerZero
    ) where
import Control.Arrow ((***))
import System.Random (Random)
import Test.QuickCheck (sized, choose, Gen)


-- | The number of elements in a bounded enum
count :: (Bounded t, Num t, Enum t) => t
count = succ (maxBound - minBound)


-- | All elements from the given one to zero, non-inclusive
allBefore :: Integral a => a -> [a]
allBefore a = if a >= 0 then [0..pred a] else [succ a..pred 0]


-- | A test set generator that does not exceed a certain magnitude
maxMag :: (Random a, Integral a) => Int -> Gen a
maxMag n = sized $ \s -> choose $ from (negate $ max s n, max s n)
    where from = fromIntegral *** fromIntegral


-- | Given a sentinal value that determines the sign of output,
-- | alter the sign of a given input
signed :: (Integral a, Integral b) => a -> b -> b
signed = (*) . sign where sign a = if a >= 0 then 1 else -1


-- | Given
-- | * a (value -> size of quotient) counting function
-- | * a (presumably infinite) list of values
-- | * a target value to watch for
-- | this function finds a value, of the same sign as the target, that is
-- | of maximum magnitude not exceeding that of the target, and also returns
-- | the difference between the found value and the target.
-- |
-- | In other words, this is `divMod` where the quotients keep changing size.
-- | It's useful for trying to get a decent `divMod` out of something like
-- | the number of days in a list of months.
search :: (Integral a, Integral b, Integral c) => (a -> c) -> [a] -> c -> (a, b)
search cnt xs n = search' 0 xs where
    search' _ [] = error "so apparently that list wasn't infinite."
    search' t (b:as) = let u = t + cnt b
        in if enough u then (b, leftover t u) else search' u as
    enough c = if n >= 0 then c > n else c >= (-n)
    leftover b c = fromIntegral $ if n >= 0 then n-b else n+c


nearerZero :: (Integral x, Integral a, Bounded a) => x -> a -> [a]
nearerZero x a = if x >= 0 then [minBound..pred a] else [succ a..maxBound]


-- | Attempts to find a value at a certain index in a list.
-- | If the index is not present, falls back to the first element in the list.
-- | If the list is empty, falls back to a given default value.
force :: a -> Int -> [a] -> a
force a _ [] = a
force _ i xs = cycle xs !! i


flatten2 :: [(a, a)] -> [a]
flatten2 (x:xs) = fst x : snd x : flatten2 xs
flatten2 [] = []
