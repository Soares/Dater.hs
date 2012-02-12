module Utils
    ( (!!)
    , (%)
    , around
    , leftover
    , pad
    , prods
    ) where
import Prelude hiding ((!!))

-- | Generally, turns a list into a list of products.
-- | Useful in the following scenario:
-- |
-- | Given 24 hours in a day, 60 minutes in a second, 60 seconds in a minute
-- | as the list [24, 60, 60]
-- | generates the list [86400, 3600, 60, 1]
-- | Which can be used to combine days, hours, minutes, and seconds into a
-- | common ratio.
prods :: [Integer] -> [Integer]
prods [] = [1]
prods (x:xs) = x * product xs : prods xs

-- | Pad a list out to a known length
pad :: a -> Int -> [a] -> [a]
pad _ 0 xs = xs
pad x n [] = x : pad x (n-1) []
pad x n xs = pad x (n-1) xs

-- | List indexing that works on integers
(!!) :: [a] -> Integer -> a
xs !! 0 = head xs
xs !! n = tail xs !! (n-1)

-- | Division that works on reals
(%) :: (Real a, Real b) => a -> b -> Rational
x % y = toRational x / toRational y

-- | The stuff dropped by floor
leftover :: Rational -> Rational
leftover r = r - toRational (floor r :: Integer)

-- | A helper function that makes it easier to use Range.push
around :: Num a => (a -> b) -> a -> (b, b, b)
around fn a = (fn $ a-1, fn a, fn $ a+1)
