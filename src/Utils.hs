module Utils
    ( (!!)
    , (%)
    , around
    , leftover
    , pad
    , showRatio
    ) where
import Prelude hiding ((!!))
import Data.Ratio (Ratio, numerator, denominator)
import Text.Printf (printf)

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

-- | Shows like "x/y" instead of "x % y", or "x" instead of "x/1"
showRatio :: (Show a, Num a, Eq a, Integral a) => Ratio a -> String
showRatio x
    | denominator x == 1 = show $ numerator x
    | otherwise = printf "%s/%s" (show $ numerator x) (show $ denominator x)
