{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Range
    ( Ranged(range, start, end)
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
import Zeroed


-- | The counterpart to `Bounded`, Ranged elements `a` are bounded
-- | given a certain context `x`. For example, `Day` types are bounded in
-- | `Year:/:Month` types, where the maximum bound of the `Day` is dependant
-- | both upon the Month (September or October?) and the year (Leap year?)
class Enum a => Ranged a x | a -> x, x -> a where
    range :: x -> (a, a)
    range = start &&& end
    start :: x -> a
    start = fst . range
    end :: x -> a
    end = snd . range



-- == Operations on individual elements == --

-- | Whether or not an orderable Ranged element is in the given range
-- | (Useful for normalization purposes)
isInRange :: (Ord b, Ranged b a) => a -> b -> Bool
isInRange a b = b >= start a && b <= end a

-- | Encode a ranged value as an Integer
intify :: forall a b. (Ranged b a, Integral b) => a -> b -> Integer
intify a b = fromIntegral b - fromIntegral (start a :: b)

-- | Decode a ranged value from an Integer
elemify :: forall a b. (Ranged b a, Integral b) => a -> Integer -> b
elemify a i = fromIntegral (i + fromIntegral (start a :: b))



-- == Operations on the set of elements == --

-- | A list of all the values in the range
elements :: Ranged a x => x -> [a]
elements = enumFromTo <$> start <*> end

-- | The count of all values in the range
count :: (Integral a, Ranged a x) => x -> Integer
count x = succ $ fromIntegral (end x) - fromIntegral (start x)



-- == Operations involving the context == --

-- | The size of an element and it's preceeding contexts.
-- | For example, the size of (Year 2, Month 4) is 14 in the Gregorian
-- | calendar, because April of Year 2 is the 14th (zero-indexed) month
-- | of the calendar. (Year 1, Month 1 is size 0.)
-- | (Year 0, Month 12) is size -1, by contrast.
size :: (Zeroed a, Integral b, Ranged b a) => a -> b -> Integer
size a b = intify a b + sum (map count $ predecessors a)

-- | Essentially, this function is version of `quotRem` where the
-- | quotients keep changing. It splits an integer into a quotient
-- | and a remainder, where the quotient is (for instance) of type
-- | (Year:/:Month) and the remainder is used to construct the Day.
split :: (Zeroed a, Integral b, Ranged b a) => Integer -> (a, Integer)
split n = choose 0 elems where
    elems = if n >= 0 then nexts zero else prevs (prev zero)
    choose _ [] = error "Reached the end of an infinite list"
    choose t (a:as) = let u = t + count a
        in if enough u then (a, leftover t u) else choose u as
    enough c = if n >= 0 then c > n else c >= (-n)
    leftover b c = if n >= 0 then n-b else n+c
