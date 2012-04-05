{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Ranged
    ( Ranged(range, start, end)
    , elements
    , count
    , intify
    , elemify
    , isInRange
    ) where
import Control.Applicative
import Control.Arrow


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
