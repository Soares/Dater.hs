{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Data.BoundedIn
    ( BoundedIn(range, start, end, size, split)
    , nearerZeroIn
    , isInBounds
    , countIn
    , intify
    ) where
import Control.Arrow ((&&&))
import Data.Calendar.Utils (signed, search, allBefore)


-- | Contextually bounded types.
-- | The counterpart to `Bounded`, BoundedIn elements `a` are bounded
-- | given a certain context `x`.
class (Integral a, Integral x) => BoundedIn a x | a -> x, x -> a where
    range :: x -> (a, a)
    range = start &&& end

    start :: x -> a
    start = fst . range

    end :: x -> a
    end = snd . range

    -- | Override size and split if you need efficiency
    -- | For example, if you must support leap seconds, you _really_ don't
    -- | want `size YMDHm s` to do a sum of (map count) over all the minutes
    -- | that came before..
    size :: x -> a -> Integer
    size a b = signed a $ intify a b + sum (map countIn $ allBefore a)

    split :: Integer -> (x, a)
    split = search countIn =<< elems
        where elems n = if n >= 0 then [0..] else [-1, -2..]


-- | Whether or not an orderable BoundedIn element is in the given range
-- | (Useful for normalization purposes)
isInBounds :: (Ord b, BoundedIn b a) => a -> b -> Bool
isInBounds a b = b >= start a && b <= end a


-- | Encode a contextually bounded value as an Integer
-- | Be careful how you get it out. Make sure you add (start x)
-- | as necessary when decoding.
intify :: BoundedIn a x => x -> a -> Integer
intify x a
    | x >= 0 = fromIntegral (a - start x)
    | otherwise = succ $ fromIntegral $ end (pred x) - a


countIn :: BoundedIn a x => x -> Integer
countIn x = fromIntegral $ succ $ end x - start x


nearerZeroIn :: BoundedIn a x => x -> a -> [a]
nearerZeroIn x a = if x >= 0 then [start x..pred a] else [succ a..end x]
