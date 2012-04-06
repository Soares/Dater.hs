{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
module Data.DateTime.VarPart ((:/:)(..)) where
import Data.Coded
import Data.Normalize
import Data.Pair
import Data.Ranged
import Data.Zeroed

-- | A simple combinator, intended for combining types into a date type
data a :/: b = a :/: b deriving (Eq, Ord)

-- | Some utilities to treat :/: like a tuple
instance Pair (:/:) where
    toTuple (a:/:b) = (a,b)
    fromTuple (a,b) = a:/:b

-- | The slash is utalized by default when showing date values.
-- | Use a newtype and override for different semantics.
-- | Note that you can easily change the formatting with a formatter
-- | without resorting to newtypes.
instance (Show a, Show b) => Show (a:/:b) where
    show (a:/:b) = show a ++ "/" ++ show b

-- | Defines the starting point for a date
instance (Zeroed a, Ranged b a, Integral b) => Zeroed (a:/:b) where
    zero = zero :/: start zero

-- | Allows us to generate prior and succeeding dates
instance (Zeroed a, Enum a, Ord b, Ranged b a, Integral b) => Enum (a:/:b) where
    succ (a:/:b)
        | b < end a = a :/: succ b
        | otherwise = succ a :/: start a
    pred (a:/:b)
        | b > start a = a :/: pred b
        | otherwise = pred a :/: end a
    toEnum = decode . fromIntegral
    fromEnum = fromIntegral . encode

-- | Normalizes the date such that all parts are within their respective
-- | ranges. By convention, dates shouldn't overflow: they should just get
-- | bigger and smaller.
instance (Enum a, Normalize a, Ranged b a, Integral b) => Normalize (a:/:b) where
    isNormal (a:/:b) = isNormal a && isInRange a b
    normalize (a:/:b)
        | not (isNormal a) = let
            (o, a') = normalize a
            (p, ab) = normalize $ a' :/: b
            in (o+p, ab)
        | isInRange a b = (0, a:/:b)
        | b > end a = let
            a' = succ a
            b' = fromInteger $ toInteger b - count a
            in normalize $ a' :/: b'
        | otherwise = let
            a' = pred a
            b' = fromInteger $ toInteger b + count a'
            in normalize $ a' :/: b'

-- | Allows us to encode and decode the date
instance (Zeroed a, Integral b, Ranged b a) => Coded (a:/:b) where
    encode (a:/:b) = size a b
    decode = fromTuple . (id **^ elemify) . split
        where (**^) f g (a, b) = (f a, g a b)

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
    elems = if n >= 0 then nexts zero else prevs (pred zero)
    choose _ [] = error "Reached the end of an infinite list"
    choose t (a:as) = let u = t + count a
        in if enough u then (a, leftover t u) else choose u as
    enough c = if n >= 0 then c > n else c >= (-n)
    leftover b c = if n >= 0 then n-b else n+c
