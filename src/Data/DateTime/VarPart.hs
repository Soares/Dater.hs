{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.DateTime.VarPart ((:/:)(..), Composable, size, split) where
import Control.Applicative
import Data.Coded
import Data.Normalize
import Data.Pair
import Data.Ranged
import Data.Zeroed
import Test.QuickCheck.Arbitrary
import Text.Format.Write

-- | A simple combinator, intended for combining types into a date type
data a :/: b = a :/: b

type Composable a b = (Enum a, Normalize a, Ranged b a, Integral b)

-- | Equality is checked post-normalization
instance (Eq a, Eq b, Composable a b) => Eq (a:/:b) where
    x == y = let
        (a:/:b) = normal x
        (c:/:d) = normal y
        in a == c && b == d

-- | Ordering is determined post-normalization
instance (Ord a, Ord b, Composable a b) => Ord (a:/:b) where
    x <= y = let
        (a:/:b) = normal x
        (c:/:d) = normal y
        in if a == c then b <= d else a < c

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
instance (Zeroed a, Composable a b) => Zeroed (a:/:b) where
    zero = zero :/: start zero

-- | Allows us to generate prior and succeeding dates
instance (Zeroed a, Composable a b) => Enum (a:/:b) where
    succ (a:/:b)
        | not (isNormal (a:/:b)) = succ (normal (a:/:b))
        | b < end a = a :/: succ b
        | otherwise = succ a :/: start (succ a)
    pred (a:/:b)
        | not (isNormal (a:/:b)) = pred (normal (a:/:b))
        | b > start a = a :/: pred b
        | otherwise = pred a :/: end (pred a)
    toEnum = decode . fromIntegral
    fromEnum = fromIntegral . encode

-- | Normalizes the date such that all parts are within their respective
-- | ranges. By convention, dates shouldn't overflow: they should just get
-- | bigger and smaller.
instance Composable a b => Normalize (a:/:b) where
    isNormal (a:/:b) = isNormal a && isInRange a b
    normalize (a:/:b)
        | not (isNormal a) = let
            (o, a') = normalize a
            (p, ab) = normalize $ a' :/: b
            in (o+p, ab)
        | isInRange a b = (0, a:/:b)
        | b > end a = let
            a' = succ a
            adjusted = b + start a' - start a
            delta = fromInteger $ count a
            in normalize $ a' :/: (adjusted - delta)
        | otherwise = let
            a' = pred a
            adjusted = b + start a' - start a
            delta = fromInteger $ count a'
            in normalize $ a' :/: (adjusted + delta)

-- | Allows us to encode and decode the date
instance (Zeroed a, Composable a b) => Coded (a:/:b) where
    encode = (size <$> left <*> right) . normal
    decode = fromTuple . (id **^ elemify) . split
        where (**^) f g (a, b) = (f a, g a b)

-- | When asked to be formatted, give the number of days since 0
instance (Zeroed a, Composable a b) => Formattable (a:/:b) where
    numbers = pure . show . encode

-- | Allows QuickCheck testing
instance (Arbitrary a, Arbitrary b) => Arbitrary (a:/:b) where
    arbitrary = (:/:) <$> arbitrary <*> arbitrary
    shrink (a:/:b) = [ x:/:b | x <- shrink a ] ++ [ a:/:y | y <- shrink b ]

-- | The size of an element and it's preceeding contexts.
-- | For example, the size of (Year 2, Month 4) is 14 in the Gregorian
-- | calendar, because April of Year 2 is the 14th (zero-indexed) month
-- | of the calendar. (Year 1, Month 1 is size 0.)
-- | (Year 0, Month 12) is size -1, by contrast.
size :: (Zeroed a, Integral b, Ranged b a) => a -> b -> Integer
size a b = intify a b + ((if a >= zero then 1 else -1) * sizea) where
    sizea = sum $ map count $ predecessors a

-- | Essentially, this function is version of `quotRem` where the
-- | quotients keep changing. It splits an integer into a quotient
-- | and a remainder, where the quotient is (for instance) of type
-- | (Year:/:Month) and the remainder is used to construct the Day.
split :: (Zeroed a, Integral b, Ranged b a) => Integer -> (a, Integer)
split n = choose 0 elems where
    elems = if n >= 0 then succs zero else preds (pred zero)
    succs a = a : succs (succ a)
    preds a = a : preds (pred a)
    choose _ [] = error "Reached the end of an infinite list"
    choose t (a:as) = let u = t + count a
        in if enough u then (a, leftover t u) else choose u as
    enough c = if n >= 0 then c > n else c >= (-n)
    leftover b c = if n >= 0 then n-b else n+c
