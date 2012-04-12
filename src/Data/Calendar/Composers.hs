{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Calendar.Composers ((:/)((:/)), (:\)((:\))) where
import Control.Applicative
import Control.Arrow ((***), second)
import Data.Normalize
import Data.Pair
import Data.BoundedIn
import Test.QuickCheck.Arbitrary
import Text.Format.Write

-- | The constant combinator.
-- | Use this when `b` is bounded in it's own right
-- | (i.e. minutes are always [0..59])
-- | It's more efficient but less flexible than it's brother ':\'
data a :/ b = a :/ b

-- | The variable combinator.
-- | Use this when `b` is bounded for a given `a`
-- | (i.e. February is [0..27] or [0..28] depending on the year)
-- | It's less efficient but more felxible than it's brother ':/'
data a :\ b = a :\ b


-- | The type constraint for types that should be composed using the
-- | constant combinator. Adhering to these constraints makes the composed
-- | type a normalizable integral, amongst other things.
type Constposable a b = (Integral a, Integral b, Normalize a, Bounded b)


-- | The type constraint for types that should be composed using the
-- | variable combinator. Adhering to these constraints makes the composed
-- | type a normalizable integral, amongst other things.
type Varposable a b = (Integral a, Integral b, Normalize a, BoundedIn b a)


-- | You may use the composers like tuples
instance Pair (:/) where
    toTuple (a:/b) = (a, b)
    fromTuple (a, b) = a :/ b

instance Pair (:\) where
    toTuple (a:\b) = (a, b)
    fromTuple (a, b) = a :\ b


-- | Some helper functions for normality-dependant boolean operations
eq :: (Pair p, Eq a, Eq b, Normalize (p a b)) => p a b -> p a b -> Bool
eq x y = a == c && b == d where
    (a, b) = toTuple $ normal x
    (c, d) = toTuple $ normal y

lt :: (Pair p, Ord a, Ord b, Normalize (p a b)) => p a b -> p a b -> Bool
lt x y = if a == c then b <= d else a <= c where
    (a, b) = toTuple $ normal x
    (c, d) = toTuple $ normal y

-- | Equality and ordering of composed types are independant of normalization,
-- | i.e. January 32nd == February 1st
instance Constposable a b => Eq (a:/b) where (==) = eq
instance Varposable a b => Eq (a:\b) where (==) = eq
instance Constposable a b => Ord (a:/b) where (<=) = lt
instance Varposable a b => Ord (a:\b) where (<=) = lt


-- | The constant composer uses a dash when showing, because it is simpler.
-- | The variable composer uses an equals sign.
-- | (I find it a bit confusing to use / and \ signs.)
-- | If you want a well-formatted DateTime, you'll want to take a gander at
instance (Show a, Show b) => Show (a:/b) where
    show (a:/b) = show a ++ "-" ++ show b

instance (Show a, Show b) => Show (a:\b) where
    show (a:\b) = show a ++ "=" ++ show b


-- | Allows propagation of bounds through bounded constant compositions.
-- | Constant compositions need not be bounded to be integrals.
instance (Bounded a, Bounded b) => Bounded (a:/b) where
    minBound = minBound :/ minBound
    maxBound = maxBound :/ maxBound


-- | Notice how constant compositions can be normalized much more efficiently.
instance Constposable a b => Normalize (a:/b) where
    isNormal (a:/b) = isNormal a && b >= minBound && b <= maxBound
    normalize (a:/b)
        | isNormal (a:/b) = (0, a:/b)
        | otherwise = let
            (q, b') = second (+ minBound) $ b `divMod` depth
            (o, a') = normalize (a + fromIntegral q)
            in (o, a':/b')

instance Varposable a b => Normalize (a:\b) where
    isNormal (a:\b) = isNormal a && isInRange a b
    normalize (a:\b)
        | not (isNormal a) = let
            (o, a') = normalize a
            (p, ab) = normalize $ a' :\ b
            in (o+p, ab)
        | isInRange a b = (0, a:\b)
        | b > end a = let
            a' = succ a
            adjusted = b + start a' - start a
            delta = fromInteger $ count a
            in normalize $ a' :\ (adjusted - delta)
        | otherwise = let
            a' = pred a
            adjusted = b + start a' - start a
            delta = fromInteger $ count a'
            in normalize $ a' :\ (adjusted + delta)


-- | Numerical operations may cause values to become non-normalized.
-- | Normalizing at every step would be inefficient. Remember to normalize
-- | whenever you need your data in a normal form!
instance (Integral a, Integral b, Bounded b) => Num (a:/b) where
    (a:/b) + (x:/y) = (a+x):/(b+y)
    (a:/b) - (x:/y) = (a-x):/(b-y)
    (a:/b) * (x:/y) = (a*x):/(b*y)
    abs (a:/b) = (abs a) :/ b
    signum (0:/0) = 0
    signum (a:/_) = if a < 0 then -1 else 1
    fromInteger = fromTuple . (fromInteger *** fromInteger) . (`divMod` x)
        where x = toInteger (depth::b)

instance (Integral a, Integral b, BoundedIn b a) => Num (a:\b) where
    (a:\b) + (x:\y) = (a+x) :\ (b+y)
    (a:\b) - (x:\y) = (a-x) :\ (b-y)
    (a:\b) * (x:\y) = (a*x) :\ (b*y)
    abs (a:\b) = abs a :\ abs b
    signum (0:\0) = 0
    signum (a:\_) = if a < 0 then -1 else 1
    fromInteger = fromTuple . split


-- | The Real instance is only really there to make Integral happy
instance Constposable a b => Real (a:/b) where
    toRational = toRational . toInteger

instance Varposable a b => Real (a:\b) where
    toRational = fromIntegral . toInteger


-- | pred/succ will keep a normal value normal, but won't normalize
-- | a non-normal value. It's also likely a bit faster than (+) or (-).
-- | Use them if you can.
instance Constposable a b => Enum (a:/b) where
    toEnum = fromIntegral
    fromEnum = fromIntegral
    pred (a:/b)
        | b == minBound = (pred a) :/ maxBound
        | otherwise = a :/ (pred b)
    succ (a:/b)
        | b == maxBound = (succ a) :/ minBound
        | otherwise = a :/ (succ b)

instance Varposable a b => Enum (a:\b) where
    toEnum = fromIntegral
    fromEnum = fromIntegral
    succ (a:\b)
        | b == end a = succ a :\ start (succ a)
        | otherwise = a :\ (succ b)
    pred (a:\b)
        | b == start a = pred a :\ end (pred a)
        | otherwise = a :\ (pred b)


-- | The real prize here is `toInteger`.
-- | `quotRem` (and therefore `divMod`) just delegate the work to Integer.
-- | It's not pretty, but it works. Don't expect much efficiency there.
dumbQuotRem :: Integral a => a -> a -> (a, a)
dumbQuotRem a b = from $ (toInteger a) `quotRem` (toInteger b)
    where from = (fromInteger *** fromInteger)

instance Constposable a b => Integral (a:/b) where
    toInteger (a:/b) = (toInteger a * x) + toInteger b
        where x = toInteger (depth :: b)
    quotRem = dumbQuotRem

instance Varposable a b => Integral (a:\b) where
    toInteger = (size <$> left <*> right) . normal
    quotRem = dumbQuotRem


-- | Some quickcheck helpers.
-- | NOTE TO DEVELOPERS: If you're debugging bugs in `Normalize` instances,
-- | you're going to want to comment out the `shrink = normal` definitions.
instance (Constposable a b, Arbitrary a, Arbitrary b) => Arbitrary (a:/b) where
    arbitrary = (:/) <$> arbitrary <*> arbitrary
    shrink = pure . normal

instance (Varposable a b, Arbitrary a, Arbitrary b) => Arbitrary (a:\b) where
    arbitrary = (:\) <$> arbitrary <*> arbitrary
    shrink = pure . normal


-- | Composed types can be formatted numerically.
-- | This allows you to, say, use "%s" to show seconds since time 0.
instance Constposable a b => WriteBlock (a:/b) where
    numerical = show . toInteger

instance Varposable a b => WriteBlock (a:\b) where
    numerical = show . toInteger


-- | TODO: Find a new home for this.
-- | The range within the boundaries of a bounded number
depth :: forall t. (Bounded t, Num t, Enum t) => t
depth = succ (maxBound - minBound)
