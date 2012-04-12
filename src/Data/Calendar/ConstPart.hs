{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Calendar.ConstPart ((:::)(..)) where
import Control.Applicative
import Control.Arrow ((***))
import Data.Normalize
import Data.Pair
import Test.QuickCheck.Arbitrary
import Text.Format.Write

-- TODO: All quotRem should be divMod

-- | A simple combinator, inteded for combining types into a time type.
data a ::: b = a ::: b

instance (Normalize a, Integral a, Integral b, Bounded b) => Eq (a:::b) where
    x == y = let
        (a:::b) = normal x
        (c:::d) = normal y
        in a==c && b==d

instance (Normalize a, Integral a, Integral b, Bounded b) => Ord (a:::b) where
    x <= y = let
        (a:::b) = normal x
        (c:::d) = normal y
        in if a == c then b <= d else a <= c

-- | Some utilities to allow us to treat ::: like a tuple
instance Pair (:::) where
    toTuple (a:::b) = (a,b)
    fromTuple (a,b) = a:::b

-- | The colin is utilized by default when showing time values.
-- | Use a newtype and override if you'd like different semantics.
-- | Note that you can easily change the formatting with a formatter
-- | without resorting to newtypes.
instance (Show a, Show b) => Show (a:::b) where
    show (a:::b) = show a ++ ":" ++ show b

-- | Allows propagation of bounds.
instance (Bounded a, Bounded b) => Bounded (a:::b) where
    minBound = minBound ::: minBound
    maxBound = maxBound ::: maxBound

-- | Normalizes the time, such that all parts are within their respective
-- | boundaries. Normalization is NOT indempotent, as there may be overflow.
instance
    ( Normalize a, Integral a, Integral b, Bounded b
    ) => Normalize (a:::b) where
    isNormal (a:::b) = isNormal a && b >= minBound && b <= maxBound
    normalize (a:::b)
        | isNormal (a:::b) = (0, a:::b)
        | b > maxBound     = normalize $ succ a ::: (b - range)
        | b < minBound     = normalize $ pred a ::: (b + range)
        | otherwise        = let (o, a') = normalize a in (o, a' ::: b)

-- | Allows normal numerical operations on pairs
-- | This may cause the time to be non-normalized
instance (Integral a, Integral b, Bounded b) => Num (a:::b) where
    (a:::b) + (x:::y) = (a+x):::(b+y)
    (a:::b) - (x:::y) = (a-x):::(b-y)
    (a:::b) * (x:::y) = (a*x):::(b*y)
    abs (a:::b) = abs a:::b
    signum (0:::0) = 0
    signum (a:::_) = if a < 0 then -1 else 1
    fromInteger = fromTuple . (fromInteger *** fromInteger) . (`divMod` size)
        where size = toInteger (range::b)

-- | Really just necessary so that we can get Integral support
instance (Normalize a, Integral a, Integral b, Bounded b) => Real (a:::b) where
    toRational = toRational . toInteger

-- | Really just necessary so that we can get Integral support
instance (Normalize a, Integral a, Integral b, Bounded b) => Enum (a:::b) where
    toEnum = fromIntegral
    fromEnum = fromIntegral
    pred (a:::b)
        | b > minBound = a ::: (pred b)
        | otherwise = (pred a) ::: maxBound
    succ (a:::b)
        | b < maxBound = a ::: succ b
        | otherwise = (succ a) ::: minBound

-- | Really we just want the toInteger function.
-- | The quotRem implementation is quite contrived.
instance (Normalize a, Integral a, Integral b, Bounded b) => Integral (a:::b) where
    toInteger (a:::b) = (toInteger a * size) + toInteger b
        where size = toInteger (range :: b)
    quotRem a x = (fromInteger *** fromInteger) $
        (toInteger a) `quotRem` (toInteger x)

-- | Allows QuickCheck testing
instance (Arbitrary a, Arbitrary b) => Arbitrary (a:::b) where
    arbitrary = (:::) <$> arbitrary <*> arbitrary
    shrink (a:::b) = [ x:::b | x <- shrink a ] ++ [ a:::y | y <- shrink b ]

-- | When asked to be formatted, give the number of seconds since 0
instance (Normalize a, Integral a, Integral b, Bounded b) => WriteBlock (a:::b) where
    numerical = show . toInteger

-- | The range within the boundaries of a bounded number
range :: forall t. (Bounded t, Num t, Enum t) => t
range = succ (maxBound - minBound)
