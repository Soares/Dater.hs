{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.DateTime.ConstPart ((:::)(..), Composable) where
import Control.Applicative
import Control.Arrow
import Data.Coded
import Data.Normalize
import Data.Pair
import Data.Zeroed
import Test.QuickCheck.Arbitrary
import Text.Format.Write

-- | A simple combinator, inteded for combining types into a time type.
data a ::: b = a ::: b

type Composable a b =
    ( Bounded b
    , Integral a
    , Integral b
    , Normalize a
    )

-- | Equality is checked post-normalization
instance Composable a b => Eq (a:::b) where
    x == y = let
        (a:::b) = normal x
        (c:::d) = normal y
        in a == c && b == d

instance (Zeroed a, Bounded b) => Zeroed (a:::b) where
    zero = zero ::: minBound

-- | Ordering is determined post-normalization
instance Composable a b => Ord (a:::b) where
    x <= y = let
        (a:::b) = normal x
        (c:::d) = normal y
        in if a == c then b <= d else a < c

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
instance Composable a b => Normalize (a:::b) where
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
    signum (a:::_) = signum a:::(minBound + 0)
    fromInteger = decode

-- | Really just necessary so that we can get Integral support
instance Composable a b => Real (a:::b) where
    toRational = toRational . encode

-- | Really just necessary so that we can get Integral support
instance Composable a b => Enum (a:::b) where
    toEnum = decode . toInteger
    fromEnum = fromIntegral . encode

-- | Really we just want the toInteger function.
-- | The quotRem implementation is quite contrived.
instance Composable a b => Integral (a:::b) where
    toInteger = encode
    quotRem ab xy = let
        n = encode ab
        d = encode xy
        (q, r) = fromTuple $ n `quotRem` d
        in (fromInteger q, fromInteger r)

-- | Allows QuickCheck testing
instance (Arbitrary a, Arbitrary b) => Arbitrary (a:::b) where
    arbitrary = (:::) <$> arbitrary <*> arbitrary
    shrink (a:::b) = [ x:::b | x <- shrink a ] ++ [ a:::y | y <- shrink b ]

-- | Allows us to encode and decode the time
-- | TODO: remove dependency
instance (Integral a, Integral b, Bounded b) => Coded (a:::b) where
    encode (a:::b) = (toInteger a * size) + toInteger b
        where size = toInteger (range :: b)
    decode = fromTuple . (fromInteger *** fromInteger) . (`quotRem` size)
        where size = toInteger (range :: b)

-- | When asked to be formatted, give the number of seconds since 0
instance (Integral a, Integral b, Bounded b) => WriteBlock (a:::b) where
    numerical = show . encode

-- | The range within the boundaries of a bounded number
range :: forall t. (Bounded t, Num t, Enum t) => t
range = succ (maxBound - minBound)
