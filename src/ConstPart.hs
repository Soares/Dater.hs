{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
module ConstPart ((:::)(..)) where
import Coded
import Control.Arrow
import Normalize
import Pair

-- | A simple combinator, inteded for combining types into a time type.
data a ::: b = a ::: b deriving (Eq, Ord)

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
    ( Integral a
    , Bounded a
    , Normalize a
    , Integral b
    , Bounded b
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
    signum (a:::_) = signum a:::(minBound + 0)
    fromInteger = decode

-- | Really just necessary so that we can get Integral support
instance (Integral a, Integral b, Bounded b) => Real (a:::b) where
    toRational = toRational . encode

-- | Really just necessary so that we can get Integral support
instance (Integral a, Integral b, Bounded b) => Enum (a:::b) where
    toEnum = decode . toInteger
    fromEnum = fromIntegral . encode

-- | Really we just want the toInteger function.
-- | The quotRem implementation is quite contrived.
instance (Integral a, Integral b, Bounded b) => Integral (a:::b) where
    toInteger = encode
    quotRem ab xy = let
        n = encode ab
        d = encode xy
        (q, r) = fromTuple $ n `quotRem` d
        in (fromInteger q, fromInteger r)

-- | Allows us to encode and decode the time
instance (Integral a, Integral b, Bounded b) => Coded (a:::b) where
    encode (a:::b) = (toInteger a * size) + toInteger b
        where size = toInteger (range :: b)
    decode = fromTuple . (fromInteger *** fromInteger) . (`quotRem` size)
        where size = toInteger (range :: b)

-- | The range within the boundaries of a bounded number
range :: forall t. (Bounded t, Num t, Enum t) => t
range = succ (maxBound - minBound)
