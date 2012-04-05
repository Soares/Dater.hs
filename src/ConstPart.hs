{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module ConstPart where
import Control.Applicative
import Format
import Parse
import TwoTuple
import Normalize
import Prelude hiding (break)

-- | A simple combinator, inteded for combining types into a time type.
data a ::: b = a ::: b deriving (Eq, Ord)

-- | Some utilities to allow us to treat ::: like a tuple
instance TwoTuple (:::) where
    toTuple (a:::b) = (a,b)
    fromTuple (a,b) = a:::b

-- | The colin is utilized by default when showing time values.
-- | Use a newtype and override if you'd like different semantics.
instance (Show a, Show b) => Show (a:::b) where
    show (a:::b) = show a ++ ":" ++ show b

-- | TODO: This is deprecated.
instance (Format x a, Format a b) => Format x (a:::b) where
    display x (a:::b) c = display x a c ++ display a b (descend c)

-- | The colin is parsed by default when parsing time values.
-- | Use a newtype and override if you'd like different semantics.
instance (Parse a, Parse b) => Parse (a:::b) where
    parse = (:::) <$> (parse <* colin) <*> parse

-- | Allows propagation of bounds.
instance (Bounded a, Bounded b) => Bounded (a:::b) where
    minBound = minBound ::: minBound
    maxBound = maxBound ::: maxBound

-- | Puts the pair into a normalized state, where all parts are
-- | within their respective bounds
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
        | b > maxBound     = normalize $ (succ a) ::: (b - range)
        | b < minBound     = normalize $ (pred a) ::: (b + range)
        | otherwise        = let (o, a') = normalize a in (o, a' ::: b)

-- | Allows normal numerical operations on pairs
instance (Integral a, Integral b, Bounded a, Bounded b) => Num (a:::b) where
    (+) = apply (+)
    (-) = apply (-)
    (*) = apply (*)
    abs = alter abs
    signum = alter signum
    fromInteger = break

-- | Really just necessary so that we can get Integral support
instance (Integral a, Integral b, Bounded a, Bounded b) => Real (a:::b) where
    toRational (a:::b) = toRational $ build (a,b)

-- | Really just necessary so that we can get Integral support
instance (Integral a, Integral b, Bounded a, Bounded b) => Enum (a:::b) where
    toEnum = break . toInteger
    fromEnum = fromIntegral . build

-- | Allows us to use build/break/apply
instance
    ( Integral a
    , Bounded a
    , Integral b
    , Bounded b
    ) => Integral (a:::b) where
    toInteger = build
    quotRem ab xy = let
        d = build xy
        n = build ab
        (q, r) = fromTuple $ n `quotRem` d
        in (fromInteger q, fromInteger r)

-- | The range within the buondaries of a bounded number
-- | TODO: Find a better place for this
range :: forall t. (Bounded t, Num t, Enum t) => t
range = succ (maxBound - minBound)

-- | TODO Note: These could all be moved to two-tuple?

-- | Map a two-tuple into an Integer. One-to-one mapping.
build :: forall t x y. (Integral x, Integral y, Bounded y, TwoTuple t)
    => t x y -> Integer
build t = (x * sizey) + y where
    x = toInteger $ left t
    y = toInteger $ right t
    sizey = 1 + toInteger (maxBound - minBound :: y)

-- | Map an integer into a two-tuple. One-to-one mapping.
break :: forall t x y. (Integral x, Integral y, Bounded y, TwoTuple t)
    => Integer -> t x y
break = integers . flip quotRem sizey where
    integers = tmap fromInteger fromInteger . fromTuple
    sizey = 1 + fromIntegral (maxBound - minBound :: y)

-- | Apply an Integer function to an element that can be broken down and rebuilt
alter :: (Integral x, Integral y, Bounded x, Bounded y, TwoTuple t)
    => (Integer -> Integer) -> t x y -> t x y
alter f = break . f . build

-- | Like alter, but works on a function of two integer arguments
apply :: (Integral x, Integral y, Bounded x, Bounded y, TwoTuple t)
    => (Integer -> Integer -> Integer) -> t x y -> t x y -> t x y
apply f x y = break $ f (build x) (build y)
