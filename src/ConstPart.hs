{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module ConstPart where
import Control.Applicative
import Format
import Gen
import Parse
import TwoTuple
import Normalize
import Prelude hiding (break)

data a ::: b = a ::: b deriving (Eq, Ord)

instance (Show a, Show b) => Show (a:::b) where
    show (a:::b) = show a ++ ":" ++ show b

instance (Format x a, Format a b) => Format x (a:::b) where
    display x (a:::b) c = display x a c ++ display a b (descend c)

instance (Parse a, Parse b) => Parse (a:::b) where
    parse = (:::) <$> (parse <* colin) <*> parse

instance TwoTuple (:::) where
    toTuple (a:::b) = (a,b)
    fromTuple (a,b) = a:::b

instance (Bounded a, Bounded b) => Bounded (a:::b) where
    minBound = minBound ::: minBound
    maxBound = maxBound ::: maxBound

instance
    ( Integral a
    , Bounded a
    , Integral b
    , Bounded b
    ) => Normalize (a:::b) where
    isOver (a:::_) = a > maxBound
    isUnder (a:::_) = a < minBound
    isNormal (a:::b) = inRange a && inRange b
        where inRange x = x >= minBound && x <= maxBound
    normalize (a:::b)
        | isNormal (a:::b) = a:::b
        | a > maxBound = normalize ((a - range) ::: b)
        | a < minBound = normalize ((a + range) ::: b)
        | b > maxBound = normalize (succ a ::: (b - range))
        | otherwise = normalize (pred a ::: (b + range)) where
            range :: forall a. (Bounded a, Integral a) => a
            range = succ (maxBound - minBound)

instance
    ( Integral a
    , Integral b
    , Bounded a
    , Bounded b
    ) => Num (a:::b) where

    (+) = apply (+)
    (-) = apply (-)
    (*) = apply (*)
    abs = alter abs
    signum = alter signum
    fromInteger = break

instance
    ( Integral a
    , Integral b
    , Bounded a
    , Bounded b
    ) => Real (a:::b) where

    toRational (a:::b) = toRational $ build (a,b)

instance
    ( Integral a
    , Integral b
    , Bounded a
    , Bounded b
    ) => Enum (a:::b) where

    toEnum = break . toInteger
    fromEnum = fromIntegral . build

instance
    ( Integral a
    , Integral b
    , Bounded a
    , Bounded b
    ) => Integral (a:::b) where

    toInteger = build
    quotRem ab xy = let
        d = build xy
        n = build ab
        (q, r) = fromTuple $ n `quotRem` d
        in (fromInteger q, fromInteger r)

build :: forall t x y.
    ( Integral x
    , Integral y
    , Bounded y
    , TwoTuple t
    ) => t x y -> Integer
build t = (x * sizey) + y where
    x = fromIntegral $ left t
    y = fromIntegral $ right t
    sizey = 1 + fromIntegral (maxBound - minBound :: y)

break :: forall t x y.
    ( Integral x
    , Integral y
    , Bounded y
    , TwoTuple t
    ) => Integer -> t x y
break = integers . flip quotRem sizey where
    integers = tmap fromInteger fromInteger . fromTuple
    sizey = 1 + fromIntegral (maxBound - minBound :: y)

alter :: (Integral x, Integral y, Bounded x, Bounded y, TwoTuple t) =>
    (Integer -> Integer) -> t x y -> t x y
alter f = break . f . build

apply ::
    ( Integral x
    , Integral y
    , Bounded x
    , Bounded y
    , TwoTuple t
    ) => (Integer -> Integer -> Integer) -> t x y -> t x y -> t x y
apply f x y = break $ f (build x) (build y)
