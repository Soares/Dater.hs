{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module VarPart where
import Control.Applicative
import Normalize
import Format
import Parse
import Gen
import Range
import Zeroed
import TwoTuple

data a :/: b = a :/: b deriving (Eq, Ord)

instance (Show a, Show b) => Show (a:/:b) where
    show (a:/:b) = show a ++ "/" ++ show b

instance (Format x a, Format a b) => Format x (a:/:b) where
    display x (a:/:b) c = display x a c ++ display a b (descend c)

instance (Parse a, Parse b) => Parse (a:/:b) where
    parse = (:/:) <$> (parse <* slash) <*> parse

instance TwoTuple (:/:) where
    toTuple (a:/:b) = (a,b)
    fromTuple (a,b) = a:/:b

instance (Zeroed a, Ranged b a, Ord b) => Zeroed (a:/:b) where
    zero = zero :/: start zero

instance (Gen a, Ord b, Ranged b a) => Gen (a:/:b) where
    next (a:/:b)
        | b < end a = a :/: succ b
        | otherwise = next a :/: start a
    prev (a:/:b)
        | b > start a = a :/: pred b
        | otherwise = prev a :/: end a

instance (Gen a, Normalize a, Ranged b a, Integral b) => Normalize (a:/:b) where
    isNormal (a:/:b) = isNormal a && isInRange a b
    normalize (a:/:b)
        | not (isNormal a) = let
            (o, a') = normalize a
            (p, ab) = normalize $ a' :/: b
            in (o+p, ab)
        | isInRange a b = (0, a:/:b)
        | b > end a = let
            a' = next a
            b' = fromInteger $ toInteger b - count a
            in normalize $ a' :/: b'
        | otherwise = let
            a' = prev a
            b' = fromInteger $ toInteger b + count a'
            in normalize $ a' :/: b'

encode :: (Zeroed a, Integral b, Ranged b a) => (a:/:b) -> Integer
encode (a:/:b) = size a b

decode :: (Zeroed a, Integral b, Ranged b a) => Integer -> (a:/:b)
decode n = let
    (a, i) = split n
    b = elemify a i
    in a :/: b
