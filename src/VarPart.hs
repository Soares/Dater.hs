{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module VarPart where
import Control.Applicative
import Control.Arrow
import Format
import Parse
import Gen
import Quotiented
import Range
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


size :: (Zeroed a, Integral b, Ranged b a) => a -> b -> Integer
size a b = (intify a b) + (sum $ map count $ predecessors a)

split :: (Zeroed a, Integral b, Ranged b a) => Integer -> (a, Integer)
split n = choose $ zip3 as (0:sizes) sizes where
    choose ((a, b, c):ds) = if enough c then (a, leftover b c) else choose ds
    enough c = if n >= 0 then c > n else c >= (-n)
    leftover b c = if n >= 0 then n-b else c+n
    sizes = cascade $ map count as
    as = nexts zero

intify :: forall a b. (Ranged b a, Integral b) => a -> b -> Integer
intify a b = fromIntegral b - fromIntegral (start a :: b)

elemify :: forall a b. (Ranged b a, Integral b) => a -> Integer -> b
elemify a i = fromIntegral (i + fromIntegral (start a :: b))

encode :: (Zeroed a, Integral b, Ranged b a) => (a:/:b) -> Integer
encode (a:/:b) = size a b

decode :: (Zeroed a, Integral b, Ranged b a) => Integer -> (a:/:b)
decode n = let
    (a, i) = split n
    b = elemify a i
    in a :/: b

cascade :: Num a => [a] -> [a]
cascade (x:xs) = x : map (+x) (cascade xs)
cascade [] = []
