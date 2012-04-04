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
import FullEnum
import Quotiented
import Range
import Sized
import TwoTuple
import Zeroed

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

instance (Ord a, Ord b, Sized a, SizedIn b a, Ranged b a) => Sized (a:/:b) where
    size (a:/:b) = sa + sb where
        sa = sum $ map size $ predecessors a
        sb = sum $ map (sizeIn a) $ filter (< b) (elements a)

instance (Gen a, Ord b, Ranged b a) => Gen (a:/:b) where
    next (a:/:b)
        | b < end a = a :/: succ b
        | otherwise = next a :/: start a
    prev (a:/:b)
        | b > start a = a :/: pred b
        | otherwise = prev a :/: end a

instance (Ord a, VQR a, VQRIn b a, Integral b, Ranged b a, SizedIn b a) => VQR (a:/:b) where
    vqr n = let
        (a, i) = vqr n
        (b, j) = vqrIn a i
        in (a:/:b, j)


encode :: forall a b. (Sized a, Ranged b a, Integral b)
    => (a:/:b) -> Integer
encode (a:/:b) = size a + b' where
    b' = fromIntegral b - fromIntegral (start a :: b)


decode :: forall a b. (VQR a, Sized a, Ranged b a, Integral b)
    => Integer -> (a:/:b)
decode n = let
    (ym, i) = vqr n
    d = fromIntegral (i + fromIntegral (start ym :: b))
    in ym :/: d
