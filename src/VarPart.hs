{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module VarPart where
import Control.Applicative
import Format
import FullEnum
import Parse
import Prelude hiding (Enum(..), (!!))
import qualified Prelude
import Range
import TwoTuple
import Zeroed

-- TODO: Zeroed/Ranged combination

data a :/: b = a :/: b deriving (Eq, Ord)

instance (Show a, Show b) => Show (a:/:b) where
    show (a:/:b) = show a ++ "/" ++ show b

instance (Format x a, Format a b) => Format x (a:/:b) where
    display f x (a:/:b) = display f x a ++ display f a b

instance (Parse a, Parse b) => Parse (a:/:b) where
    parse = (:/:) <$> (parse <* slash) <*> parse

instance (Zeroed a, Zeroed b) => Zeroed (a:/:b) where
    zero = zero :/: zero

instance TwoTuple (:/:) where
    toTuple (a:/:b) = (a,b)
    fromTuple (a,b) = a:/:b

instance (Bounded a, Range a b) => Bounded (a:/:b) where
    minBound = minBound :/: start minBound
    maxBound = maxBound :/: end maxBound

instance
    ( Eq b
    , Ord a
    , Ord b
    , Zeroed a
    , Range x a
    , Range a b
    ) => Range x (a:/:b) where
    start x = start x :/: start (start x)
    end x = end x :/: end (end x)

instance
    ( Eq b
    , Ord a
    , Ord b
    , Enum a
    , Zeroed a
    , Range a b
    ) => Enum (a:/:b) where
    fromEnum (a:/:b) = indexOf (a:/:b) $ possibilities a
    toEnum i = possibilities i !! i
    succ (a:/:b)
        | b < end a = a :/: succ b
        | otherwise = succ a :/: start a
    pred (a:/:b)
        | b > start a = a :/: pred b
        | otherwise = pred a :/: end a
    enumFrom x = x : enumFrom (succ x)
    enumFromTo x y
        | x <= y = x : enumFromTo (succ x) y
        | otherwise = []
    -- TODO: We can't safely do enumFromThen* until we're at least a Num.

instance (Ord (a:/:b), Enum (a:/:b)) => Prelude.Enum (a:/:b) where
    fromEnum = fromIntegral . fromEnum
    toEnum = toEnum . toInteger
    succ = succ
    pred = pred
    enumFrom x = x : enumFrom (succ x)
    enumFromTo x y
        | x <= y = x : enumFromTo (succ x) y
        | otherwise = []

elements :: Range x a => x -> [a]
elements = enumFromTo <$> start <*> end

(!!) :: [a] -> Integer -> a
[] !! _ = error "Index too large."
(x:_) !! 0 = x
(_:xs) !! n = xs !! (n-1)

indexOf :: Eq a => a -> [a] -> Integer
indexOf a (x:xs)
    | a == x = 0
    | otherwise = 1 + indexOf a xs
indexOf _ [] = error "So it turns out that this stream isn't infinite."

possibilities ::
    ( Ord i
    , Zeroed i
    , Enum a
    , Zeroed a
    , Range a b
    ) => i -> [a:/:b]
possibilities i = concatMap expand (including i) where
    expand a = (a:/:) <$> elements a

-- Note that zero is always included.
including :: (Zeroed b, Ord b, Zeroed a, Enum a) => b -> [a]
including b
    | b >= zero = enumFrom zero
    | otherwise = enumFromThen zero (pred zero)
