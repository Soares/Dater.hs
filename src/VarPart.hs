{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module VarPart where
import Control.Applicative
import Format
import Parse
import TwoTuple

data a :/: b = a :/: b deriving (Eq, Ord)

class Enum a => Range x a | a -> x where
    start :: x -> a
    end :: x -> a

instance (Show a, Show b) => Show (a:/:b) where
    show (a:/:b) = show a ++ "/" ++ show b

instance (Format x a, Format a b) => Format x (a:/:b) where
    display f x (a:/:b) = display f x a ++ display f a b

instance (Parse a, Parse b) => Parse (a:/:b) where
    parse = (:/:) <$> (parse <* slash) <*> parse

instance TwoTuple (:/:) where
    toTuple (a:/:b) = (a,b)
    fromTuple (a,b) = a:/:b

instance (Bounded a, Range a b) => Bounded (a:/:b) where
    minBound = minBound :/: start minBound
    maxBound = maxBound :/: end maxBound

instance (Eq b, Num a, Ord a, Zeroed a, Range x a, Range a b) => Range x (a:/:b) where
    start x = start x :/: start (start x)
    end x = end x :/: end (end x)

instance (Eq b, Num a, Ord a, Zeroed a, Enum a, Range a b) => Enum (a:/:b) where
    toEnum i = possibilities i !! i
    fromEnum (a:/:b) = fromIntegral . indexOf (a:/:b) $ possibilities a

elements :: Range x a => x -> [a]
elements x = [start x .. end x]

indexOf :: Eq a => a -> [a] -> Integer
indexOf a (x:xs)
    | a == x = 0
    | otherwise = 1 + indexOf a xs
indexOf _ [] = error "You said this stream was infinite!"

possibilities :: (Ord i, Num i, Zeroed a, Enum a, Range a b) => i -> [a:/:b]
possibilities i = concatMap expand (including i) where
    expand a = (a:/:) <$> elements a

-- Note that zero is always included.
including :: (Ord b, Num b, Zeroed a, Enum a) => b -> [a]
including b
    | b >= 0 = [zero ..]
    | otherwise = [zero, pred zero..]
