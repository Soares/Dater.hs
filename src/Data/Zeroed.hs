module Data.Zeroed
    ( Zeroed(zero)
    , predecessors
    ) where

-- | Elements that have a default (zero) position.
-- | (Note than zero need not be analagous to 0. Gregorian years begin on
-- | Year 1, for example.) Generally used for data (e.g. Years) that have
-- | a 'starting point' and may then fan out poth in the positive and
-- | negative directions.
class Zeroed a where
    zero :: a

-- | All elements between the given element and the zero element.
-- | The positive direction contains the zero element, the negative
-- | direction does not.
-- | (Ex. predecessors 3 = [0,1,2,3]; predecessors (-2) = [-2, -1]
predecessors :: (Eq a, Ord a, Enum a, Zeroed a) => a -> [a]
predecessors a
    | a == zero = []
    | a > zero = succTo zero (pred a)
    | otherwise = predTo (pred zero) a

-- | The list of all values from one point to another, inclusive
succTo :: (Eq a, Enum a) => a -> a -> [a]
succTo a b
    | a == b = [a]
    | otherwise = a : succTo (succ a) b

-- | The list of all values from one point to backwards to another, inclusive
predTo :: (Eq a, Enum a) => a -> a -> [a]
predTo a b
    | a == b = [a]
    | otherwise = a : predTo (pred a) b

-- | Simple instances
instance Zeroed Integer where zero = 0
instance Zeroed Int where zero = 0
