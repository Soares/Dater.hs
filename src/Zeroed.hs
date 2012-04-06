module Zeroed where -- TODO: (Zeroed(zero) , predecessors) where

-- | Elements that have a default (zero) position.
-- | (Note than zero need not be analagous to 0. Gregorian years begin on
-- | Year 1, for example.) Generally used for data (e.g. Years) that have
-- | a 'starting point' and may then fan out poth in the positive and
-- | negative directions.
class (Ord a, Enum a) => Zeroed a where
    zero :: a

-- | All elements between the given element and the zero element.
-- | The positive direction contains the zero element, the negative
-- | direction does not.
-- | (Ex. predecessors 3 = [0,1,2,3]; predecessors (-2) = [-2, -1]
predecessors :: (Eq a, Ord a, Zeroed a) => a -> [a]
predecessors a
    | a == zero = []
    | a > zero = nextTo zero (pred a)
    | otherwise = prevTo (pred zero) a


-- | Simple instances
instance Zeroed Integer where zero = 0
instance Zeroed Int where zero = 0


-- | TODO: Organize

-- | The infinite list of values generated from a point
nexts :: Enum a => a -> [a]
nexts a = a : nexts (succ a)

-- | The list of all values from one point to another, inclusive
nextTo :: (Eq a, Enum a) => a -> a -> [a]
nextTo a b
    | a == b = [a]
    | otherwise = a : nextTo (succ a) b

-- | The infinite list of values generated before a point
prevs :: Enum a => a -> [a]
prevs a = a : prevs (pred a)

-- | The list of all values from one point to backwards to another, inclusive
prevTo :: (Eq a, Enum a) => a -> a -> [a]
prevTo a b
    | a == b = [a]
    | otherwise = a : prevTo (pred a) b
