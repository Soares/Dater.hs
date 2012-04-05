module Zeroed
    ( Zeroed(zero)
    , predecessors
    ) where
import Gen

-- | Elements that have a default (zero) position.
-- | (Note than zero need not be analagous to 0. Gregorian years begin on
-- | Year 1, for example.) Generally used for data (e.g. Years) that have
-- | a 'starting point' and may then fan out poth in the positive and
-- | negative directions.
class (Ord a, Gen a) => Zeroed a where
    zero :: a

-- | All elements between the given element and the zero element.
-- | The positive direction contains the zero element, the negative
-- | direction does not.
-- | (Ex. predecessors 3 = [0,1,2,3]; predecessors (-2) = [-2, -1]
predecessors :: (Eq a, Ord a, Zeroed a) => a -> [a]
predecessors a
    | a == zero = []
    | a > zero = nextTo zero (prev a)
    | otherwise = prevTo (prev zero) a


-- | Simple instances

instance Zeroed Integer where zero = 0
instance Zeroed Int where zero = 0
