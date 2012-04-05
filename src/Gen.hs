module Gen
    ( Gen(next, prev)
    , nexts
    , nextTo
    , prevs
    , prevTo
    ) where

-- | A slimmed down version of 'Enum' that doesn't require
-- | all those strong ties to Integers.
-- | Useful when you only ever want the entire list of something.
class Gen a where
    next :: a -> a
    prev :: a -> a


-- | The infinite list of values generated from a point
nexts :: Gen a => a -> [a]
nexts a = a : nexts (next a)

-- | The list of all values from one point to another, inclusive
nextTo :: (Eq a, Gen a) => a -> a -> [a]
nextTo a b
    | a == b = [a]
    | otherwise = a : nextTo (next a) b

-- | The infinite list of values generated before a point
prevs :: Gen a => a -> [a]
prevs a = a : prevs (prev a)

-- | The list of all values from one point to backwards to another, inclusive
prevTo :: (Eq a, Gen a) => a -> a -> [a]
prevTo a b
    | a == b = [a]
    | otherwise = a : prevTo (prev a) b


-- | Simple instances

instance Gen Integer where
    next = succ
    prev = pred

instance Gen Int where
    next = succ
    prev = pred
