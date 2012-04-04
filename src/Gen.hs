module Gen
    ( Gen(next, prev)
    , nexts
    , nextTo
    , prevs
    , prevTo
    ) where

class Gen a where
    next :: a -> a
    prev :: a -> a


nexts :: Gen a => a -> [a]
nexts a = a : nexts (next a)


nextTo :: (Eq a, Gen a) => a -> a -> [a]
nextTo a b
    | a == b = [a]
    | otherwise = a : nextTo (next a) b


prevs :: Gen a => a -> [a]
prevs a = a : prevs (prev a)


prevTo :: (Eq a, Gen a) => a -> a -> [a]
prevTo a b
    | a == b = [a]
    | otherwise = a : prevTo (prev a) b


-- Simple instances

instance Gen Integer where
    next = succ
    prev = pred

instance Gen Int where
    next = succ
    prev = pred
