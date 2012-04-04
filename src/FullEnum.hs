module FullEnum where

class Gen a where
    next :: a -> a
    prev :: a -> a
instance Gen Integer where
    next = succ
    prev = pred
instance Gen Int where
    next = succ
    prev = pred
